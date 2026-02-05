# Cleaning data
library(tidyverse)
library(sf) # to make a map of locations of study plots
library(climateR) # to download climate data
library(elevatr) # to download elevation data

#####
# Set values for which data to extract - options in data_README.txt
#####
eco_region        <- c("EASTERN TEMPERATE FORESTS", "NORTHERN FORESTS")
data_source       <- "NPS" 
include_elevation <- FALSE # !!!getting elevation data is slow!!!

#####
# Load datasets
#####
plot_data <- read_csv("./data/SPCIS_plots.csv")


#####
# subset plot data
#####
plot_data <- plot_data |>
  # subset region and data source
  subset(EcoRegionLevelI %in% eco_region) |>
  subset(Dataset %in% data_source) |>  
  # keep most recent observation
  group_by(Plot) |>
  filter(Year == max(Year)) |> 
  ungroup() |>
  # add variables to indicate whether coordinates for a plot are unique 
  # these data are harder to incorporate into the framework
  mutate(coords = paste0(Long, " ", Lat)) |>
  group_by(coords) |>
  mutate(coord_counts = n())
  

###### The following bit of code inspects the plots with duplicate coords. 
plot_data_repeated <- plot_data |>
  filter(coord_counts > 1)
# dim(plot_data)
# [1] 730  17
hist(plot_data_repeated$coord_counts, breaks = 1:30)
abline(h = c(11, 27), col = 2)
# It looks like there is one set of plots that has 27 ?subplots?, one set that 
#     11 subplots, and then several sets with 2, 3, and 4 subplots. 
plot_data_repeated_unique_years <- plot_data_repeated |>
  group_by(coords, Year) |>
  mutate(unique_year = n()) |>
  mutate(diff_years = coord_counts == unique_year) |>
  filter(diff_years == FALSE) |> 
  arrange(coords)
# One caution is that some of the doubles are not measured in the same year... 
#     It seems like there are several reasons why there might be duplicate 
#     coordinates. What to do about that? Just remove them all? 

plot_data <- plot_data |>
  filter(coord_counts == 1) |>
  ungroup() |>
  select(-c(coords, coord_counts,          # not needed any more
            Original.Site, Original.Plot,  # duplicate info
            Zone, FuzzedCoord, Resampled)) # same value for every row

###Prep for pulling climate data###
plot_sf <- st_as_sf(plot_data, coords=c("Long","Lat"))
st_crs(plot_sf) <- st_crs(4326)
plot_sf <- cbind(plot_sf, as.data.frame(st_coordinates(plot_sf)))

# ########
# # TW - map the coordinates: 
# usa_states <- st_as_sf(maps::map("state", fill=TRUE, plot=FALSE))
# ggplot() + # State lines
#   geom_sf(data = usa_states, fill = NA, color = "gray20", size = 0.1) + 
#   geom_sf(data = plot_sf, alpha = 0.35) + # plot data on top
#   coord_sf(xlim = c(-95, -65)) + # limit to states we care about
#   theme_bw() +
#   labs(title = "Plot locations")
# ########


#### Download and process TerraClim normals###
## Source: TerraClimate (https://www.climatologylab.org/terraclimate.html)
## 30-year climate normals (1981-2010)
## Citation: Abatzoglou et al., (2018) https://doi.org/10.1038/sdata.2017.191

# set proj_lib to the terra package
plib <- Sys.getenv("PROJ_LIB")
prj <- system.file("proj", package = "terra")[1]
Sys.setenv("PROJ_LIB" = prj)

period <- "19812010"
climate_vars <- c("tmax","tmin","soil","ppt","pet","aet","def","vpd")
X <- data.frame(Plot = plot_sf$Plot)
for (i in 1:length(climate_vars)){
  climate_data <- getTerraClimNormals(plot_sf, 
                                      climate_vars[i], period, 1:12)[[1]]
  climate_data <- terra::extract(climate_data, plot_sf, ID=FALSE)
    # !!!!Currently have a WARNING in the extract function b/c no spatVector data
  
  if (climate_vars[i] == "tmax"){ # map monthly data to annual values. 
    X[[climate_vars[i]]] <- apply(climate_data[,5:9], 1, max)}
  else if (climate_vars[i] == "tmin"){
    X[[climate_vars[i]]] <- apply(climate_data[,5:9], 1, min)}
  else if (climate_vars[i] %in% c("vpd")){
    X[[climate_vars[i]]] <- apply(climate_data[,5:9], 1, mean)}
  else if (climate_vars[i] %in% c("ppt","pet","aet","def","soil")){
    X[[climate_vars[i]]] <- apply(climate_data[,5:9], 1, sum)}
  else {
    print("~~~ ERROR: variable not in climate_vars ~~~")
  }
}
# reset proj_lib
Sys.setenv("PROJ_LIB" = plib)

if(include_elevation == TRUE){
  # elevation (Source: https://rpubs.com/ials2un/elevationdata; Amazon Web Services Terrain Tiles)###
  df_elev <- get_elev_raster(plot_sf, src="aws", z=8) #download elevation data
    # !!!! This is very slow !!!!
  elev_data <- terra::extract(df_elev, plot_sf) #extract elevation data at points
  temp <- terra::terrain(df_elev, opt=c("slope", "aspect"), unit='degrees')
  slope_aspect <- terra::extract(temp, plot_sf)
  
  # Merge all predictor variables
  temp <- data.frame(elv=elev_data, slp=slope_aspect[,1], asp=slope_aspect[,2])
  X<-cbind(X, temp)
}


X <- na.omit(X)


