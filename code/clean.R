# Cleaning data
library(tidyverse)
library(sf) # to make a map of locations of study plots
library(ncdf4) # to download climate data
library(elevatr) # to download elevation data
library(data.table) # to change taxa data into long form with dcast()
# library(aws.s3) # (library used once...) to import the SPCIS_plant_taxa data 


#####
# Set values for which data to extract - options in data_README.txt
#####
eco_region         <- c("EASTERN TEMPERATE FORESTS", "NORTHERN FORESTS")
data_source        <- "NPS" 
include_elevation  <- TRUE # !getting elev data is slow! ~ 64s on small region
elevation_res      <- 6     # scale of larger plots ~= avg elev across plots
subset_region      <- TRUE  # if you want fewer plots for analysis
lon_min            <- -75 
lon_max            <- -65
lat_min            <- 41
lat_max            <- 50 # -75, -65, 41, 50 gives 1065 plots across NJ - ME
plant_type         <- "Forb/herb"
percent_occ_cutoff <- 0.05 # species in fewer than this prop of plots removed
# Limit to species that are present in 10 plots (trees) or 5% (others) of sites...
latent_var_cutoff  <- 0.75 # move species more abundant to end of ordering 


#####
# Load datasets
#####
plot_data     <- read_csv("./data/SPCIS_plots.csv")
taxa_data    <- 
  aws.s3::s3read_using(read.csv, 
                       object = "s3://plant-communities-taxa/SPCIS_plant_taxa_tw.csv")


#####
# subset plot data 
#####
plot_data <- plot_data |>
  # subset region and data source
  subset(EcoRegionLevelI %in% eco_region) |>
  subset(Dataset %in% data_source) |>  
  # keep most recent observation
  group_by(Plot) |>
  filter(Year == max(Year)) |> # keep most recent year for each plot
  ungroup() |>
  # add variables to indicate whether coordinates for a plot are unique 
  mutate(coords = paste0(Long, " ", Lat)) |>
  group_by(coords) |>
  filter(Year == max(Year)) |> # keep most recent year for each set of coords
  mutate(coord_counts = n())


###### The following code inspects/handles the plots with duplicate coords.
plot_data_repeated <- plot_data |>
  filter(coord_counts > 1) |>
  group_by(coords) |>
  mutate(same_month = length(unique(Month)) == 1, 
         same_m2 = length(unique(PlotArea.m2)) == 1) |> 
  arrange(coords)
dim(plot_data_repeated)
# [1] 679  19
hist(plot_data_repeated$coord_counts, breaks = 1:30)
abline(h = 27, col = 2)
# It looks like there is one set of plots that has 27 ?subplots?, 
#     and then several sets with 2, 3, and 4 subplots.
# remove the 27 repeats, likely a transect... 
maybe_transect <- plot_data$Plot[which(plot_data$coord_counts == 27)]
plot_data <- filter_out(plot_data, coord_counts == 27)
plot_data_repeated <- filter_out(plot_data_repeated, coord_counts == 27)

# Check to see if these "nested" plots have different species:
plot_data_nested <- filter(plot_data_repeated, !same_m2)
dim(plot_data_nested)
# [1] 164  19
setequal(taxa_data$SpCode[which(taxa_data$Plot == plot_data_nested$Plot[1])], 
         taxa_data$SpCode[which(taxa_data$Plot == plot_data_nested$Plot[2])])
# different species, so we will union the species up to the larger plot
# rename the plots... 
# temp_plot_name <- plot_data_nested$Plot[which.max(plot_data_nested$PlotArea.m2[1:2])]
# taxa_data_sub1$Plot <- temp_plot_name
unique_lat_lons <- unique(plot_data_nested$coords)
plots_kept <- character(length(unique_lat_lons))
for(i in 1:length(unique_lat_lons)){
  temp_data     <- 
    plot_data_nested[which(plot_data_nested$coords == unique_lat_lons[i]), ]
  plots_kept[i] <- temp_data$Plot[which.max(temp_data$PlotArea.m2)]
  taxa_data$Plot[which(taxa_data$Plot %in% temp_data$Plot)] <- plots_kept[i]
}
plots_removed <- setdiff(unique(plot_data_nested$Plot), plots_kept)
plot_data     <- filter_out(plot_data, Plot %in% plots_removed)
plot_data$coord_counts[which(plot_data$Plot %in% plots_kept)] <- 1
rm(plot_data_nested)

plot_data_all_same <- filter(plot_data_repeated, same_month & same_m2)
dim(plot_data_all_same)
# [1] 455  19
# how many repeats in each group? 
unique(plot_data_all_same$coord_counts)
# [1]  2  3  4
# Check to see if taxa_data rows are the same for plots in all_same 
setequal(taxa_data$SpCode[which(taxa_data$Plot == plot_data_all_same$Plot[1])], 
         taxa_data$SpCode[which(taxa_data$Plot == plot_data_all_same$Plot[2])])
    # Not all the same... union the species
unique_lat_lons <- unique(plot_data_all_same$coords)
plots_kept <- character(length(unique_lat_lons))
for(i in 1:length(unique_lat_lons)){
  temp_data     <- 
    plot_data_all_same[which(plot_data_all_same$coords == unique_lat_lons[i]), ]
  plots_kept[i] <- temp_data$Plot[1]
  taxa_data$Plot[which(taxa_data$Plot %in% temp_data$Plot)] <- plots_kept[i]
}
plots_removed <- setdiff(unique(plot_data_all_same$Plot), plots_kept)
plot_data     <- filter_out(plot_data, Plot %in% plots_removed)
plot_data$coord_counts[which(plot_data$Plot %in% plots_kept)] <- 1
rm(plot_data_all_same)


# need to union the taxa for these in analysis
plot_data_dmonths <- filter(plot_data_repeated, !same_month & same_m2) 
dim(plot_data_dmonths)
# [1] 33 19
# These are measuring across months to see flowering plants that are only 
#      ID-able in that month, just do an indicator of existence in any of them
unique_lat_lons <- unique(plot_data_dmonths$coords)
plots_kept <- character(length(unique_lat_lons))
for(i in 1:length(unique_lat_lons)){
  temp_data     <- 
    plot_data_dmonths[which(plot_data_dmonths$coords == unique_lat_lons[i]), ]
  plots_kept[i] <- temp_data$Plot[1]
  taxa_data$Plot[which(taxa_data$Plot %in% temp_data$Plot)] <- plots_kept[i]
}
plots_removed <- setdiff(unique(plot_data_dmonths$Plot), plots_kept)
plot_data     <- filter_out(plot_data, Plot %in% plots_removed)
plot_data$coord_counts[which(plot_data$Plot %in% plots_kept)] <- 1
rm(plot_data_dmonths, plot_data_repeated, temp_data, i, plots_kept, 
   plots_removed, unique_lat_lons)

# did we handle all of the repeats? 
unique(plot_data$coord_counts)
# [1] 1
#####


plot_data <- plot_data |>
  ungroup() |>
  select(-c(coords, coord_counts,          # not needed any more
            Original.Site, Original.Plot,  # duplicate info
            Zone, FuzzedCoord, Resampled)) |>  # same value for every row
  column_to_rownames(var = "Plot")

if(subset_region == TRUE){
  plot_data <- filter(plot_data, Long > lon_min & Long < lon_max)
  plot_data <- filter(plot_data, Lat > lat_min & Lat < lat_max)
}

#####
# Get coordinate data into format for spOccupancy
#####
plot_coords <- select(plot_data, c(Long, Lat)) |> 
  st_as_sf(coords = c("Long","Lat"))
st_crs(plot_coords) <- st_crs(4326)
plot_coords <- st_transform(plot_coords, crs = "ESRI:102005") |> 
  st_coordinates() |> 
  as.data.frame()
rownames(plot_coords) <- rownames(plot_data)




#####
# Prepare taxa data
#####
taxa_metadata <- taxa_data |> 
  filter(Plot %in% rownames(plot_data)) |> # subset to our plots chosen above
  filter(grepl(plant_type, GrowthHabit)) |> # subset to one plant type
  mutate(occupied = ifelse(PctCov > 0, 1, 0)) |> 
  select(Plot, Year, SpCode, AcceptedTaxonName, # keep necessary columns
         PctCov, NativeStatus, occupied) 

# change into data matrix with rows as Plots and columns as Species
taxa_data <- as.data.table(taxa_metadata) |> 
        # fun.aggregate=mean handles the repeated taxa from "unioning" above
  dcast(Plot ~ SpCode, value.var = "occupied", fill = 0, fun.aggregate=mean) |> 
  column_to_rownames("Plot")

# remove species that are in fewer than percent_occ_cutoff of the plots
#    and reorder so that latent variables have a chance 
prop_occupied   <- sort(colSums(taxa_data) / NROW(taxa_data), decreasing = TRUE)
too_common_occ  <- names(which(prop_occupied >= latent_var_cutoff))
sp_to_keep      <- names(which(prop_occupied > percent_occ_cutoff & 
                                 prop_occupied < latent_var_cutoff))
taxa_data       <- select(taxa_data, 
                          c(all_of(sp_to_keep), all_of(too_common_occ)))


# ???? Do we want to add sites back into the taxa_data when none of our species
#    were observed there? These are meaningful zeros in some sense... 
missing_rows      <- setdiff(rownames(plot_data), rownames(taxa_data))
temp_df           <- matrix(0, nrow = length(missing_rows), 
                            ncol = dim(taxa_data)[2])
colnames(temp_df) <- colnames(taxa_data)
rownames(temp_df) <- missing_rows
taxa_data         <- rbind(taxa_data, temp_df)



#####
# Get climate/elevation covariates
#####
###Prep for pulling climate data###
plot_data         <- st_as_sf(plot_data, coords=c("Long","Lat"))
st_crs(plot_data) <- st_crs(4326)
plot_data         <- cbind(plot_data, as.data.frame(st_coordinates(plot_data)))

# ########
# # TW - map the coordinates:
# lon_range <- c(min(plot_data$Long), max(plot_data$Long))
# lat_range <- c(min(plot_data$Lat), max(plot_data$Lat))
# usa_states <- st_as_sf(maps::map("state", fill=TRUE, plot=FALSE))
# ggplot() + # State lines
#   geom_sf(data = usa_states, fill = NA, color = "gray20", size = 0.1) +
#   geom_sf(data = plot_data, alpha = 0.35) + # plot data on top
#   coord_sf(xlim = lon_range, ylim = lat_range) + # limit to states we care about
#   theme_bw() +
#   labs(title = "Plot locations")
# ########


#### Download and process TerraClim normals###
## Source: TerraClimate (https://www.climatologylab.org/terraclimate.html)
## compute 30-year climate normals (1981-2010) from monthly aggregated data
## Note that these data have approx 4km (1/24 degree) spatial resolution
## Citation: Abatzoglou et al., (2018) https://doi.org/10.1038/sdata.2017.191

climate_vars <- c("tmax","tmin","soil","ppt","pet","aet","def","vpd")
plot_covariates <- data.frame(Plot = rownames(plot_data))
# enter in variable you want to download see: http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html
for(var in climate_vars){
  baseurlagg <- paste0(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",var),"_1958_CurrentYear_GLOBE.nc")
  nc <- nc_open(baseurlagg)
  
  # netCDF is read on grid, need min index (start) and how far to read (count) 
  lon_nc  <- ncvar_get(nc, "lon")
  lonmin  <- which.min(abs(lon_nc - min(plot_data$X)))
  lonmax  <- which.min(abs(lon_nc - max(plot_data$X)))
  # lat is indexed backwards: from 90 to -90
  lat_nc  <- ncvar_get(nc, "lat") 
  latmin  <- which.min(abs(lat_nc - max(plot_data$Y))) 
  latmax  <- which.min(abs(lat_nc - min(plot_data$Y)))
  # only want aggregated monthly records from 1981-2010, index 1 is 01/01/1950
  mintime <- 12 * (1981 - 1950) + 1 
  start   <- c(lonmin, latmin, mintime) 
  # keep 360 months - 30 years - 12/01/2010
  count   <- c(length(lonmax:lonmin), length(latmax:latmin), 360) 
  
  # only need some of the points in the grid
  lat_inds <- sapply(plot_data$Y, function(x)
  {which.min(abs(lat_nc - x))}) - latmin + 1
  lon_inds <- sapply(plot_data$X, function(x)
  {which.min(abs(lon_nc - x))}) - lonmin + 1
  
  # read in all aggregated monthly records using aggregated files
  data    <- ncvar_get(nc, varid = var, start = start, count)[lon_inds, lat_inds, ]
  # subset to lon-lat pairs we care about
  data    <- as.numeric(apply(data, 3, diag)) 
  # get date variables
  time    <- ncvar_get(nc, "time", start=c(mintime), count=c(360))
  time    <- as.Date(time, origin="1900-01-01") 
  # close the netCDF file - completes the nc_open operation above
  nc_close(nc)
  # combine info into dataframe
  data    <- data.frame(date = rep(time, each = length(plot_data$X)),
                        year = rep(year(time), each = length(plot_data$X)), 
                        month = rep(month(time), each = length(plot_data$X)), 
                        lon = rep(plot_data$X, 360),
                        lat = rep(plot_data$Y, 360),
                        lon_lat = rep(paste0("(", plot_data$X, ", ", plot_data$Y, ")"), 360), 
                        var = data)
  data <- data |> 
    group_by(lon_lat, month) |> 
    summarize(normals = mean(var), .groups = "drop_last")
  if (var == "tmax"){ # map monthly data to annual values. 
    plot_covariates[[var]] <- summarize(data, normals = max(normals))$normals
  } else if (var == "tmin"){
    plot_covariates[[var]] <- summarize(data, normals = min(normals))$normals
  } else if (var == "vpd"){
    plot_covariates[[var]] <- summarize(data, normals = mean(normals))$normals
  } else if (var %in% c("ppt","pet","aet","def","soil")){
    plot_covariates[[var]] <- summarize(data, normals = sum(normals))$normals
  } else {
    print("~~~ ERROR: variable not in climate_vars ~~~")}
}


# elevation
if(include_elevation == TRUE){
  # elevation (Source: https://rpubs.com/ials2un/elevationdata; 
  #     Amazon Web Services Terrain Tiles)###
  df_elev <-      #download elevation data
    get_elev_raster(plot_data, src="aws", z=elevation_res) 
  # !!!! This is very slow !!!! # 64 seconds with sub-setted region
  elev_data <-   #extract elevation data at points
    terra::extract(df_elev, plot_data) 
  temp <- terra::terrain(df_elev, opt=c("slope", "aspect"), unit='degrees')
  slope_aspect <- terra::extract(temp, plot_data)
  
  # Merge all predictor variables
  temp <- data.frame(elv=elev_data, slp=slope_aspect[,1], asp=slope_aspect[,2])
  plot_covariates<-cbind(plot_covariates, temp)
}

plot_covariates <- plot_covariates |> 
  column_to_rownames(var = "Plot") |> 
  na.omit() 


# All same dimensions? 
dim(taxa_data)
dim(plot_coords)  
dim(plot_covariates)

#####
# Save data for inputting to spOccupancy model
#####
data_list <- list(y = taxa_data, coords = plot_coords, covs = plot_covariates)
saveRDS(data_list, "./data/nps_herbs_northeast_spOcc_data.rds")
