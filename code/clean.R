# Cleaning data
library(tidyverse)
library(sf)


#####
# Load datasets
#####
plot_data <- read_csv("./data/SPCIS_plots.csv")


#####
# subset plot data
#####
plot_data <- plot_data |>
  # subset region and data source
  # abbreviations in data_README.txt
  subset(EcoRegionLevelI %in% 
           c("EASTERN TEMPERATE FORESTS", "NORTHERN FORESTS")) |>
  subset(Dataset %in% "NPS") |>  
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
dim(plot_data)
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
  select(-c(coords, coord_counts, # not needed any more
            Original.Site, Original.Plot, # duplicate info
            Zone, FuzzedCoord, Resampled)) # same value for every row

###Prep for pulling climate data###
plot_sf <- st_as_sf(plot_data, coords=c("Long","Lat"))
st_crs(plot_sf) <- st_crs(4326)

# ########
# # TW - map the coordinates: 
# usa_states <- st_as_sf(maps::map("state", fill=TRUE, plot=FALSE))
# ggplot() +
#   geom_sf(data = usa_states, fill = NA, color = "gray20", size = 0.1) + # State borders
#   geom_sf(data = plot_sf, alpha = 0.35) + # Your data on top
#   coord_sf(xlim = c(-95, -65)) + # Ensures consistent projection - xlim limits to states we care about
#   theme_bw() +
#   labs(title = "Plot locations")
# ########

