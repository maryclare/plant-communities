# data explanations

# Eco Regions
"EASTERN TEMPERATE FORESTS"
"GREAT PLAINS"
"MARINE WEST COAST FOREST"
"MEDITERRANEAN CALIFORNIA"
"NORTH AMERICAN DESERTS"
"NORTHERN FORESTS"
"NORTHWESTERN FORESTED MOUNTAINS" 
"SOUTHERN SEMIARID HIGHLANDS"
"TAIGA"
"TEMPERATE SIERRAS"
"TROPICAL WET FORESTS"
"TUNDRA"
"WATER"
NA

# Data source abbreviations
NEON - NSF National Ecological Observatory Network
FIA - USFS Forest Inventory Analysis
WVNHP - West Virginia Natural Heritage Program
IL_CTAP - Illinois Critical Trends Assessment Program
VNHP - Virginia Natural Heritage Program
NPS - National Parks Service
NWCA - EPA National Wetland Condition Assessment
CVS - either Carolina Vegitation Survey or BLS Current Vegitation Survey (most lat and long are in the carolinas...)

# Elevation resolution key (from https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution)
Ground resolution per zoom in meters at a given latitude:
zoom	0°	      45°	      60°
0	    156543.0	110692.6	78271.5
1	    78271.5	  55346.3	  39135.8
2	    39135.8	  27673.2	  19567.9
3	    19567.9	  13836.6	  9783.9
4	    9783.9  	6918.3	  4892.0
5	    4892.0  	3459.1	  2446.0
6	    2446.0  	1729.6	  1223.0
7	    1223.0  	864.8	    611.5
8	    611.5	    432.4	    305.7
9	    305.7	    216.2	    152.9
10	  152.9	    108.1	    76.4
11	  76.4	    54.0	    38.2
12	  38.2	    27.0	    19.1
13	  19.1	    13.5	    9.6
14	  9.6	      6.8	      4.8
15	 4.8	      3.4	      2.4

# Climate variable abbreviations/definitions
tmax - Max Temperature, average for month, units = C
tmin - Min Temperature, average for month, units = C
soil - Soil Moisture, total column - at end of month, units = mm
ppt - Precipitation, monthly total, units = mm
pet - Potential evapotranspiration, monthly total, units = mm (water movement from soil (evap) and plants (transp) to atmosphere)
aet - Actual Evapotranspiration, monthly total, units = mm
def - Climate Water Deficit, monthly total, units = mm (?pet - aet?)
vpd - Vapor Pressure Deficit, average for month, units = kpa

# plant_types - info from wikipedia
"Forb/herb" - herbaceous flowering plant that is not a graminoid. Especially found in grasslands and understory. Typically eudicots without woody stems. 
"Graminoid" - herbaceous plant with grass-like morphology e.g., grasses, sedges, and rushes
"Lichenous" - ?lichen? a hybrid colony of algae or cyanobacteria living symbiotically with fungus
"Nonvascular" - plants without a vascular system e.g., algae and bryophytes (mosses, liverworts, and hornworts)
"Shrub" - small to medium-sized perennial woody plan with persistent woody stems above the ground. Distinguished from trees by multiple stems and shorter height. 
"Subshrub" - either a small shrub (less than 2m tall) or a perennial that is largely herbaceous with a slightly woody base e.g., lavender and thyme
"Tree" - a perennial plant with an elongated stem or trunk often supporting branches and leaves
"Vine" - any plant with a growth habit of trailing or climbing stems, lianas, or runners
NA


# Data set explanations: 

SPCIS_plots.csv is the dataset with all of the plot metadata

SPCIS_plant_taxa_tw.csv is the dataset with all of the taxa data. It has been edited from the dataset found at 
                        https://figshare.com/articles/dataset/SPCIS_planta_taxa_and_plot_information/19593373 
                        because that dataset has special characters in rows 837234, 859173, and 939603 column 5 
                        this made it so read_csv() deleted those rows. 
                        The dataset is stored in an aws S3 bucket because it is too big for github
                        It can be accessed with: 
                            taxa_data <- aws.s3::s3read_using(read.csv, 
                                object = "s3://plant-communities-taxa/SPCIS_plant_taxa_tw.csv")

nps_herbs_northeast_spOcc_data.rds is the spOccupancy ready dataset that is output when clean.R is run with the following settings: 
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
        
  
        
        