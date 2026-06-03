library(terra)

plot_data <- readRDS("./data/nps_plot_data_as_SpatRaster.rds")
covariate_files <- list.files("../soil_covariates/", pattern = "*.tif$")
covariates      <- data.frame(1:dim(plot_data))
for(i in 1:length(covariate_files)){
  temp_file <- rast(paste0("../soil_covariates/", covariate_files[i]))
  temp_data <- terra::extract(temp_file, plot_data, ID = F)
  covariates <- cbind(covariates, temp_data)
}
covariates <- covariates[,1]
cov_names       <- c(paste0("n_tot_", c(0, 5, 15, 30, 60, 100, 200), "m"), 
                     paste0("pH_", c(0, 5, 15, 30, 60, 100, 200), "m"),
                     paste0("sand_", c(0, 5, 15, 30, 60, 100, 200), "m"))
colnames(covariates) <- cov_names

saveRDS(covariates, "./data/soil_covariates.rds")
