#####
# This script runs the spOccupancy model 
#     requires: dataset (current is nps_herbs_northeast_spOcc_data.rds)


#####
#     !!!!!!!! NOT COMPLETE !!!!!!!!


library(spOccupancy)
library(coda)

# load the plot, taxa, and climate covariates data
data_list <- readRDS("./data/nps_herbs_northeast_spOcc_data.rds")

# settings: 
num_factors     <- 5
cov_model       <- "exponential"
num_species     <- nrow(data_list$y)
batch_length    <- 100
num_batch       <- 100
num_burn        <- 100
num_thin        <- 10 #
num_chains      <- 1
tuning          <- list(phi = 0.5)
# num_omp_threads <- 4
verbose         <- TRUE
num_report      <- 20

# Model formula
jsdm_formula <- ~ scale(tmin) + I(scale(tmin)^2) + scale(ppt) + I(scale(ppt)^2) 

# distance matrix between sites
dist_matrix <- dist(data_list$coords)

# #####
# # Initial values
# #####
# # factor loadings matrix
# lambda_inits <- matrix(0, num_species, num_factors)
# diag(lambda_inits) <- 1
# lambda_inits[lower.tri(lambda_inits)] <- rnorm(sum(lower.tri(lambda_inits)))
# 
# 
# ######get inits for betas, and common means and vars: beta.comm and tau.sq.beta
# response <- data.frame(t(data_list$y))
# temp_df  <- cbind(response, data_list$covs)
# 
# yhat <- temp_df[,1]
# temp <- lm(yhat~scale(tmin) + I(scale(tmin)^2) + scale(ppt) + I(scale(ppt)^2), 
#            data=temp_df)
# betas <- data.frame(temp$coefficients)
# 
# for(i in 2:ncol(response)){
#   yhat <- temp_df[,i]
#   temp <- lm(yhat~scale(tmin) + I(scale(tmin)^2) + scale(ppt) + I(scale(ppt)^2), 
#              data=temp_df)
#   betas <- cbind(betas, temp$coefficients)
# }
# colnames(betas) <- colnames(data_list$y)
# betas <- t(betas)
# betas_mean <- data.frame(Intercept = mean(betas[, 1]),
#                          tmin = mean(betas[, 2]),
#                          tmin.2 = mean(betas[, 3]),
#                          ppt = mean(betas[, 4]),
#                          ppt.2 = mean(betas[, 5])) |> 
#   as.matrix.data.frame()
# betas_var  <- data.frame(Intercept = var(betas[, 1]),
#                          tmin = var(betas[, 2]),
#                          tmin.2 = var(betas[, 3]),
#                          ppt = var(betas[, 4]),
#                          ppt.2 = var(betas[, 5])) |> 
#   as.matrix.data.frame()
# 
# inits      <- list(beta.comm = betas_mean,
#                   beta = betas,
#                   tau.sq.beta = betas_var,
#                   lambda = lambda_inits, 
#                   phi = 3 / mean(dist_matrix))
# 
# 
# 
# # ---------------------------------------------------------------
# 
# 
# #####
# # Priors
# #####
# min_dist <- min(dist_matrix)
# max_dist <- max(dist_matrix)
# 
# priors <- list(beta.comm.normal = list(mean = 0, var = 2.72),
#                tau.sq.beta.ig = list(a = 0.1, b = 0.1),
#                #phi.unif = list(3 /max.dist, 3 / min.dist))
#                #phi.unif = list(c(3/(2.5E06),3/(5E05),3/(5E04),3/(1E03),3/(100)), c(3/(5E05),3/(5E04),3/(1E03), 3/(100),3/(5))))
#                phi.unif = list(c(3/(100),3/(1E03),3/(5E04),3/(5E05),3/(2.5E06)), c(3/(5),3/(100),3/(1E03),3/(5E04),3/(5E05))))
# 
# 
# phi.unif =  list(c(3/(100),3/(1E03),3/(5E04),3/(5E05),3/(2.5E06)), c(3/(5),3/(100),3/(1E03),3/(5E04),3/(5E05)))
# tmp.df <- data.frame(low = phi.unif[[1]], high = phi.unif[[2]])
# inits$phi <- apply(tmp.df, 1, mean)
# 
# # ---------------------------------------------------------------
# 
# 



#####
# run the model
#####
out <- sfJSDM(formula = jsdm_formula, 
              data = data_list, 
              # inits = inits, 
              n.batch = num_batch, 
              batch.length = batch_length, 
              accept.rate = 0.43, 
              # priors = priors, 
              n.factors = num_factors,
              cov.model = cov_model, 
              # tuning = tuning, 
              # n.omp.threads = num_omp_threads, 
              verbose = TRUE, 
              NNGP = TRUE, 
              n.neighbors = 5, 
              n.report = num_report, 
              n.burn = num_burn, 
              n.thin = num_thin, 
              n.chains = num_chains)

saveRDS(out, paste0(chain, "prelim_mod_herb.Rda"))