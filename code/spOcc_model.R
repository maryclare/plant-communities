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
batch_length    <- 25
num_batch       <- 1000
num_burn        <- 15000
num_thin        <- 10 #
num_chains      <- 1
tuning          <- list(phi = 1)
num_omp_threads <- cores
verbose         <- TRUE
num_report      <- 200

# Model formula
jsdm_formula <- ~ scale(tmin) + I(scale(tmin)^2) + scale(ppt) + I(scale(ppt)^2) 

# distance matrix between sites
dist_matrix <- dist(data_list$coords)

#####
# Initial values
#####
lambda_inits <- matrix(0, num_species, num_factors)
diag(lambda_inits) <- 1
lambda_inits[lower.tri(lambda_inits)] <- rnorm(sum(lower.tri(lambda_inits)))




# ---------------------------------------------------------------


######get inits for beta, beta.com and tau.sq.beta
resp  <- t(data_list$y)

inidf <- cbind(resp, data_list$covs)
spcs  <- 1:ncol(resp)
betaz <- data.frame()

for(i in spcs){
  yhat  <- inidf[,i]
  check <- summary(lm(yhat~scale(tmin) + I(scale(tmin)^2)+scale(ppt) + I(scale(ppt)^2),data=inidf))
  bet   <- check$coefficients[,1]
  betaz <- rbind(bet,betaz)
}

colnames(betaz) <- c("Intercept","tmin","tmin^2","ppt","ppt^2")

betaz.mat <- as.matrix.data.frame(betaz)
betaz.com <- data.frame(Intercept=mean(betaz$Intercept),tmin=mean(betaz$tmin),`tmin^2`=mean(betaz$`tmin^2`),ppt=mean(betaz$ppt),`ppt^2`=mean(betaz$`ppt^2`))
betaz.var <- data.frame(Intercept=var(betaz$Intercept),tmin=var(betaz$tmin),`tmin^2`=var(betaz$`tmin^2`),ppt=var(betaz$ppt),`ppt^2`=var(betaz$`ppt^2`))

betaz.com <- as.matrix.data.frame(betaz.com)
betaz.var <- as.matrix.data.frame(betaz.var)

inits     <- list(beta.comm = betaz.com,
                  beta = betaz.mat,
                  tau.sq.beta = betaz.var,
                  lambda = lambda.inits, 
                  phi = 3 / mean(dist.mat))

#####
# Priors
#####
min.dist <- min(dist.mat)
max.dist <- max(dist.mat)

priors <- list(beta.comm.normal = list(mean = 0, var = 2.72),
               tau.sq.beta.ig = list(a = 0.1, b = 0.1),
               #phi.unif = list(3 /max.dist, 3 / min.dist))
               #phi.unif = list(c(3/(2.5E06),3/(5E05),3/(5E04),3/(1E03),3/(100)), c(3/(5E05),3/(5E04),3/(1E03), 3/(100),3/(5))))
               phi.unif = list(c(3/(100),3/(1E03),3/(5E04),3/(5E05),3/(2.5E06)), c(3/(5),3/(100),3/(1E03),3/(5E04),3/(5E05))))


phi.unif =  list(c(3/(100),3/(1E03),3/(5E04),3/(5E05),3/(2.5E06)), c(3/(5),3/(100),3/(1E03),3/(5E04),3/(5E05)))
tmp.df <- data.frame(low = phi.unif[[1]], high = phi.unif[[2]])
inits$phi <- apply(tmp.df, 1, mean)

# ---------------------------------------------------------------





#####
# run the model
#####
out <- sfJSDM(formula = jsdm_formula, 
              data = data_list, 
              inits = inits, 
              n.batch = num_batch, 
              batch.length = batch_length, 
              accept.rate = 0.43, 
              priors = priors, 
              n.factors = num_factors,
              cov.model = cov_model, 
              tuning = tuning, 
              n.omp.threads = num_omp_threads, 
              verbose = TRUE, 
              NNGP = TRUE, 
              n.neighbors = 5, 
              n.report = num_report, 
              n.burn = num_burn, 
              n.thin = num_thin, 
              n.chains = num_chains)

saveRDS(out, paste0(chain, "prelim_mod_herb.Rda"))