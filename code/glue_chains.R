#####
# this script glues the second run of a chain onto the first run so that samples
#     saved separately can be analyzed together. It saves the object as a 
#     spOccupancy model object, such that the object can subsequently be used 
#     with all spAbundance model functions.
# Adapted from the script here: https://github.com/doserjef/Switzerland24-Spatial-Workshop/blob/main/code/12d-combine-chains.R

library(spOccupancy)
library(coda)
library(abind)

# Read in the model objects from the three chains -------------------------
out1 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_20factors_samps100k125k_chain2_2026-08-17.rds")
out2 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_20factors_samps125k150k_chain2_2026-08-18.rds")
out3 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_20factors_samps150k175k_chain2_2026-08-20.rds")
out4 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_20factors_samps175k200k_chain2_2026-08-21.rds")
## Adjust to your needs or load separately

# Other stuff -------------------------------------------------------------
# This stuff is used under the hood for use with summaries, plotting,
# prediction, WAIC,  PPCs.
out.full <- out1 # set to baseline so I don't have to manually input everything

# Combine all MCMC samples together ---------------------------------------
# Community-level abundance effects
out.full$beta.comm.samples <- mcmc(rbind(out1$beta.comm.samples,
                                         out2$beta.comm.samples,
                                         out3$beta.comm.samples,
                                         out4$beta.comm.samples))
# Community-level abundance variances
out.full$tau.sq.beta.samples <- mcmc(rbind(out1$tau.sq.beta.samples,
                                           out2$tau.sq.beta.samples,
                                           out3$tau.sq.beta.samples,
                                           out4$tau.sq.beta.samples))

# Species-level abundance regression coefficients
out.full$beta.samples <- mcmc(rbind(out1$beta.samples,
                                    out2$beta.samples,
                                    out3$beta.samples,
                                    out4$beta.samples))

# Spatial decay parameters
out.full$theta.samples <- mcmc(rbind(out1$theta.samples,
                                     out2$theta.samples,
                                     out3$theta.samples,
                                     out4$theta.samples))
# Spatial factor loadings
out.full$lambda.samples <- mcmc(rbind(out1$lambda.samples,
                                      out2$lambda.samples,
                                      out3$lambda.samples,
                                      out4$lambda.samples))
# Spatial factors
out.full$w.samples <- abind(out1$w.samples, 
                            out2$w.samples, 
                            out3$w.samples, 
                            out4$w.samples, 
                            along = 1)
# # Occurrance probability of species i at site j
# out.full$psi.samples <- abind(out1$psi.samples, 
#                               out2$psi.samples, 
#                               out3$psi.samples, 
#                               out4$psi.samples, 
#                               along = 1)
# likelihood samples
out.full$like.samples <- mcmc(rbind(out1$like.samples, 
                               out2$like.samples, 
                               out3$like.samples, 
                               out4$like.samples))
# # expected occupancy values
# out.full$z.samples <- abind(out1$z.samples,
#                             out2$z.samples,
#                             out3$z.samples,
#                             out4$z.samples,
#                             along = 1)
# Get RHat values for the main parameters ---------------------------------
out.full$rhat <- out1$rhat
# beta.comm
tmp <- mcmc.list(out1$beta.comm.samples, 
                 out2$beta.comm.samples, 
                 out3$beta.comm.samples, 
                 out4$beta.comm.samples)
out.full$rhat$beta.comm <- as.vector(gelman.diag(tmp, autoburnin = FALSE)$psrf[, 2])
# tau.sq.beta
tmp <- mcmc.list(out1$tau.sq.beta.samples, 
                 out2$tau.sq.beta.samples, 
                 out3$tau.sq.beta.samples, 
                 out4$tau.sq.beta.samples)
out.full$rhat$tau.sq.beta <- as.vector(gelman.diag(tmp, autoburnin = FALSE)$psrf[, 2])
# beta
tmp <- mcmc.list(out1$beta.samples, 
                 out2$beta.samples, 
                 out3$beta.samples, 
                 out4$beta.samples)
out.full$rhat$beta <- as.vector(gelman.diag(tmp, autoburnin = FALSE)$psrf[, 2])
# theta
tmp <- mcmc.list(out1$theta.samples, 
                 out2$theta.samples, 
                 out3$theta.samples, 
                 out4$theta.samples)
out.full$rhat$theta <- as.vector(gelman.diag(tmp, autoburnin = FALSE)$psrf[, 2])
# lambda
tmp <- mcmc.list(out1$lambda.samples, 
                 out2$lambda.samples, 
                 out3$lambda.samples, 
                 out4$lambda.samples)
out.full$rhat$lambda <- as.vector(gelman.diag(tmp, autoburnin = FALSE, multivariate = FALSE)$psrf[, 2])

# Get ESS values for the main parameters ----------------------------------
out.full$ESS <- list()
out.full$ESS$beta.comm <- effectiveSize(out.full$beta.comm.samples)
out.full$ESS$tau.sq.beta <- effectiveSize(out.full$tau.sq.beta.samples)
out.full$ESS$beta <- effectiveSize(out.full$beta.samples)
out.full$ESS$lambda <- effectiveSize(out.full$lambda.samples)
out.full$ESS$theta <- effectiveSize(out.full$theta.samples)


# Run time
# Setting the "overall" run time to be the longest run time across the three chains
tmp <- which.max(c(out1$run.time[3], 
                   out2$run.time[3], 
                   out3$run.time[3], 
                   out4$run.time[3]))
if (tmp == 1) out.full$run.time <- out1$run.time
if (tmp == 2) out.full$run.time <- out2$run.time
if (tmp == 3) out.full$run.time <- out3$run.time
if (tmp == 4) out.full$run.time <- out4$run.time

# fix sample sizes 
out.full$n.post <- 10000
out.full$n.samples <- 200000

# Make sure the new object has class sfJSDM ------------------------------
class(out.full) <- 'sfJSDM'

# Check to make sure it's working -----------------------------------------
# These should all just return without an error.
summary(out.full)
plot(out.full, 'beta.comm', density = FALSE)

source("~/plant_communities/code/model_assesment_functions.R")
plot_lhood(out.full)

saveRDS(out.full, "~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_20factors_samps100k200k_chain2_2026-08-26.rds")



