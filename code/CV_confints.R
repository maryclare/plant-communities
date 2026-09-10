### 
# Script to get plot of cross validation confints
###

# load the data
out1 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_1factors_CV_wSE_2026-09-03.rds")
out2 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_2factors_CV_wSE_2026-09-06.rds")
out5 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_5f")
out10 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_10factors_CV_wSE_2026-09-06.rds")

# put together
cv_vals <- list(factor1 = out1$k.fold.deviance, 
                factor2 = out2$k.fold.deviance, 
                factor5 = out5$k.fold.deviance, 
                factor10 = out10$k.fold.deviance)
# average across species
cv_vals <- lapply(cv_vals, function(x){sapply(x, mean, na.rm = T)})
# compute confidence intervals
cv_confints <- sapply(cv_vals, function(x){t.test(x)$conf.int})

# plot
factor_numbers <- c(1, 2, 5, 10)
plot(factor_numbers, sapply(cv_vals, mean, na.rm = T), 
     ylim = c(min(cv_confints) - 5, max(cv_confints) + 5))
segments(x0 = factor_numbers, y0 = cv_confints[1,], y1 = cv_confints[2,], col = 2)


# Try t.tests
t.test(cv_vals$factor1, cv_vals$factor2)
# FTR
t.test(cv_vals$factor1, cv_vals$factor5)
# reject

t.test(cv_vals$factor2, cv_vals$factor5)
# reject
t.test(cv_vals$factor2, cv_vals$factor10)
# reject

t.test(cv_vals$factor5, cv_vals$factor10)
# FTR




