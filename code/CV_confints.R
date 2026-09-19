### 
# Script to get plot of cross validation confints
###

# load the data
out1 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_1factors_CV_wSE_2026-09-03.rds")
out2 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_2factors_CV_wSE_2026-09-06.rds")
out3 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_3factors_CV_wSE_2026-09-12.rds")
out4 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_4factors_CV_wSE_2026-09-12.rds")
out5 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_5factors_CV_wSE_2026-09-11.rds")
out6 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_6factors_CV_wSE_2026-09-13.rds")
out7 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_7factors_CV_wSE_2026-09-13.rds")
out8 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_8factors_CV_wSE_2026-09-13.rds")
out9 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_9factors_CV_wSE_2026-09-14.rds")
out10 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_10factors_CV_wSE_2026-09-06.rds")
out15 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_15factors_CV")

# put together
cv_vals <- list(factor1 = out1$k.fold.deviance, 
                factor2 = out2$k.fold.deviance,
                factor3 = out3$k.fold.deviance,
                factor4 = out4$k.fold.deviance,
                factor5 = out5$k.fold.deviance,
                factor6 = out6$k.fold.deviance,
                factor7 = out7$k.fold.deviance,
                factor8 = out8$k.fold.deviance,
                factor9 = out9$k.fold.deviance,
                factor10 = out10$k.fold.deviance)
# average across species
cv_vals <- lapply(cv_vals, function(x){sapply(x, mean, na.rm = T)})
# compute confidence intervals
cv_confints <- sapply(cv_vals, function(x){t.test(x)$conf.int})

# plot
factor_numbers <- 1:10
plot(factor_numbers, sapply(cv_vals, mean, na.rm = T), 
     ylim = c(min(cv_confints) - 5, max(cv_confints) + 5), 
     type = "b")
segments(x0 = factor_numbers, y0 = cv_confints[1,], y1 = cv_confints[2,], col = 2)


# Try t.tests
t.test(cv_vals$factor1, cv_vals$factor2, paired = T)
# FTR
t.test(cv_vals$factor2, cv_vals$factor3)
# FTR
t.test(cv_vals$factor3, cv_vals$factor4)
# FTR
t.test(cv_vals$factor4, cv_vals$factor5)
# FTR
t.test(cv_vals$factor5, cv_vals$factor6)
# FTR
t.test(cv_vals$factor6, cv_vals$factor7)
# FTR
t.test(cv_vals$factor7, cv_vals$factor8)
# FTR
t.test(cv_vals$factor8, cv_vals$factor10)
# FTR

t.test(cv_vals$factor1, cv_vals$factor3)
# reject
t.test(cv_vals$factor2, cv_vals$factor4)
# reject
t.test(cv_vals$factor3, cv_vals$factor5)
# reject
t.test(cv_vals$factor4, cv_vals$factor6)
# FTR
t.test(cv_vals$factor5, cv_vals$factor7)
# FTR
t.test(cv_vals$factor6, cv_vals$factor8)
# FTR
t.test(cv_vals$factor7, cv_vals$factor10)
# FTR

t.test(cv_vals$factor1, cv_vals$factor4)
# reject
t.test(cv_vals$factor2, cv_vals$factor5)
# reject
t.test(cv_vals$factor3, cv_vals$factor6)
# reject
t.test(cv_vals$factor4, cv_vals$factor7)
# FTR
t.test(cv_vals$factor5, cv_vals$factor8)
# FTR
t.test(cv_vals$factor6, cv_vals$factor10)
# FTR

t.test(cv_vals$factor1, cv_vals$factor5)
# reject
t.test(cv_vals$factor2, cv_vals$factor6)
# reject
t.test(cv_vals$factor3, cv_vals$factor7)
# reject
t.test(cv_vals$factor4, cv_vals$factor8)
# reject
t.test(cv_vals$factor6, cv_vals$factor9, paired = T)
# FTR

sd(cv_vals$factor5 - cv_vals$factor9)
