### 
# Script to get plot of cross validation confints
###
library(tidyverse)

# load the data
out1 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_1factors_CV_wSE_tempPrecipBlocks_2026-09-24.rds")
out2 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_2factors_CV_wSE_tempPrecipBlocks_2026-09-24.rds")
out3 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_3factors_CV_wSE_tempPrecipBlocks_2026-09-25.rds")
out4 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_4factors_CV_wSE_tempPrecipBlocks_2026-09-25.rds")
out5 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_5factors_CV_wSE_tempPrecipBlocks_2026-09-25.rds")
out6 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_6factors_CV_wSE_tempPrecipBlocks_2026-09-25.rds")
out7 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_7factors_CV_wSE_tempPrecipBlocks_2026-09-25.rds")
out8 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_8factors_CV_wSE_tempPrecipBlocks_2026-09-25.rds")
out9 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_9factors_CV_wSE_tempPrecipBlocks_2026-09-25.rds")
out10 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_10factors_CV_wSE_tempPrecipBlocks_2026-09-25.rds")
# out15 <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_15factors_CV_wSE_2026-09-16.rds")

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
                factor10 = out10$k.fold.deviance) # , 
                # factor15 = out15$k.fold.deviance)
# average across species
cv_vals <- lapply(cv_vals, function(x){sapply(x, mean, na.rm = T)})
# compute confidence intervals
cv_confints <- sapply(cv_vals, function(x){t.test(x)$conf.int})

# plot
factor_numbers <- c(1:10) #, 15)
plot(factor_numbers, sapply(cv_vals, mean, na.rm = T), 
     ylim = c(min(cv_confints) - 5, max(cv_confints) + 5), 
     type = "b")
segments(x0 = factor_numbers, y0 = cv_confints[1,], y1 = cv_confints[2,], col = 2)



# save results in a list: 
random_holdouts <- list(cv_vals = cv_vals, 
                        cv_confints = cv_confints)
full_env_holdouts <- list(cv_vals = cv_vals, 
                          cv_confints = cv_confints)
tempPrecip_holdouts <- list(cv_vals = cv_vals, 
                            cv_confints = cv_confints)

full_data <- data.frame(factors = c(1:10 - 0.05, 1:10, 1:10 + 0.05), 
                        holdouts = c(rep("random", 10), rep("full env", 10), rep("temp precip", 10)), 
                        means = c(sapply(random_holdouts$cv_vals, mean, na.rm = T)[1:10], 
                                  sapply(full_env_holdouts$cv_vals, mean, na.rm = T), 
                                  sapply(tempPrecip_holdouts$cv_vals, mean, na.rm = T)), 
                        lowers = c(random_holdouts$cv_confints[1,1:10], 
                                   full_env_holdouts$cv_confints[1,], 
                                   tempPrecip_holdouts$cv_confints[1,]), 
                        uppers = c(random_holdouts$cv_confints[2,1:10], 
                                   full_env_holdouts$cv_confints[2,], 
                                   tempPrecip_holdouts$cv_confints[2,]), 
                        sds = c(sapply(random_holdouts$cv_vals, sd, na.rm = T)[1:10], 
                                sapply(full_env_holdouts$cv_vals, sd, na.rm = T), 
                                sapply(tempPrecip_holdouts$cv_vals, sd, na.rm = T))/sqrt(10))
full_data$holdouts <- factor(full_data$holdouts, 
                             levels = c("temp precip", "full env", "random"), 
                             ordered = FALSE)

ggplot(full_data) + 
  geom_point(aes(x = factors, y = means, color = holdouts)) + 
  geom_line(aes(x = factors, y = means, color = holdouts)) + 
  # geom_segment(aes(x = factors, xend = factors, 
  #                  y = means + sds, yend = means - sds, 
  #                  color = holdouts)) + 
  scale_x_continuous(breaks = 1:10, minor_breaks = F) + 
  xlab("CV means") + 
  theme_bw()

# one SE rule
full_data |> 
  filter(holdouts == "temp precip") %>% 
  ggplot() + 
  geom_point(aes(x = factors, y = means), color = 2) + 
  geom_line(aes(x = factors, y = means), color = 2) + 
  geom_segment(aes(x = factors, xend = factors,
                   y = means + sds, yend = means - sds),
                   color = 2) + 
  geom_segment(aes(x = 1, xend = 10.05, y = (means + sds)[9]), 
               lty = 2, col = "grey") + 
  scale_x_continuous(breaks = 1:10, minor_breaks = F) + 
  xlab("CV means") + 
  theme_bw()

full_data |> 
  filter(holdouts == "full env") %>% 
  ggplot() + 
  geom_point(aes(x = factors, y = means), color = 3) + 
  geom_line(aes(x = factors, y = means), color = 3) + 
  geom_segment(aes(x = factors, xend = factors,
                   y = means + sds, yend = means - sds),
                   color = 3) + 
  geom_segment(aes(x = 1, xend = 10.05, y = (means + sds)[9]), 
               lty = 2, col = "grey") + 
  scale_x_continuous(breaks = 1:10, minor_breaks = F) + 
  xlab("CV means") + 
  theme_bw()

full_data |> 
  filter(holdouts == "random") %>% 
  ggplot() + 
  geom_point(aes(x = factors, y = means), color = 4) + 
  geom_line(aes(x = factors, y = means), color = 4) + 
  geom_segment(aes(x = factors, xend = factors,
                   y = means + sds, yend = means - sds),
                   color = 4) + 
  geom_segment(aes(x = 1, xend = 10.05, y = (means + sds)[9]), 
               lty = 2, col = "grey") + 
  scale_x_continuous(breaks = 1:10, minor_breaks = F) + 
  xlab("CV means") + 
  theme_bw()


saveRDS(full_data, "../plant_com_non_git/CV_results_with_blocking.rds")

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
