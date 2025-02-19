# read the features
features <- read.csv("data/hamby-comparisons.csv")

##########
# fit the random forest
library(randomForest)

# train random forest
#set.seed(20140105)
#csafe_rf3_big <- randomForest(factor(samesource) ~ ccf + rough_cor + matches_per_mm + D + sd_D+ cms_per_mm +
#                            mismatches_per_mm + overlap + abs_lag_mm + sum_peaks,
#                          importance=TRUE, ntree=1000,
#                          data = features)
#
#plot(csafe_rf3_big)
#abline(a=0.09, b=0)



set.seed(20140105)
csafe_rf3 <- randomForest(factor(samesource) ~ ccf + rough_cor + matches_per_mm + D + sd_D+ cms_per_mm +
                            mismatches_per_mm + overlap + abs_lag_mm + sum_peaks,
                          importance=TRUE, ntree=200,
                          data = features)

saveRDS(csafe_rf3, "data/csafe_rf3.rds")
