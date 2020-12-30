# read the features
features <- read.csv("hamby-comparisons.csv")

##########
# fit the random forest
library(randomForest)

set.seed(20140105)
csafe_rf2 <- randomForest(factor(samesource) ~ ccf + rough_cor + matches_per_mm + D + sd_D+ cms_per_mm +
                          mismatches_per_mm + overlap + abs_lag_mm + sum_peaks,
                        data = features)

saveRDS(csafe_rf2, "csafe_rf2.rds")
