# read the features
features <- read.csv("data/hamby-comparisons.csv")

##########
# fit the random forest
library(randomForest)

# train random forest
set.seed(20140105)
csafe_rf3 <- randomForest(factor(samesource) ~ ccf + rough_cor + matches_per_mm + D + sd_D+ cms_per_mm +
                            mismatches_per_mm + overlap + abs_lag_mm + sum_peaks,
                          importance=TRUE, ntree=200,
                          data = features)

csafe_rf3 <- randomForest(factor(samesource) ~ ccf + rough_cor + matches_per_mm + D + sd_D+ cms_per_mm +
                          mismatches_per_mm + overlap + abs_lag_mm + sum_peaks,
                          importance=TRUE, ntree=1000,
                          data = features)
plot(csafe_rf4)
abline(a=0.09, b=0)
which.min(csafe_rf4$err.rate[,1])
csafe_rf4$err.rate[196,]
csafe_rf4$err.rate[200,]

set.seed(20140105)
csafe_rf3sub <- randomForest(factor(samesource) ~ ccf + rough_cor + matches_per_mm + D + sd_D+ cms_per_mm +
                            mismatches_per_mm + overlap + abs_lag_mm + sum_peaks,
                          importance=TRUE, ntree=331,
                          data = features)


saveRDS(csafe_rf3sub, "data/csafe_rf3.rds")
