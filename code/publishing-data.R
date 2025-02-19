## ----data-import, echo=FALSE, message=FALSE, warning=FALSE------------------------------------------
library(tidyverse)
library(randomForest)
library(here)
library(patchwork)
features <- read.csv(here("data/hamby-comparisons.csv"))
rf3 <- readRDS(here("data/csafe_rf3.rds"))
important <- data.frame(rf3$importance) %>% arrange(desc(MeanDecreaseGini))
important$name <- row.names(important)
doubles <- duplicated(features$id)
features <- features %>% filter(!doubles, !damaged)

library(pROC)
important$auc <- NA
for (i in 1:nrow(important)) {
  important$auc[i] <- auc(features$samesource, features[,important$name[i]])
}
detach(package:pROC)


## ----densities, echo=FALSE, message=FALSE, warning=FALSE, fig.width = 10, fig.height=8, out.width='\\textwidth', fig.cap="Histograms of same-source and different source comparisons of each  feature used in fitting the random forest model. Features are sorted according to decreasing importance (measured as Gini contribution). AUC (area under the curve) describes each feature's univariate strength to distinguish between same source and different source."----

long_rf <- features %>%
  filter(!doubles, land1 != land2) %>%
  select(important$name, samesource) %>%
  pivot_longer(cols = -samesource) %>%
  mutate(
    name = factor(name, levels=important$name),
    `Ground truth` = c("Different", "Same source")[as.numeric(samesource)+1]
  ) %>% left_join(important %>% select(name, auc), by="name")

data_rf <- long_rf %>% group_by(name, auc) %>% nest()

ggs <- 1:nrow(data_rf) %>% lapply(FUN = function(i) {
  data_rf$data[[i]] %>%
  ggplot(aes(x = value, colour=`Ground truth`, fill = `Ground truth`)) +
  geom_histogram(alpha = 0.8) +
  facet_grid(`Ground truth`~., scales="free") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values=c("darkgrey", "darkorange"))+
  scale_fill_manual(values=c("darkgrey", "darkorange")) +
  ggtitle(sprintf("%s\nAUC %.2f", data_rf$name[i], data_rf$auc[i])) +
  xlab("") + ylab("") +
    theme(plot.title = element_text(size = 10))
})

leg <- ggpubr::get_legend(ggs[[1]] + theme(legend.position = "right"))

# Figure 2
wrap_plots(ggs) + plot_spacer() + ggpubr::as_ggplot(leg)



# long_rf %>%
#   ggplot(aes(x = value)) +
#   geom_density(aes(fill=`Ground truth`), alpha = 0.7, size = 0.2) +
#   facet_wrap(~factor(name, levels=important$name), scales = "free") +
#   scale_fill_manual(values=c("darkgrey", "darkorange")) +
#   theme_bw() +
#   theme(legend.position = c(.8,0.1)) +
#   geom_label(aes(x = Inf, y = Inf, label=sprintf("AUC: %.3f", round(auc,3))), data = important,
#             color = "grey50", size = 4.5, vjust=1, hjust=1,label.size = 0) +
#   xlab("Value observed in the comparison of two LEA scans")


## ----outcomes, echo=FALSE, message=FALSE, warning=FALSE, fig.align="center", fig.width = 6, fig.height=4, out.width='.7\\textwidth', fig.cap="Histograms by source for all out-of-bag predictions of the csafe\\_rf3 random forest model trained on the features in `hamby-comparisons.csv'."----
## Code for Figure 3
features$rf_oob <- predict(rf3, type="prob")[,2]

features %>%
  mutate(
    samesource = factor(samesource, labels=c("Different-source", "Same-source")),
    Comparison = samesource
  ) %>%
  ggplot(aes(x = rf_oob, fill = Comparison, colour=Comparison)) +
  geom_histogram(alpha=0.6) +
  scale_colour_manual(values = c("darkgrey", "darkorange")) +
  scale_fill_manual(values = c("darkgrey", "darkorange")) +
  theme_bw() +
  xlab("Out-of-bag Random Forest Prediction values") +
  facet_grid(samesource~., scales="free") +
  theme(legend.position="bottom") +
  ylab("Number of Comparisons")




## ----read-forest, echo=FALSE, tidy=FALSE------------------------------------------------------------
csafe_rf3 <- readRDS(here::here("data/csafe_rf3.rds"))
error_rf3 <- csafe_rf3$err.rate %>% data.frame() %>% mutate(iteration = 1:n())
confusion <- csafe_rf3$confusion
row.names(confusion) <- c("Different","Same-Source")
#colnames(confusion) <- c("Predicted\nDifferent", "Predicted\nSame", "Error")
#knitr::kable(confusion, row.names = TRUE, label="confusion", align="c", caption="Confusion matrix of Out-of-Bag predictions versus ground truth for the 76,245 comparison pairs.", format = "latex")


## ----show-forest, echo=FALSE, fig.height = 5, fig.width = 8, fig.cap="Sequence of out-of-bag error rates over the cumulative number of trees evaluated for the random forest. After 200 iterations (not show), the fit stabilizes and no further reduction of the error rate is achieved."----
## Code for Figure 4

error_rf3 %>%
  pivot_longer(-iteration, names_to="Types of Errors", values_to="Error rate") %>%
  mutate(
    Types = `Types of Errors`,
    Types = factor(Types, labels=c("Overall & False positives", "Overall & False positives", "False negatives")),
    Types = factor(Types, levels=c("False negatives", "Overall & False positives"))
    ) %>%
  ggplot(aes(x = iteration, y = `Error rate`, colour = `Types of Errors`)) +
    geom_line(linewidth = 1) +
  theme_bw() +
  ylab("Error rate") +
  facet_grid(Types~., scale="free_y") +
  scale_colour_manual("Types of Errors", values=c("orange", "grey50", "#219ebc"),
                      labels=c("False positives", "Overall", "False negatives"),
                      guide = guide_legend(reverse = TRUE)) +
  theme(legend.position.inside = TRUE,  legend.position= c(.9, .45))


## ----session-info-----------------------------------------------------------------------------------
require(sessioninfo)
session_info()

