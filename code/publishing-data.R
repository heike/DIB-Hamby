library(tidyverse)
features <- read.csv("~/CSAFE/features-hamby.csv", stringsAsFactors = FALSE)

hamby <- features %>% 
  filter(study1 %in% c("Hamby44", "Hamby252"), study2 %in% c("Hamby44", "Hamby252"))

hamby <- hamby %>% mutate(
  nbarrel1 = parse_number(barrel1),
  nbarrel2 = parse_number(barrel2)
)

hamby <- hamby %>% mutate(
  bullet1 = ifelse(!is.na(nbarrel1), bullet1, barrel1),
  bullet2 = ifelse(!is.na(nbarrel2), bullet2, barrel2),
  barrel1 = ifelse(is.na(nbarrel1),  "Unk", barrel1),
  barrel2 = ifelse(is.na(nbarrel2), "Unk", barrel2),
  study1 = ifelse(study1=="Hamby44", "Hamby173", study1),
  study2 = ifelse(study2=="Hamby44", "Hamby173", study2)
)

hamby <- hamby %>% mutate(
  land_id1 = paste(study1, paste0("Br", barrel1), paste0("B", bullet1), paste0("L", land1), sep="-"),
  land_id2 = paste(study2, paste0("Br", barrel2), paste0("B", bullet2), paste0("L", land2), sep="-")
)

#barrel 6 bullet 2-1
#barrel 9 bullet 2-4
#unknown bullet B-2
#unknown bullet Q-4

# identified as problematic by AOAS paper:
hamby <- hamby %>% mutate(
  flag = ifelse(land_id1 %in% c("Hamby252-Br6-B2-L1", "Hamby252-Br9-B2-L4", "Hamby252-Unk-BB-L2", "Hamby252-Unk-BQ-L4"), TRUE, FALSE),
  flag = ifelse(land_id2 %in% c("Hamby252-Br6-B2-L1", "Hamby252-Br9-B2-L4", "Hamby252-Unk-BB-L2", "Hamby252-Unk-BQ-L4"), TRUE, flag)
)

hamby <- hamby %>% mutate(
  abs_lag = abs(lag)
)

publish <- hamby %>% filter(!flag) %>%
   select(land_id1, land_id2, ccf, rough_cor, lag, abs_lag, D, sd_D, 
                            matches, mismatches, cms, overlap,
                            non_cms, sum_peaks, signature_length, same_source=match)

write.csv(publish, "~/CSAFE/hamby-comparisons.csv", row.names = FALSE)
####
hamby <- read.csv("~/CSAFE/hamby-comparisons.csv", stringsAsFactors = FALSE)


  
# fit the forest
library(randomForest)
set.seed(20140501)
rtrees <- randomForest(factor(same_source) ~ ccf + rough_cor + abs_lag + D + sd_D + matches + mismatches + cms + non_cms + sum_peaks + overlap, data = hamby)

saveRDS(rtrees, "~/CSAFE/rtrees.rds")

hamby$rfscore <- predict(rtrees, type="prob", newdata = hamby)[,2]


hamby %>% ggplot(aes(x = ccf, y = rfscore)) + geom_point() + facet_wrap(~same_source, labeller = "label_both")

hamby %>% ggplot(aes(x = lag, y = ccf, colour =same_source)) + geom_point() + facet_wrap(~same_source)
hamby %>% ggplot(aes(x = ccf, y = rough_cor)) + geom_point() + 
  facet_wrap(~same_source) +
  geom_point(aes(colour = flag), data = hamby %>% filter(flag))

###############
hambyrev <- hamby
hambyrev$land_id1 <- hamby$land_id2
hambyrev$land_id2 <- hamby$land_id1
hamby2 <- rbind(hamby, hambyrev)
hamby2 %>% group_by(land_id2) %>% tally() %>% arrange(desc(n))
hamby2 <- hamby2 %>% 
  separate(land_id1, remove = FALSE, sep="-", 
           into=c("study1", "barrel1", "bullet1", "land1")) %>%
  separate(land_id2, remove = FALSE, sep="-", 
           into=c("study2", "barrel2", "bullet2", "land2"))

hamby2 %>% filter(barrel1 %in% c("BrUnk"), bullet1 %in% "BU") %>% 
  group_by(study1, bullet1, land1) %>% tally() %>% data.frame()

