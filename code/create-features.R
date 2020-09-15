library(tidyverse)
library(x3ptools)
library(bulletxtrctr)
x3p <- read_x3p("~/papers/dissertations/eric-dissertation/images/Hamby (2009) Barrel/bullets/Barrel 1/Br1 Bullet 1-5.x3p")
x3p <- x3p %>% x3p_rotate(angle = -90)
x3p <- x3p %>% y_flip_x3p()
x3p <- x3p %>% x3p_m_to_mum()
#x3p %>% x3p_image()

cc <- x3p %>% x3p_crosscut_optimize()
ccdata <- x3p %>% x3p_crosscut(y = cc)
ccdata %>% ggplot(aes(x = x, y = value)) + geom_line()

grooves <- ccdata %>% cc_locate_grooves(return_plot = TRUE)

sigs <- ccdata %>% cc_get_signature(grooves)
sigs %>%
  ggplot(aes(x = x, y = sig)) + geom_line()


bstats <- read.csv("~/papers/dissertations/eric-dissertation/data/data-25-25/bullet-stats.csv", stringsAsFactors = FALSE)

bullets <- read_bullet("~/papers/dissertations/eric-dissertation/images/Hamby (2009) Barrel/bullets/Barrel 1/")
bullets <- bullets %>%
  mutate(
    x3p = x3p %>% purrr::map(.f = function(x) {
      x <- x %>% x3p_rotate(angle=-90) %>% y_flip_x3p()
      x %>% x3p_m_to_mum()
    })
  )

bullets <- bullets %>% mutate(
  cc = x3p %>% purrr::map(.f = function(x) x3p_crosscut_optimize(x))
)

bullets <- bullets %>% mutate(
  ccdata = purrr::map2(.x = x3p, .y = cc, .f = function(x, y) x3p_crosscut(x3p=x, y = y))
)

bullets <- bullets %>% mutate(
  grooves = ccdata %>% purrr::map(.f = function(x) cc_locate_grooves(x))
)

bullets <- bullets %>% mutate(
  sigs = purrr::map2(.x = ccdata, .y = grooves, .f = function(x, y) cc_get_signature(ccdata=x, grooves = y))
)

signatures <- bullets %>% unnest(sigs)
signatures %>%
  ggplot(aes( x= x, y = sig)) +
  geom_line() +
  facet_wrap(~source, ncol=6)


lands <- unique(bullets$source)
comparisons <- data.frame(
  expand.grid(land1 = lands, land2 = lands), stringsAsFactors = FALSE)

comparisons <- comparisons %>% mutate(
  aligned = purrr::map2(.x = land1, .y = land2, .f = function(xx, yy) {
    land1 <- bullets$sigs[bullets$source == xx][[1]]
    land2 <- bullets$sigs[bullets$source == yy][[1]]
    land1$bullet <- "first-land"
    land2$bullet <- "second-land"

    sig_align(land1$sig, land2$sig)
  })
)

comparisons <- comparisons %>% mutate(
  striae = aligned %>% purrr::map(.f = sig_cms_max, span = 75)
)

comparisons <- comparisons %>% mutate(
  legacy_features = purrr::map(striae, extract_features_all_legacy, resolution = 1.5625)
)

legacy <- comparisons %>% tidyr::unnest(legacy_features)
legacy <- legacy %>% mutate(
  bullet1 = gsub(".*(Bullet [12]).*", "\\1", land1),
  l1 = gsub(".*Bullet [12]-([1-6]).*", "\\1", land1),
  land_id1 = sprintf("Hamby252-Br1-B%d-L%s", parse_number(bullet1), l1)
)
legacy <- legacy %>% mutate(
  bullet2 = gsub(".*(Bullet [12]).*", "\\1", land2),
  l2 = gsub(".*Bullet [12]-([1-6]).*", "\\1", land2),
  land_id2 = sprintf("Hamby252-Br1-B%d-L%s", parse_number(bullet2), l2)
)

legacy %>% ggplot(aes(x = land_id1, y=land_id2, fill=ccf)) + geom_tile() +
  scale_fill_gradient2(low="darkgrey", mid="white", high = "darkorange", midpoint = 0.5)


cf <- read.csv("data/hamby-comparisons.csv")
br1 <- cf %>% filter(grepl("Hamby252-Br1-", land_id1), grepl("Hamby252-Br1-", land_id2))
br1 %>% ggplot(aes(x = land_id1, y=land_id2, fill=ccf)) + geom_tile() +
  scale_fill_gradient2(low="darkgrey", mid="white", high = "darkorange", midpoint = 0.5)


full_features <- legacy %>%
  left_join(br1, by=c("land_id1", "land_id2"))

feature_x <- full_features %>% select(land_id1, land_id2, ends_with(".x")) %>%
  pivot_longer(ends_with(".x"), names_to = "feature", values_to="values") %>%
  mutate(
    feature = gsub(".x", "", feature)
  )
feature_y <- full_features %>% select(land_id1, land_id2, ends_with(".y")) %>%
  pivot_longer(ends_with(".y"), names_to = "feature", values_to="values") %>%
  mutate(
    feature = gsub(".y", "", feature)
  )

features <- feature_x %>% left_join(feature_y, by=c("land_id1", "land_id2", "feature"))

features %>%
  ggplot(aes(x = values.x, y = values.y)) + geom_point() +
  facet_wrap(~feature, scales="free")

####
# try all barrels

bullets <- read_bullet("~/papers/dissertations/eric-dissertation/images/Hamby (2009) Barrel/bullets/")
bullets <- bullets %>%
  mutate(
    x3p = x3p %>% purrr::map(.f = function(x) {
    #  browser()
      dims <- dim(x$surface.matrix)
      if (dims[1] < dims[2]) {
        x <- x %>% x3p_rotate(angle=-90) %>% y_flip_x3p()
      } else {
         x <- x %>% y_flip_x3p()
      }
      x %>% x3p_m_to_mum()
    })
  )

cc <- rep(NA, nrow(bullets))
for (i in 1:nrow(bullets)) {
  cc[i] <- bullets$x3p[[i]] %>% x3p_crosscut_optimize()
}
bullets$cc <- cc

bullets <- bullets %>% mutate(
  ccdata = purrr::map2(.x = x3p, .y = cc, .f = function(x, y) x3p_crosscut(x3p=x, y = y))
)

bullets <- bullets %>% mutate(
  grooves = ccdata %>% purrr::map(.f = function(x) cc_locate_grooves(x))
)

bullets <- bullets %>% mutate(
  sigs = purrr::map2(.x = ccdata, .y = grooves, .f = function(x, y) cc_get_signature(ccdata=x, grooves = y))
)

signatures <- bullets %>% unnest(sigs)
signatures %>%
  ggplot(aes( x= x, y = sig)) +
  geom_line() +
  facet_wrap(~source, ncol=6)

saveRDS(bullets, "bullets.rds")

lands <- unique(bullets$source)
comparisons <- data.frame(
  expand.grid(land1 = lands, land2 = lands), stringsAsFactors = FALSE)

comparisons <- comparisons %>% mutate(
  aligned = purrr::map2(.x = land1, .y = land2, .f = function(xx, yy) {
    land1 <- bullets$sigs[bullets$source == xx][[1]]
    land2 <- bullets$sigs[bullets$source == yy][[1]]
    land1$bullet <- "first-land"
    land2$bullet <- "second-land"

    sig_align(land1$sig, land2$sig)
  })
)
saveRDS(comparisons, "comparisons.rds")

comparisons <- comparisons %>% mutate(
  striae = aligned %>% purrr::map(.f = sig_cms_max, span = 75)
)

comparisons <- comparisons %>% mutate(
  legacy_features = purrr::map(striae, extract_features_all_legacy, resolution = 1.5625)
)
saveRDS(comparisons, "comparisons.rds")

legacy <- comparisons %>% tidyr::unnest(legacy_features)
legacy <- legacy %>% mutate(
  study1 = ifelse(grepl("/Br", legacy$land1), "Hamby252", NA),
  study1 = ifelse(grepl("/Ukn", legacy$land1), "Hamby252", study1),
  study1 = ifelse(grepl("/br", legacy$land1), "Hamby173", study1),
  study2 = ifelse(grepl("/Br", legacy$land2), "Hamby252", NA),
  study2 = ifelse(grepl("/Ukn", legacy$land2), "Hamby252", study2),
  study2 = ifelse(grepl("/br", legacy$land2), "Hamby173", study2)
)

legacy <- legacy %>% mutate(
  barrel1 = gsub(".*((Br[0-9]+)|(Ukn)|(br[0-9A-Z]+)).*", "\\1", land1),
  barrel1 = ifelse(is.na(parse_number(barrel1)), "Ukn", parse_number(barrel1))
)
legacy <- legacy %>% mutate(
  barrel2 = gsub(".*((Br[0-9]+)|(Ukn)|(br[0-9A-Z]+)).*", "\\1", land2),
  barrel2 = ifelse(is.na(parse_number(barrel2)), "Ukn", parse_number(barrel2))
)

legacy <- legacy %>% mutate(
  bullet1 = gsub(".*((Bullet [12A-Z])|(_[12]_)).*", "\\1", land1),
  bullet1 = gsub("Bullet ", "", bullet1),
  bullet1 = ifelse(is.na(parse_number(bullet1)), bullet1, parse_number(bullet1))
)
legacy <- legacy %>% mutate(
  bullet2 = gsub(".*((Bullet [12A-Z])|(_[12]_)).*", "\\1", land2),
  bullet2 = gsub("Bullet ", "", bullet2),
  bullet2 = ifelse(is.na(parse_number(bullet2)), bullet2, parse_number(bullet2))
)

legacy <- legacy %>% mutate(
  l1 = gsub(".*Bullet [12A-Z]-([1-6]).*", "\\1", land1),
  l1 = gsub(".*_land([1-6]).*", "\\1", l1),
)
legacy <- legacy %>% mutate(
  l2 = gsub(".*Bullet [12A-Z]-([1-6]).*", "\\1", land2),
  l2 = gsub(".*_land([1-6]).*", "\\1", l2),
)


legacy <- legacy %>% mutate(
  land_id1 = sprintf("%s-Br%s-B%s-L%s", study1, barrel1, bullet1, l1),
  land_id2 = sprintf("%s-Br%s-B%s-L%s", study2, barrel2, bullet2, l2)
)


write.csv(legacy %>% select(-aligned, -striae), "Hamby173-252-features.csv", row.names=FALSE)


legacy %>% filter(study1 == "Hamby252", study2 == "Hamby252") %>%
  ggplot(aes(x = l1, y=l2, fill=ccf)) + geom_tile() +
  scale_fill_gradient2(low="darkgrey", mid="white", high = "darkorange", midpoint = 0.5) +
  facet_grid(barrel1+bullet1~barrel2+bullet2)


cf <- read.csv("data/hamby-comparisons.csv")


full_features <- legacy %>%
  left_join(cf, by=c("land_id1", "land_id2"))

feature_x <- full_features %>% select(land_id1, land_id2, ends_with(".x")) %>%
  pivot_longer(ends_with(".x"), names_to = "feature", values_to="values") %>%
  mutate(
    feature = gsub(".x", "", feature)
  )
feature_y <- full_features %>% select(land_id1, land_id2, ends_with(".y")) %>%
  pivot_longer(ends_with(".y"), names_to = "feature", values_to="values") %>%
  mutate(
    feature = gsub(".y", "", feature)
  )

feature_y <- na.omit(feature_y)
features <- feature_x %>% left_join(feature_y, by=c("land_id1", "land_id2", "feature"))
features <- features %>% left_join(cf %>% select(land_id1, land_id2, same_source), by=c("land_id1", "land_id2"))

features %>%
  ggplot(aes(x = values.x, y = values.y, colour = same_source)) + geom_point() +
  facet_wrap(~feature, scales="free")


