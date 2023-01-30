library(tidyverse) # version 1.3.2
library(x3ptools) # remotes::install_github("heike/x3ptools@e763e1cd")
library(bulletxtrctr) # remotes::install_github("heike/bulletxtrctr@e8cea5d")
library(purrr) # version 1.0.1
library(future) # version 1.30.0

# ------------------------------------------------------------------------------
# *** Step 1: Download scans for the Hamby Study 173 and 225    ****************
# ------------------------------------------------------------------------------
nbtrd <- "https://tsapps.nist.gov"

h252 <- read.csv("data/Hamby252.csv")
p252 <- "NBTRD/Hamby 252 (2009) Barrel/bullets/"
if (!file.exists(p252)) {
  dir.create(p252, recursive = TRUE)

h252 %>% mutate(
  foo = purrr::map2(.x=file, .y=link, .f = function(.x,.y) {
    download.file(url=paste0(nbtrd, .y),
                  destfile=paste0(p252, .x),
                  mode="wb")
})
)

h173 <- read.csv("data/Hamby173.csv")
p173 <- "NBTRD/Hamby 173 (2009) Barrel/bullets/"
if (!file.exists(p173)) dir.create(p173, recursive = TRUE)

h173 %>% mutate(
  foo = purrr::map2(.x=file, .y=link, .f = function(.x,.y) {
    download.file(url=paste0(nbtrd, .y),
                  destfile=paste0(p173, .x),
                  mode="wb")
  })
)
}

# ------------------------------------------------------------------------------
# *** Step 2: Read in data and orient scans, convert to microns ****************
# ------------------------------------------------------------------------------
b252 <- read_bullet("NBTRD/Hamby 252 (2009) Barrel/bullets")
b252 <- b252 %>%
  mutate(
    x3p = x3p %>% purrr::map(.f = function(x) {
      x <- x %>% x3p_rotate(angle=-90) %>% y_flip_x3p()
      x %>% x3p_m_to_mum()
    })
  )
b252 <- b252 %>% mutate(
  study = "Hamby252",
  barrel = ifelse(grepl("Ukn", source), "Ukn", gsub(".*/Br([0-9]+).*","\\1", source)),
  bullet = gsub(".*Bullet ([A-Z12]).*", "\\1", source),
  land = parse_number(gsub(".*Bullet [12A-Z]-([1-6]).*", "\\1", source)),
  land_id = sprintf("%s-Br%s-B%s-L%s",study, barrel, bullet, land)
)

b173 <- read_bullet("NBTRD/Hamby 173 (2009) Barrel")
b173 <- b173 %>%
  mutate(
    x3p = x3p %>% purrr::map(.f = function(x) {
      x <- x %>% y_flip_x3p()
      x %>% x3p_m_to_mum()
    })
  )
b173 <- b173 %>% mutate(
  study = "Hamby173",
  barrel = gsub(".*/br([A-Z0-9]+)_.*", "\\1", source),
  bullet = gsub(".*_([12])_.*", "\\1", source),
  land = parse_number(gsub(".*_land([1-6]).*", "\\1", source))
)
# warning is expected - 90 lands from unknown barrels
b173 <- b173 %>% mutate(
  bullet = ifelse(is.na(parse_number(barrel)), barrel, bullet),
  barrel = ifelse(is.na(parse_number(barrel)), "Ukn", barrel)
)
b173 <- b173 %>% mutate(
  land_id = sprintf("%s-Br%s-B%s-L%s",study, barrel, bullet, land)
)

bullets <- rbind(b173, b252)


# ------------------------------------------------------------------------------
# *** Step 3: Merge meta info into bullets & compare checksums *****************
# ------------------------------------------------------------------------------

meta <- read.csv("data/meta-info.csv")
bullets <- bullets %>% left_join(meta, by = c("source", "study", "barrel", "bullet", "land", "land_id"))

bullets <- bullets %>% mutate(
  checksum2 = x3p %>% purrr::map_chr(.f = function(x) {
    x3p_show_xml(x ,element = "DataLink.MD5ChecksumPointData")[[1]]
  })
)

if (any(bullets$checksum != bullets$checksum2)) {
  cat("x3p scans have changed. Results will not match previous calculations.\n")
  differences <- which(bullets$checksum != bullets$checksum2)
  cat("Differences were found in the following scans:\n")
  cat(paste(bullets$land_id[differences], collapse="\n"))
}

bullets <- bullets %>% select(-checksum2)

# ------------------------------------------------------------------------------
# *** Step 4: Get measurements at the identified crosscut **********************
# ------------------------------------------------------------------------------

resolution <- bullets$x3p[[1]] %>% x3p_get_scale()

bullets <- bullets %>% mutate(
  ccdata = purrr::map2(.x = x3p, .y = cc,
                       .f = function(x, y) x3p_crosscut(x3p=x, y = y, range = 8*resolution))
)

# summarize ccdata by x - use median of captured values for stability
bullets <- bullets %>% mutate(
  ccdata = ccdata %>% purrr::map(.f = function(cc) {
    cc %>% group_by(x) %>% summarize(
        y = median(y),
        sd_value = sd(value),
        value = median(value)
    )
  })
)

# ------------------------------------------------------------------------------
# *** Step 5: create the groove structure for signature extraction *************
# ------------------------------------------------------------------------------

# combine left and right groove locations into the structure needed
# to get the signature
bullets$grooves <-  purrr::map2(bullets$groove_left, bullets$groove_right,
                       .f = function(left, right) list(groove = c(left, right)))


# ------------------------------------------------------------------------------
# *** Step 6: Remove bullet curvature and groove areas, get signature **********
# ------------------------------------------------------------------------------

bullets <- bullets %>% mutate(
  sigs = purrr::map2(.x = ccdata, .y = grooves,
              .f = function(x, y) cc_get_signature(ccdata=x, grooves = y))
)

potential_problems <- bullets$sigs %>% sapply(FUN=function(sig) {
  max(abs(sig$sig), na.rm=TRUE)
})

# inspect all signatures with high values of 'potential_problems' (above 10 or 15)
idx <- which(potential_problems > 10)
# if those bullets are damaged, there is nothing we can do:
idx <- idx[!meta[idx,"damaged"]]
# check if there are issues we can fix with changes to crosscuts or grooves
# there should not be any issues based on the meta-info.csv file
if (length(idx) > 0) {
  bullets$sigs[[idx[1]]] %>% ggplot(aes(x = x, y=value)) + geom_line()
  bullets$sigs[[idx[1]]] %>% ggplot(aes(x = x, y=sig)) + geom_line()
}
# # example of changes:
# bullets$grooves[[idx[1]]]$groove[2] <- 400
# bullets$grooves[[idx[2]]]$groove[1] <- 300
# bullets$grooves[[idx[2]]]$groove[2] <- 2125
# bullets$grooves[[idx[3]]]$groove[1] <- 250

saveRDS(bullets, "data/bullets.rds")

# ------------------------------------------------------------------------------
# *** Step 7: Create pairwise comparisons **************************************
# ------------------------------------------------------------------------------

lands <- unique(bullets$land_id)
comparisons <- data.frame(
  expand.grid(land1 = lands, land2 = lands), stringsAsFactors = FALSE)


alignHelper <- function(xx, yy) {
  land1 <- bullets$sigs[bullets$land_id == xx][[1]]
  land2 <- bullets$sigs[bullets$land_id == yy][[1]]
  land1$bullet <- "first-land"
  land2$bullet <- "second-land"

  sig_align(land1$sig, land2$sig)
}

# the following code is set up to be evaluated using multiple
# cores in case they are available.
plan(multisession, workers = max(parallelly::availableCores()-1, 1))

# # sequential version
# comparisons <- comparisons %>% mutate(
#   aligned = purrr::map2(.x = land1, .y = land2, .f = alignHelper)
# )

system.time({
comparisons <- comparisons %>% mutate(
   aligned = furrr::future_map2(.x = land1, .y = land2, .f = alignHelper)
)
})

#    user   system  elapsed
# 553.108   93.213 5218.017


# sequential, parallel re-write below
# comparisons <- comparisons %>% mutate(
#   striae = aligned %>% purrr::map(.f = sig_cms_max, span = 75)
# )

system.time({
comparisons <- comparisons %>% mutate(
   striae = aligned %>% furrr::future_map(.f = sig_cms_max, span = 75)
)
})

#    user   system  elapsed
# 480.869   47.256 4456.626

# # sequential, parallel re-write below
# comparisons <- comparisons %>% mutate(
#   features = purrr::map(striae, extract_features_all, resolution = 1.5625)
# )
system.time({
  comparisons <- comparisons %>% mutate(
    features = furrr::future_map2(aligned, striae, extract_features_all, resolution = 1.5625)
  )
})

#    user   system  elapsed
# 278.031  157.864 1419.414
saveRDS(comparisons, "data/comparisons.rds")

# ------------------------------------------------------------------------------
# *** Step 8: Include ground truth *********************************************
# ------------------------------------------------------------------------------
meta <- read.csv("data/meta-info.csv", stringsAsFactors = FALSE)

comparisons <- comparisons %>%
  left_join(meta %>% select(land_id, index_land1=H173_B1_index), by=c("land1"="land_id")) %>%
  left_join(meta %>% select(land_id, index_land2=H173_B1_index), by=c("land2"="land_id"))

comparisons$samesource <- comparisons$index_land1 == comparisons$index_land2

# ------------------------------------------------------------------------------
# *** Step 9: Exclude comparisons from damaged scans and duplicates ************
# ------------------------------------------------------------------------------

comparisons <- comparisons %>%
  left_join(meta %>% select(land_id, flagged1=damaged), by=c("land1"="land_id")) %>%
  left_join(meta %>% select(land_id, flagged2=damaged), by=c("land2"="land_id"))
comparisons$damaged <- comparisons$flagged1|comparisons$flagged2
comparisons %>% count(damaged)

# create a unique identifier for each pair of comparisons
comparisons <- comparisons %>%
  mutate(
    id = paste(pmin(land1, land2), pmax(land1,land2), sep="|")
  )
saveRDS(comparisons, "data/comparisons.rds")

# remove comparisons from duplicates and damaged lands from the training data
features <- comparisons  %>% unnest(features)
doubles <- duplicated(features$id)
features <- features %>% filter(!doubles, !damaged)
#features <- features %>% filter(land1 != land2)
features <- features %>% mutate(abs_lag_mm = abs(lag_mm))
write.csv(features %>%
            select(-aligned,-striae), "data/hamby-comparisons.csv", row.names = FALSE)

