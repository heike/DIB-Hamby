# Go to https://tsapps.nist.gov/NRBTD/Studies/Search and select the studies
# matching Hamby (2009) Barrel Set X (but download each set on its own;
# they are structured differently!)
# Click Download for each set.
# Unzip downloaded file into NBTRD folder for each study
# The studies are formatted and scanned slightly differently; it is best to
# process them separately.

library(tidyverse)
library(x3ptools)
library(bulletxtrctr) # remotes::install_github("heike/bulletxtrctr")


# ------------------------------------------------------------------------------
# *** Step 1: Read in data and orient scans, convert to microns ****************
# ------------------------------------------------------------------------------
b252 <- read_bullet("NBTRD/Hamby 252 (2009) Barrel/bullets/")
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
  land = gsub(".*Bullet [12A-Z]-([1-6]).*", "\\1", source),
  land_id = sprintf("%s-Br%s-B%s-L%s",study, barrel, bullet, land)
)

b173 <- read_bullet("NBTRD/Hamby 173 (2009) Barrel/bullets/")
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
  land = gsub(".*_land([1-6]).*", "\\1", source)
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
#write.csv(bullets %>% select(-x3p), "meta-info.csv", row.names = FALSE)


# ------------------------------------------------------------------------------
# *** Step 2: Identify a stable crosscut ***************************************
# ------------------------------------------------------------------------------

bullets <- bullets %>% mutate(
  cc = x3p %>% purrr::map_dbl(.f = function(x) x3p_crosscut_optimize(x))
)

# check crosscuts manually
for (i in 1:nrow(bullets)) {
  x3p <- bullets$x3p[[i]]
  x3p <- x3p %>% x3p_add_hline(yintercept = bullets$cc[[i]], size = 10, color = "white")
  x3p %>% image_x3p(file=paste0("images/",bullets$land_id[[i]],".png"))
}
# identify problematic crosscuts
cc_manual <- dir("images/crosscut/")
cc_manual <- gsub(".png", "", cc_manual)
bullets$cc[which(bullets$land_id %in% cc_manual)]
bullets$cc[which(bullets$land_id %in% cc_manual)] <- c(4,3)*bullets$cc[which(bullets$land_id %in% cc_manual)]
for (i in which(bullets$land_id %in% cc_manual)) {
  x3p <- bullets$x3p[[i]]
  x3p <- x3p %>% x3p_add_hline(yintercept = bullets$cc[[i]], size = 10, color = "white")
  x3p %>% image_x3p(file=paste0("images/crosscut/",bullets$land_id[[i]],"-manual.png"))
}


# ------------------------------------------------------------------------------
# *** Step 3: Get measurements at the identified crosscut **********************
# ------------------------------------------------------------------------------
bullets <- bullets %>% mutate(
  ccdata = purrr::map2(.x = x3p, .y = cc, .f = function(x, y) x3p_crosscut(x3p=x, y = y))
)

# ------------------------------------------------------------------------------
# *** Step 4: Identify the grooves between GEA and LEAs on the scans ***********
# ------------------------------------------------------------------------------
bullets <- bullets %>% mutate(
  grooves = ccdata %>% purrr::map(.f = function(x) cc_locate_grooves(x, return_plot = TRUE))
)

# Manually assess the grooves
library(shiny)
grooves <- bullets$grooves

shinyApp(
  ui = fluidPage(
    selectInput("k","Investigate kth plot:", selected = 1,
                choices=1:length(grooves)),
    textOutput("groovelocations"),
    actionButton("confirm", "Confirm"),
    actionButton("save", "Save"),
    plotOutput("groovePlot", click = "plot_click"),
    verbatimTextOutput("info")
  ),

  server = function(input, output, session) {
    output$groovePlot <- renderPlot({
      k <- as.numeric(input$k)
      p <- grooves[[k]]$plot

      p
    })
    output$groovelocations <- renderText({
      paste("Left Groove: ",grooves[[as.numeric(input$k)]]$groove[1],
            " Right Groove: ",grooves[[as.numeric(input$k)]]$groove[2])
    })
    observeEvent(input$confirm,{
      cat(str(input$k))
      updateSelectInput(session, "k","Investigate kth plot:",
                        selected = as.numeric(input$k)+1,
                        choices=1:length(grooves))
    })
    observeEvent(input$save,{
      saveRDS(grooves, file="data/grooves.rda")
      cat("groove data saved\n")
    })

    observeEvent(input$plot_click,{
      k <- as.numeric(input$k)
      xloc <- input$plot_click$x

      gr <- grooves[[k]]$groove
      if (abs(gr[1]-xloc) < abs(gr[2]-xloc)) {
        grooves[[k]]$groove[1] <<- xloc
      } else {
        grooves[[k]]$groove[2] <<- xloc
      }
      output$groovePlot <- renderPlot({
        k <- as.numeric(input$k)
        p <- grooves[[k]]$plot +
          geom_vline(xintercept = grooves[[k]]$groove[1], colour="green") +
          geom_vline(xintercept = grooves[[k]]$groove[2], colour="green")

        p
      })

    })
    output$info <- renderText({
      paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })

  },

  options = list(height = 500)
)

bullets$grooves_pred <- bullets$grooves
bullets$grooves <- grooves


# ------------------------------------------------------------------------------
# *** Step 5: Remove bullet curvature and groove areas, get signature **********
# ------------------------------------------------------------------------------

bullets <- bullets %>% mutate(
  sigs = purrr::map2(.x = ccdata, .y = grooves, .f = function(x, y) cc_get_signature(ccdata=x, grooves = y))
)


# ------------------------------------------------------------------------------
# *** Step 6: Create pairwise comparisons **************************************
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
# # sequential version - use parallelized version below
# comparisons <- comparisons %>% mutate(
#   aligned = purrr::map2(.x = land1, .y = land2, .f = alignHelper)
# )

library(parallel)
numCores <- detectCores()
comp_i <- function(i) {
  cc <- comparisons[i,]
  alignHelper(cc$land1, cc$land2)
}

system.time(
  aligned <- mclapply(1:nrow(comparisons), FUN = comp_i, mc.cores = numCores-1)
)

#      user    system   elapsed
# 42227.436  6808.655  4891.661


# sequential, parallel re-write below
# comparisons <- comparisons %>% mutate(
#   striae = aligned %>% purrr::map(.f = sig_cms_max, span = 75)
# )

striae_i <- function(i) {
  sig_cms_max(aligned[[i]], span=75)
}

system.time(
  striae <- mclapply(1:nrow(comparisons), FUN = striae_i, mc.cores = numCores-1)
)

#      user    system   elapsed
# 37560.232  7273.051  4639.551

comparisons <- tibble(comparisons, aligned, striae)
saveRDS(comparisons, "comparisons.rds")


# # comparisons <- comparisons %>% mutate(
# #   legacy_features = purrr::map(striae, extract_features_all_legacy, resolution = 1.5625)
# # )
#
# legacy_i <- function(i) {
#   extract_features_all_legacy(striae[[i]], resolution = 1.5625)
# }
#
# system.time(
#   legacy_features <- mclapply(1:nrow(comparisons), FUN = legacy_i, mc.cores = numCores-1)
# )
# #     user   system  elapsed
# # 9445.579  392.776  754.253
# comparisons <- tibble(comparisons, legacy_features)
# saveRDS(comparisons, "comparisons.rds")
#
# # comparisons <- comparisons %>% mutate(
# #   legacy_features = purrr::map(striae, extract_features_all_legacy, resolution = 1.5625)
# # )

feature_i <- function(i) {
  extract_features_all(aligned[[i]], striae[[i]], resolution = 1.5625)
}

system.time(
  features <- mclapply(1:nrow(comparisons), FUN = feature_i, mc.cores = numCores-1)
)
#  user   system  elapsed
# 3.220    9.590 1426.357
comparisons <- tibble(comparisons, features)
saveRDS(comparisons, "comparisons.rds")

# ------------------------------------------------------------------------------
# *** Step 7: Include ground truth *********************************************
# ------------------------------------------------------------------------------
meta <- read.csv("meta-info.csv", stringsAsFactors = FALSE)

comparisons <- comparisons %>%
  left_join(meta %>% select(land_id, index_land1=H173_B1_index), by=c("land1"="land_id")) %>%
  left_join(meta %>% select(land_id, index_land2=H173_B1_index), by=c("land2"="land_id"))

comparisons$samesource <- comparisons$index_land1 == comparisons$index_land2
saveRDS(comparisons, "comparisons.rds")


# ------------------------------------------------------------------------------
# *** Step 8: Mark damaged scans ***********************************************
# ------------------------------------------------------------------------------


comparisons <- comparisons %>%
  left_join(meta %>% select(land_id, flagged1=damaged), by=c("land1"="land_id")) %>%
  left_join(meta %>% select(land_id, flagged2=damaged), by=c("land2"="land_id"))
comparisons$damaged <- comparisons$flagged1|comparisons$flagged2
comparisons %>% count(damaged)


# ------------------------------------------------------------------------------
# *** Step 9: Remove duplicate comparisons *************************************
# ------------------------------------------------------------------------------

# use only one comparison of a pair of scans

comparisons <- comparisons %>%
  mutate(
    id = paste(pmin(land1, land2), pmax(land1,land2), sep="|")
  )
saveRDS(comparisons, "comparisons.rds")

features <- comparisons %>% select(-ccf) %>% unnest(features)
doubles <- duplicated(features$id)
features <- features %>% filter(!doubles, !damaged)
features <- features %>% mutate(abs_lag_mm = abs(lag_mm))
write.csv(features %>%
            select(-aligned,-striae, -legacy_features), "data/hamby-comparisons.csv", row.names = FALSE)


