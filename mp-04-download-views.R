# Measuring the Importance of German MPs - 02

# set working directory
setwd("/Users/DominikCramer/Desktop/master-thesis/mtdc-mp-analysis")

## load packages and functions -------------------------------
source("mtdc_packages.r")
source("mtdc_functions.r")

# destination folder
destfolder <- "../mtdc-mp-analysis/views/"

# # using what I already have ---
# views_joined <- left_join(mp_all_df, pviews_df, by = "article") # incorporating the info I already have
# mp_views_missing <- views_joined[is.na(views_joined$"2023-03-01"),] # pageviews for 1035 MPs are still missing

## download page view statistics using wikipediatrend package -----------------------------

# import article links
load("../mtdc-mp-analysis/mp_all_df.RData")
urls <- mp_all_df$wikiurl

# define timespan
date_start <- "2010-01-01" # 1st of January 2010 because prior to this, wikipedia was virtually unknown
date_end <-   "2023-04-30"

# download page view statistics as csv files
flag <- integer()
# this for loop takes approx. 35 minutes for 2631 downloads
for (i in seq_along(urls)) {
  if(!file.exists(paste0(destfolder, basename(urls[i]), ".csv"))) {
    tryCatch({article_pviews <- wp_trend( # using wikipediatrend::wp_trend() to access daily page view statistics prior to July 2015
      page = URLdecode(basename(urls[i])), 
      lang = "de", 
      from = date_start,
      to   = date_end)
    # Aggregate the daily page views by month using dplyr
    article_pviews <- article_pviews %>%
      mutate(date = ymd(date)) %>% # Convert the date column to a date format using lubridate
      group_by(date = floor_date(date, unit = "month")) %>% 
      aggregate(views ~ date, ., sum)
    article_pviews$article <- basename(urls[i])
    },
    error = function(err){
      message('On iteration ',i, ' there was an error: ',err)
      flag <<- c(flag,i)
    })
    try(write.csv(article_pviews, file = paste0(destfolder, basename(urls[i]), ".csv")))
  }
  if (i%%100==0) { print(paste0(i, " downloads"))}
  # if (i == 20) {
  #   break
  #  }
}

length(flag) # This was zero -> apparently, I managed to download at least some page view data for every songle one MP!
save(flag, file = "./flag_pviews.RData")
basename(urls[flag])
file.remove(paste0(destfolder, basename(urls[flag]), ".csv"))



df <- read_csv("/Users/DominikCramer/Desktop/master-thesis/mtdc-mp-analysis/views/Knut_Abraham.csv", col_types = cols())
df_wide <- df %>%
  select(c(date, views, article)) %>%
  pivot_wider(names_from = date, values_from = views)

# --- importing the csv files, pivot_wider, create one dataframe ---
files <- list.files(destfolder, full.names = TRUE, pattern = ".+csv$")
views_df <- data.frame()
# for loop takes approx. 1 min 30 sec for 2630 iterations
for (i in seq_along(files)) {
  df <- read_csv(paste0(destfolder, basename(files[i])), col_types = cols())
  df_wide <- df %>%
    select(c(article, date, views)) %>%
    pivot_wider(names_from = date, values_from = views)
  views_df <- bind_rows(views_df, df_wide)
  if (i%%100==0) { print(paste0(i, " imports"))}
  # if (i == 263) {
  #   break
  # }
}

save(views_df,  file = "../mtdc-mp-analysis/views_df.RData")
