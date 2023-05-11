### Measuring the Significance of Political Elites
### Simon Munzert

# set working directory
setwd("/Users/DominikCramer/Desktop/master-thesis/mtdc-mp-analysis/")

## load packages and functions -------------------------------
source("mtdc_packages.r")
source("mtdc_functions.r")

## import Wikidata database
load("../mtdc-mp-analysis/mp_all_df.RData")
urls <- mp_all_df$wikiurl
urls_basenames <- basename(urls)

# destination folder
destfolder <- "../mtdc-mp-analysis/edits-xtools/"

# # define timespan
years <- c("20100101/20110101", "20110101/20120101", "20120101/20130101", "20130101/20140101", "20140101/20150101", "20150101/20160101", "20160101/20170101", "20170101/20180101", "20180101/20190101", "20190101/20200101", "20200101/20210101", "20210101/20220101", "20220101/20230101", "20230101/20230430")

# download monthly edits as csv files
flag <- integer()
# this for loop works but takes approx. 6hrs (3min 37sec for 26 iterations)); started 8.55pm with 57 downloads already done; 10.55pm: 1100 downloads
for (i in seq_along(urls)) {
  if(!file.exists(paste0(destfolder, basename(urls[i]), ".csv"))) {
    article_edits_df <- data.frame()
    for (year in seq_along(years)){
      link <- paste0("https://wikimedia.org/api/rest_v1/metrics/edits/per-page/de.wikipedia.org/", basename(urls[i]), "/all-editor-types/monthly/", years[year])
      response <- GET(link, headers = c("User-Agent" <- "dominikcramer.dc@gmail.com"))
      if (response$status_code == 200) {
        article_edits <- response$content %>% rawToChar() %>% fromJSON() # I changed this line. old version:    article_edits <- fromJSON(rawToChar(response$content))
        article_edits <- article_edits$items$results %>% .[[1]]
        article_edits$article <- basename(urls[i]) # Add new column with article name
        article_edits_df <- bind_rows(article_edits_df, article_edits)
      } else {
        message(paste("Failed to retrieve data for article", basename(urls[i])))
        flag <<- c(flag,i)
      }
      Sys.sleep(0.2) # ensuring I don't make more than 5 requests per second as required
    }
    write.csv(article_edits_df, file = paste0(destfolder,  basename(urls[i]), ".csv"))
  }
  if (i%%25==0) { print(paste0(i, " downloads"))}
  # if (i == 57) {
  #   break
  # }
}

save(flag, file = "../flag_edits.RData")
length(flag)
basename(urls[flag]) # There are three MPs who don't have their own wikipedia article (all Members of the Berliner Abgeordnetenhaus): Alexander_Bertram (AfD), Tom_Cywinski (CDU), and Martin_Sattelkau (CDU, who was errously named Martin Sattelklau in the scraped list on Wikipedia)
file.remove(paste0(destfolder, basename(urls[flag]), ".csv"))

#importing the csv files, pivot_wider, create one dataframe
files <- list.files(destfolder, full.names = TRUE, pattern = ".+csv$")
edits_df <- data.frame()
# for loop takes 90 sec for 2600 iterations
for (i in seq_along(files)) {
  df <- read_csv(paste0(destfolder, basename(files[i])), col_types = cols())
  df_wide <- df %>%
    select(c(article, timestamp, edits)) %>%
    pivot_wider(names_from = timestamp, values_from = edits)
  edits_df <- bind_rows(edits_df, df_wide)
  if (i%%100==0) { print(paste0(i, " imports"))}
  # if (i == 100) {
  #   break
  # }
}

save(edits_df,  file = "../mtdc-mp-analysis/edits_df.RData")
