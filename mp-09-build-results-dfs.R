# Master Thesis Dominik Cramer

# set working directory
setwd("/Users/DominikCramer/Desktop/master-thesis/mtdc-mp-analysis/")

## load packages and functions -------------------------------
source("mtdc_packages.r")
source("mtdc_functions.r")

# destination folder
destfolder <- "../mtdc-mp-analysis/results/"

#importing the csv files create one dataframe for fa_importance
files <- list.files(destfolder, full.names = TRUE, pattern = ".+csv$")
full_fa_importance_df <- data.frame(article = character())
for (i in seq_along(files)) {
  df <- read_csv2(paste0(destfolder, basename(files[i])), col_types = cols())
  df <- df %>%
    select(c(article, fa_importance))
  name_m <- basename(files[i]) %>% str_replace(".*fascores_(\\d{4}_\\d{2})\\.csv$", "\\1") %>% gsub("_", "-", .)
  full_fa_importance_df <- merge(full_fa_importance_df, df, by = "article", all = TRUE) %>% 
    rename(!!name_m  := "fa_importance") 
  if (i%%20==0) { print(paste0(i, " imports"))}
  # if (i == 10) {
  #   break
  # }
}

write_csv2(full_fa_importance_df,  file = "../mtdc-mp-analysis/results-dfs/full_fa_importance_df.csv")



#same for fa_importance_rank
files <- list.files(destfolder, full.names = TRUE, pattern = ".+csv$")
full_fa_importance_rank_df <- data.frame(article = character())
for (i in seq_along(files)) {
  df <- read_csv2(paste0(destfolder, basename(files[i])), col_types = cols())
  df <- df %>%
    select(c(article, fa_importance_rank))
  name_m <- basename(files[i]) %>% str_replace(".*fascores_(\\d{4}_\\d{2})\\.csv$", "\\1") %>% gsub("_", "-", .)
  full_fa_importance_rank_df <- merge(full_fa_importance_rank_df, df, by = "article", all = TRUE) %>% 
    rename(!!name_m  := "fa_importance_rank") 
  if (i%%20==0) { print(paste0(i, " imports"))}
  # if (i == 10) {
  #   break
  # }
}

write_csv2(full_fa_importance_rank_df,  file = "../mtdc-mp-analysis/results-dfs/full_fa_importance_rank_df.csv")



#same for bfa_importance
files <- list.files(destfolder, full.names = TRUE, pattern = ".+csv$")
full_bfa_importance_df <- data.frame(article = character())
for (i in seq_along(files)) {
  df <- read_csv2(paste0(destfolder, basename(files[i])), col_types = cols())
  df <- df %>%
    select(c(article, bfa_importance))
  name_m <- basename(files[i]) %>% str_replace(".*fascores_(\\d{4}_\\d{2})\\.csv$", "\\1") %>% gsub("_", "-", .)
  full_bfa_importance_df <- merge(full_bfa_importance_df, df, by = "article", all = TRUE) %>% 
    rename(!!name_m  := "bfa_importance") 
  if (i%%20==0) { print(paste0(i, " imports"))}
  # if (i == 10) {
  #   break
  # }
}

write_csv2(full_bfa_importance_df,  file = "../mtdc-mp-analysis/results-dfs/full_bfa_importance_df.csv")



#same for bfa_importance_rank ## Wanna see me do it again?
files <- list.files(destfolder, full.names = TRUE, pattern = ".+csv$")
full_bfa_importance_rank_df <- data.frame(article = character())
for (i in seq_along(files)) {
  df <- read_csv2(paste0(destfolder, basename(files[i])), col_types = cols())
  df <- df %>%
    select(c(article, bfa_importance_rank))
  name_m <- basename(files[i]) %>% str_replace(".*fascores_(\\d{4}_\\d{2})\\.csv$", "\\1") %>% gsub("_", "-", .)
  full_bfa_importance_rank_df <- merge(full_bfa_importance_rank_df, df, by = "article", all = TRUE) %>% 
    rename(!!name_m  := "bfa_importance_rank") 
  if (i%%20==0) { print(paste0(i, " imports"))}
  # if (i == 10) {
  #   break
  # }
}

write_csv2(full_bfa_importance_rank_df,  file = "../mtdc-mp-analysis/results-dfs/full_bfa_importance_rank_df.csv")
