# Master Thesis Dominik Cramer

### Measuring the Significance of Political Elites
### Simon Munzert


# set working directory
setwd("/Users/DominikCramer/Desktop/master-thesis/mtdc-mp-analysis/")

## load packages and functions -------------------------------
source("mtdc_packages.r")
source("mtdc_functions.r")


source_folder <- "../mtdc-mp-analysis/articles/"
monthly_folders <- list.files(source_folder, full.names = TRUE)
size_df <- data.frame("article" = character(), stringsAsFactors = FALSE)

# this for loop takes lizerally 8 seconds for 160 months
for (m in seq_along(monthly_folders)) {
  ## import article data -------------------
  files <- list.files(monthly_folders[m], full.names = TRUE)
  size <- numeric(length(files))
  for (i in seq_along(files)) {
    size[i] <- file.size(files[i]) # measuring file size in bytes
  }
  article <- str_replace(basename(files), ".html$", "")
  size_m <- data.frame(article, size)
  size_df <- merge(size_df, size_m, by = "article", all = TRUE) %>% 
    rename(!!basename(monthly_folders[m])  := "size")
  if (m%%10==0) { print(paste0(m, " months"))}
  # if (m == 8) {
  #   break
  # }
}
select(size_df, "article", "2023-03") %>% View()

save(size_df, file = "../mtdc-mp-analysis/size_df.RData")

