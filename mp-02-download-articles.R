### Measuring the Significance of Political Elites
### Simon Munzert

# set working directory
setwd("/Users/DominikCramer/Desktop/master-thesis/mtdc-mp-analysis/")

## load packages and functions -------------------------------
source("mtdc_packages.r")
source("mtdc_functions.r")

## import Wikidata database
load("../mtdc-mp-analysis/fixed_mp_all_df.RData") # importing the dataframe where redirecting links are fixed
urls <- fixed_mp_all_df$wikiurl

# destination folder
destfolder <- "../mtdc-mp-analysis/articles/2023-01/"

## download articles
flag <- integer()
# this for loop takes approx. 8mins for 2631 downloads
for (i in seq_along(urls)) {
  fname <- paste0(destfolder, basename(urls[i]), ".html")
  if (!file.exists(fname)) {
# rvdir must be set to 'older' because then, we retrieve the most recent version before or on the given date
    link <- paste0("https://de.wikipedia.org/w/api.php?action=query&prop=revisions&titles=", basename(urls[i]), "&rvlimit=1&rvprop=content&rvdir=older&format=json&rvstart=2023-01-15T00:00:00Z")
    response <- GET(link)
    if (response$status_code == 200) {
      html_page <- response$content %>% rawToChar() %>% fromJSON()
      page_id <- names(html_page$query$pages) #extract pageid/oldid
      html_content <- html_page$query$pages[[page_id]]$revisions$`*`
    } else {
      message(paste("Failed to retrieve data for article", basename(urls[i])))
      flag <<- c(flag,i)
    }
    if (!is.null(html_content)) { # this prevents html files to be written if the article did not exist at the time
      write(html_content, file = fname)
    }
  }
  if (i%%250==0) { print(paste0(i, " downloads"))}
  # if (i == 130) {
  #   break
  # }
}

# save(flag, file = "./flag_articles.RData")
# flag
# basename(urls[flag])


# # 1st proof of principle (Olaf Scholz, 15.03.2023)
# link <- "https://de.wikipedia.org/w/api.php?action=query&prop=revisions&titles=Olaf_Scholz&rvlimit=1&rvprop=content&rvdir=older&format=json&rvstart=2023-03-15T00:00:00Z"
# response <- GET(link)
# html_page <- response$content %>% rawToChar() %>% fromJSON()
# html_content <- html_page$query$pages$"293388"$revisions$`*` # Problem: this is hard to generalize
# write(html_content, file = "../mtdc-mp-analysis/articles/2023-03/Olaf_Scholz.html")
# 
# # 2nd proof of principle (genaralized, 15.03.2023)
# link <- "https://de.wikipedia.org/w/api.php?action=query&prop=revisions&titles=Matthias_Helferich&rvlimit=1&rvprop=content&rvdir=older&format=json&rvstart=2023-03-15T00:00:00Z"
# response <- GET(link)
# html_page <- response$content %>% rawToChar() %>% fromJSON()
# page_id <- names(html_page$query$pages) #extract pageid/oldid
# html_content <- html_page$query$pages[[page_id]]$revisions$`*` # insert generalizable page_id variable instead of article-specific value
# write(html_content, file = "../mtdc-mp-analysis/articles/2023-03/Olaf_Scholz.html")