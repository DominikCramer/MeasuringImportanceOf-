# Master Thesis Dominik Cramer

### Measuring the Significance of Political Elites
### Simon Munzert


# set working directory
setwd("/Users/DominikCramer/Desktop/master-thesis/mtdc-mp-analysis/")

## load packages and functions -------------------------------
source("mtdc_packages.r")
source("mtdc_functions.r")


pattern <- "\\[\\[([^]]*\\|)?([^\\]]+)\\]\\]" # pattern to identify links in the weird files obtained from MediaWiki API
source_folder <- "../mtdc-mp-analysis/articles/"
monthly_folders <- list.files(source_folder, full.names = TRUE)
pagerank_df <- data.frame("article" = character(), stringsAsFactors = FALSE)

# this for loop takes around 8 minutes for 160 months
for (m in seq_along(monthly_folders)) {
  ## import article data -------------------
  # folder <- "../mtdc-mp-analysis/articles/2023-03/" # I'm not 100% sure I can simply omit this. Maybe I still have to contruct a path using monthly_folders[m] because elements of the monthly_folders has this slightly different structure than normal file paths.
  files <- list.files(monthly_folders[m], full.names = TRUE) # Again, I am not sure whether simply using monthly_folders[m] will cut it here or whether I need to plug in a path constructed in the line above.
  files_parsed <- lapply(files, read_file) # using read_file() function here because of the weird format the MediaWiki API provided
  article_label <- basename(files) %>% str_replace(".html$", "") %>% gsub("_", " ", .)
  ## identify links between articles -----
  connections <- data.frame(from = NULL, to = NULL)
  for (i in seq_along(files_parsed)) { # this for loop literally takes 5 seconds
    matches <- gregexpr(pattern, files_parsed[[i]], perl = TRUE)
    matches <- unlist(regmatches(files_parsed[[i]], matches))
    pslinks <- gsub("\\[\\[(.*?)(\\|.*?)?\\]\\].*", "\\1", matches)
    links_in_pslinks <- seq_along(files_parsed)[article_label %in% pslinks] # not sure whether links that appear more than once in pslinks will also be accounted for more than once??
    links_in_pslinks <- links_in_pslinks[links_in_pslinks != i] # removes any self-links (i.e., links to the same article)
    connections <- rbind(
      connections,
      data.frame(
        from = rep(i, length(links_in_pslinks)),
        to = links_in_pslinks
      )
    )
  }
  connections[nrow(connections+1),] <- c(length(article_label), length(article_label)-1)   # add artificial edge for last observation to get length of graph right
  names(connections) <- c("from", "to")   ## build connections data frame ---
  graph_wiki <- graph_from_edgelist(as.matrix(connections), directed = TRUE) # build directed graph
  article <- str_replace(basename(files), ".html$", "")
  pagerank <- graph_wiki %>% page.rank(directed = TRUE) %>%  use_series("vector") # compute pagerank
  pagerank_m <- data.frame(article, pagerank = as.numeric(pagerank))
   pagerank_df <- merge(pagerank_df, pagerank_m, by = "article", all = TRUE) %>% 
     rename(!!basename(monthly_folders[m])  := "pagerank")
  if (m%%5==0) { print(paste0(m, " months"))}
  # if (m == 8) {
  #   break
  # }
}


save(pagerank_df, file = "../mtdc-mp-analysis/pagerank_df.RData")

