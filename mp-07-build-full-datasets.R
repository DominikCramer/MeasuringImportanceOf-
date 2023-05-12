# Master Thesis Dominik Cramer


# set working directory
setwd("/Users/DominikCramer/Desktop/master-thesis/mtdc-mp-analysis/")

## load packages and functions -------------------------------
source("mtdc_packages.r")
source("mtdc_functions.r")

## import dataframes with variables ----------
load("../mtdc-mp-analysis/fixed_mp_all_df.RData")
load("../mtdc-mp-analysis/fixed_edits_df.RData")
load("../mtdc-mp-analysis/fixed_net_bytes_diff_df.RData")
load("../mtdc-mp-analysis/fixed_views_df.RData")
load("../mtdc-mp-analysis/size_df.RData")
load("../mtdc-mp-analysis/pagerank_df.RData")

# create vector with months
source_folder <- "../mtdc-mp-analysis/articles/"
months <- list.files(source_folder, full.names = TRUE) %>% basename()

full_datasets <- list()  # Create an empty list to store the data frames

# for loop takes a few seconds for 160 months
for (m in seq_along(months)) {
  full_datasets[[m]] <- left_join(fixed_mp_all_df, select(fixed_views_df, "article", paste0(months[m], "-01")), by = "article") %>% 
    rename(views = paste0(months[m], "-01")) %>% 
    
    left_join(select(fixed_edits_df, "article", paste0(months[m], "-01")), by = "article") %>% 
    rename(edits = paste0(months[m], "-01")) %>% 
    
    left_join(select(fixed_net_bytes_diff_df, "article", paste0(months[m], "-01")), by = "article") %>% 
    rename(net_bytes_diff = paste0(months[m], "-01")) %>% 
    
    left_join(select(size_df, "article", months[m]), by = "article") %>% 
    rename(size = months[m]) %>% 
    
    left_join(select(pagerank_df , "article", months[m]), by = "article") %>% 
    rename(pagerank = months[m])
  # save full dataset for month m
  name_m <- months[m] %>% gsub("-", "_", .)
  write_csv2(full_datasets[[m]], file = paste0("../mtdc-mp-analysis/full-datasets/full_", name_m, "_df.csv"))
  # if (m == 4) {
  #   break
  # }
}

# # create sample df
# full_2023_03_df <- left_join(fixed_mp_all_df, select(fixed_views_df, "article", "2023-03-01"), by = "article") %>% 
#   rename(views = "2023-03-01") %>% 
#   left_join(select(fixed_edits_df, "article", "2023-03-01"), by = "article") %>% 
#   rename(edits = "2023-03-01") %>% 
#   left_join(select(fixed_net_bytes_diff_df, "article", "2023-03-01"), by = "article") %>% 
#   rename(net_bytes_diff = "2023-03-01") %>% 
#   left_join(select(size_df, "article", "2023-03"), by = "article") %>% 
#   rename(size = "2023-03") %>% 
#   left_join(select(pagerank_df , "article", "2023-03"), by = "article") %>% 
#    rename(pagerank = "2023-03")
# 
# save(full_2023_03_df, file = "../mtdc-mp-analysis/full-datasets/full_2023_03_df.RData")
# 
# 
#  # # Create the scatterplot for fun
#  ggplot(data = full_2023_03_df, aes(x = pagerank, y = size, label = article)) +
#    geom_point() +
#    geom_text(nudge_y = 40) +  # move the labels above the points
#    xlab("Pagerank") +
#    ylab("Size") +
#    ggtitle("Pagerank vs. Views Scatterplot")
