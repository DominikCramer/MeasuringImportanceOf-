# Master Thesis Dominik Cramer

### Measuring the Importance of Political Elites
### Simon Munzert

# set working directory
setwd("/Users/DominikCramer/Desktop/master-thesis/mtdc-mp-analysis/")

## load packages and functions -------------------------------
source("mtdc_packages.r")
source("mtdc_functions.r")

months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

# takes 2-3 minutes (run a few of these scripts in parallel one per year)
for (m in seq_along(months)) {
  data <- read_csv2(paste0("../mtdc-mp-analysis/full-datasets/full_2010_", months[m], "_df.csv"), col_types = cols())
  # prepare data for factor analysis
  fa_df <- select(data, "views", "edits", "size", "pagerank")
  flag_completes <- complete.cases(fa_df)
  fa_df <- fa_df[flag_completes,]
  fa_df <- fa_df - min(fa_df) + 0.1 # add constant to make all values positive
  fa_df <- log(fa_df) # take logarithm
  data <- data[flag_completes,]
  # Create a data frame with variable labels
  variable_labels <- data.frame(varname = c("views", "edits", "size", "pagerank"), label = c("Pageviews", "Edits", "Article size", "Pagerank"))
  
  # conventional factor analysis
  fact <- fa(fa_df, nfactors = 1, rotate = "promax", scores = "regression", fm = "ml")
  # generate conventional factor scores
  data$fa_importance <- factor.scores(fa_df, fact, method = "tenBerge")$scores[,1] # ten Berge factor scores preserve correlation between factors for oblique solution
  # generate conventional ranks of factor scores
  data$fa_importance_rank <- rank(-data$fa_importance, ties.method = "min")
  
  ### BAYESIAN FACTOR ANALYSIS --------
  set.seed(123)
  dat <- fa_df
  N = nrow(dat)
  J = ncol(dat)
  for_jags <- list(y = dat,       # data
                   N = nrow(dat), # number of observations
                   J = ncol(dat)) # number of indicators
  
  ## two-factor model estimation
  factor_mcmc <- jags.parallel(model.file = "onefactorMCMCcor.jag", 
                               data = for_jags, 
                               parameters.to.save = c("lambda1", "factor"),
                               n.chains = 3,
                               n.burnin = 2000,
                               n.iter = 5000)
  
  # extract estimates
  factor_mcmc_list <- as.mcmc(factor_mcmc)
  factor_mcmc_sims <- combine.mcmc(factor_mcmc_list)
  factor_mcmc_sims_df <- as.data.frame(factor_mcmc_sims)[c(501:1000,1501:2000, 2501:3000),] # cut off first 500 iterations of each chain (not fully converged yet)
  
  # sumamry statistics on full matrix of simulations
  factor_mcmc_sum_df_full <- factor_mcmc$BUGSoutput$summary %>% as.data.frame(stringsAsFactors = FALSE)
  
  # summary statistics
  factor_mcmc_sum_df <- data.frame(mean = sapply(factor_mcmc_sims_df, mean),
                                   sd = sapply(factor_mcmc_sims_df, sd),
                                   ci95lo = sapply(factor_mcmc_sims_df, quantile, 0.025),
                                   ci95hi = sapply(factor_mcmc_sims_df, quantile, 0.975),
                                   median = sapply(factor_mcmc_sims_df, median),
                                   stringsAsFactors = FALSE)
  
  # construct correct order of variable names (factors!)
  parameter_names <- c("deviance", 
                       paste0("factor[", 1:nrow(dat), ",1]"), 
                       paste0("lambda1[", 1:5, "]"))
  
  # get order of variables right
  factor_mcmc_sum_df <- factor_mcmc_sum_df[parameter_names,]
  factor_mcmc_sims_df <- factor_mcmc_sims_df[parameter_names,] 
  
  # build result vectors
  loadings <- factor_mcmc_sum_df[str_detect(rownames(factor_mcmc_sum_df), "lambda"),] %>% as.data.frame(stringsAsFactors = FALSE)
  loadings_df <- data.frame(varname = names(fa_df), f1 = loadings$median[1:4], f1_95lo = loadings$ci95lo[1:4], f1_95hi = loadings$ci95hi[1:4], stringsAsFactors = FALSE)
  factor_scores_1 <- factor_mcmc_sum_df[str_detect(rownames(factor_mcmc_sum_df), "factor.+1\\]"),] %>% as.data.frame(stringsAsFactors = FALSE)
  
  # generate median Bayesian factor scores
  data$bfa_importance <- factor_scores_1$median
  
  # generate rank of Bayesian factor scores
  data$bfa_importance_rank <- rank(-data$bfa_importance, ties.method = "min")
  
  ## export dataset with factor scores ---------------------
  write_csv2(data, file = paste0("../mtdc-mp-analysis/results/fascores_2010_", months[m], ".csv"))
  if (m%%1==0) { print(paste0(m, " months"))}
}


