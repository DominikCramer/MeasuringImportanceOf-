# Master Thesis Dominik Cramer

### Measuring the Importance of Political Elites
### Simon Munzert

# set working directory
setwd("/Users/DominikCramer/Desktop/master-thesis/mtdc-mp-analysis/")

## load packages and functions -------------------------------
source("mtdc_packages.r")
source("mtdc_functions.r")

data <- read_csv2("../mtdc-mp-analysis/full-datasets/full_2023_03_df.csv")

# prepare data for factor analysis
# fa_df <- select(data, "views", "edits", "net_bytes_diff", "size", "pagerank") # alternatively: both (interesting results, clear advantage of two-factor model through impact of short-lived net_bytes_diff (would allow for a short term and a long term dimension))
# fa_df <- select(data, "views", "edits", "net_bytes_diff", "pagerank") # alternatively: only net_bytes_diff (terrible results)
fa_df <- select(data, "views", "edits", "size", "pagerank") # alternatively: only size (great results, one-factor model)
flag_completes <- complete.cases(fa_df)
fa_df <- fa_df[flag_completes,]
fa_df <- fa_df - min(fa_df) + 0.1 # add constant to make all values positive
fa_df <- log(fa_df) # take logarithm
data <- data[flag_completes,]
# Create a data frame with variable labels
variable_labels <- data.frame(varname = c("views", "edits", "size", "pagerank"), label = c("Pageviews", "Edits", "Article size", "Pagerank"))


# export factor variable dataset
write_csv2(fa_df, file = "../mtdc-mp-analysis/one-fa-datasets/fa_one_2023_03_df.csv")

## explore correlation matrix of indicators ---------------------

# plot correlation between variables
pdf(file="../mtdc-mp-analysis/figures/corrplot_one_2023_03.pdf", height=6, width=7, family="URWTimes")
par(oma=c(0,1,0,0) + .2)
par(mar=c(3, 1, 0, 0))
dat <- fa_df
varlabels <- merge(data.frame(varname = names(dat), stringsAsFactors = FALSE), variable_labels, by = "varname", all.x = TRUE, sort = FALSE)
names(dat) <- varlabels$label
corrplot::corrplot(cor(dat), order = "hclust", method = "circle", type = "lower", tl.col='black', tl.cex=.75, p.mat = cor(dat), insig = "p-value", sig.level=-1, color="white", tl.srt = 45, tl.offset = 0.5, addCoef.col = "white", cl.lim = c(0,1)) 
dev.off()


### PERFORM CONVENTIONAL FACTOR ANALYSIS --------------

## one-factor solution ---------------------
fact <- fa(fa_df, nfactors = 1, rotate = "promax", scores = "regression", fm = "ml")
print(fact, cut = 0, digits = 3, sort = TRUE) #results suggest that a one-factor model is a good fit for the data, and that the observed variables are strongly related to a single underlying factor (total variance explained: 0.484)

# generate factor scores
data$fa_importance <- factor.scores(fa_df, fact, method = "tenBerge")$scores[,1] # ten Berge factor scores preserve correlation between factors for oblique solution

# generate ranks of factor scores
data$fa_importance_rank <- rank(-data$fa_importance, ties.method = "min")
dplyr::select(data, article, fa_importance_rank) %>% View() # this ranking by importance makes a lot of sense!


## two-factor solution ---------------------
fact <- fa(fa_df, nfactors = 2, rotate = "promax", scores = "regression", fm = "ml") # maximum likelihood estimation
print(fact, cut = 0, digits = 3, sort = TRUE) # variance explained by the two factors is 53.3%, which is only marginally higher than the 48.4% obtained with the one-factor model; plus, second factor is almost entirely controlled by size

# generate factor scores
data$fa_1 <- factor.scores(fa_df, fact, method = "tenBerge")$scores[,1] # ten Berge factor scores preserve correlation between factors for oblique solution
data$fa_2 <- factor.scores(fa_df, fact, method = "tenBerge")$scores[,2]

# generate ranks of factor scores
data$fa_1_rank <- rank(-data$fa_1, ties.method = "min")
data$fa_2_rank <- rank(-data$fa_2, ties.method = "min")
dplyr::select(data, article, fa_importance_rank, fa_1_rank, fa_2_rank) %>% View() # importance & fa_1 rank make sense, fa_2 rank has some unknown people & many (!) AfD members in top 30 (presumably because it is pretty much only size that plays a role here)


# ## three-factor solution ---------------------
# fact <- fa(fa_df, nfactors = 3, rotate = "promax", scores = "regression", fm = "ml")
# print(fact, cut = 0, digits = 3, sort = TRUE) # at this point, each factor is controlled by one variable (excluding pagerank which is split up fairly evenly)
# 
# # generate factor scores
# data$fa_1 <- factor.scores(fa_df, fact, method = "tenBerge")$scores[,1] 
# data$fa_2 <- factor.scores(fa_df, fact, method = "tenBerge")$scores[,2]
# data$fa_3 <- factor.scores(fa_df, fact, method = "tenBerge")$scores[,3] 
# 
# # generate ranks of factor scores
# data$fa_1_rank <- rank(-data$fa_1, ties.method = "min")
# data$fa_2_rank <- rank(-data$fa_2, ties.method = "min")
# data$fa_3_rank <- rank(-data$fa_3, ties.method = "min")
# dplyr::select(data, article, fa_importance_rank, fa_1_rank, fa_2_rank, fa_3_rank) %>% View() # again, you can really see how the three factors are controlled by views, size, and edits respectively



## COMPARISON one-factor vs. two-factor solution

# scores
data %>% dplyr::select(fa_importance, fa_2, fa_1) %>% cor #high correlation (0.99) between importance and fa_1, fa_2 is the odd one out (0.82 correlated with importance, 0.74 with prominence)
# ranks
data %>% dplyr::select(fa_importance_rank, fa_2_rank, fa_1_rank) %>% cor # similar picture here but fa_2 rank is even less correlated with the others

## one-factor solution mainly picking up the fa_1 dimension (which is fine) 
cor(data$fa_importance, data$fa_1) # (0.99)
cor(data$fa_importance, data$fa_2) # (0.82)


# ## two-factor vs. three-factor solution
# data %>% dplyr::select(fa_2, fa_1, fa_1, fa_2, fa_3) %>% cor
# # ranks
# data %>% dplyr::select(fa_2_rank, fa_1_rank, fa_1_rank, fa_2_rank, fa_3_rank) %>% cor


## explore dimensionality ---------------------

# scree plot to determine the adequate number of dimensions
pdf(file="../mtdc-mp-analysis/figures/screeplot.pdf", height=3, width=6, family="URWTimes")
par(oma=c(0,0,0,0) + .7)
par(mar=c(4, 4, 0, 0))
fa.parallel.refined(fa_df, fa = "fa", ylabel = "Eigenvalue", xlabel = "Factor number", main = "", sim = FALSE, show.legend = FALSE, fm = "ml")
dev.off() #extremely clear: only one factor has significant eigenvalue -> one factor model is good!



# proportion of variance explained by different solutions; BIC
fact1 <- fa(fa_df, nfactors = 1, rotate = "promax", scores = "regression", fm = "ml")
fact1$Vaccounted
fact1$Vaccounted[2,1] # one-factor model explains 48.4% of variance (not too bad)
fact1$BIC # 3.5; hard to interpret since is can only be interpreted relative to the BICs of other models

fact2 <- fa(fa_df, nfactors = 2, rotate = "promax", scores = "regression", fm = "ml")
fact2$Vaccounted
fact2$Vaccounted[3,2] # two-factor model explains 57.9% of variance
fact2$BIC #  this does not return a value

fact3 <- fa(fa_df, nfactors = 3, rotate = "promax", scores = "regression", fm = "ml")
fact3$Vaccounted
fact3$Vaccounted[3,3] # three-factor model explains 63.6% of variance 
fact3$BIC # again this does not return a value



# ## assemble and export table with explained variance & BIC for the three cases
# variance_explained <- c(fact1$Vaccounted[2,1], fact2$Vaccounted[3,2], fact3$Vaccounted[3,3])
# bic <- c(fact1$BIC, fact2$BIC, fact3$BIC)
# 
# dat <- data.frame(variance_explained, bic)
# names(dat) <- c("Variance explained", "BIC")
# head(dat)
# 
# rownames(dat) <- c("1 factor", "2 factors", "3 factors")
# cols_align <- c("l", rep("r", ncol(dat)))
# 
# print(xtable(dat, align = cols_align, digits = 2, caption = "Model fit statistics for various factor specifications.\\label{tab:modelfit}"), booktabs = TRUE, size = "normalsize", caption.placement = "top", table.placement = "h!t", include.rownames=TRUE, include.colnames = TRUE, sanitize.text.function = identity, file = "../figures/modelfit-factors.tex")

## Wilcoxon Rank Sum test for two-factor solution ---------------------------
# wilcox.test(data$fa_2, data$fa_1,
#             alternative = "two.sided",
#             paired = FALSE)
# wilcox.test(data$fa_importance, data$fa_1,
#             alternative = "two.sided",
#             paired = FALSE)


### BAYESIAN FACTOR ANALYSIS --------
set.seed(123)
dat <- fa_df
N = nrow(dat)
J = ncol(dat)
for_jags <- list(y = dat,       # data
                 N = nrow(dat), # number of observations
                 J = ncol(dat) # number of indicators
#                 halfJ = 2      # split point for items 
)

## two-factor model estimation
if(file.exists("../mtdc-mp-analysis/onefactorMCMC.rda")){
  load("../mtdc-mp-analysis/onefactorMCMC.rda")
} else {
  factor_mcmc <- jags.parallel(model.file = "onefactorMCMCcor.jag", 
                               data = for_jags, 
                               parameters.to.save = c("lambda1", "factor"),
                               n.chains = 3,
                               n.burnin = 2000,
                               n.iter = 5000)
  save(factor_mcmc, file = "../mtdc-mp-analysis/onefactorMCMC.rda")
}
# plot(factor_mcmc)
# traceplot(factor_mcmc) # careful, this returns 2500 plots (1 per observation)



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
                     paste0("lambda1[", 1:5, "]")) # changing that 7 to a 5 bc I have two variables less than Simon

# get order of variables right
factor_mcmc_sum_df <- factor_mcmc_sum_df[parameter_names,]
factor_mcmc_sims_df <- factor_mcmc_sims_df[parameter_names,] # lambda1[5] and lambda2[1] column appear to only contain zeros


# export data
save(factor_mcmc_sims_df, file = "../mtdc-mp-analysis/onefactorMCMCsimsdf.rda")
save(factor_mcmc_sum_df, file = "../mtdc-mp-analysis/onefactorMCMCsumdf.rda")


# # plot Rhat values (Gelman-Rubin statistic; should be near 1, which they are, thankfully)
# pdf(file="../mtdc-mp-analysis/figures/model_rhat.pdf", height=5, width=8, family="URWTimes")
# par(oma=c(0,0,0,0) + .7)
# par(mar=c(4,4,0,0))
# hist(factor_mcmc_sum_df_full$Rhat, main = "", xlab = "Rhat value")
# dev.off()
# 
# 
# # plot traceplots, factor loadings + phi
# traceplot_sims <- factor_mcmc_sims_df[,str_detect(colnames(factor_mcmc_sims_df), "lambda")]
# traceplot_sims <- select(traceplot_sims, -`lambda1[5]`, -`lambda2[1]`)
# 
# pdf(file="../mtdc-mp-analysis/figures/model_traceplots.pdf", height=10, width=7, family="URWTimes")
# par(oma=c(0,0,0,0) + .7)
# par(mar=c(2,2,2,2))
# par(mfrow = c(5, 3))
# for(i in 1:ncol(traceplot_sims)){
#   plot(traceplot_sims[1:500,i],  type = "l", col = "black", main = colnames(traceplot_sims)[i], ylab = "", xlab = "")
#   lines(traceplot_sims[501:1000,i],  type = "l", col = "red")
#   lines(traceplot_sims[1001:1500,i],  type = "l", col = "blue")
# }
# dev.off()

# # plot traceplots, sample of factor scores
# traceplot_sims <- factor_mcmc_sims_df[,str_detect(colnames(factor_mcmc_sims_df), "factor")]
# 
# set.seed(123)
# samples <- sample(1:2596, 15) %>% sort
# samples2 <- 2596 + samples
# traceplot_sims <- traceplot_sims[,c(samples, samples2)]
# 
# pdf(file="../mtdc-mp-analysis/figures/model_traceplots2.pdf", height=10, width=7, family="URWTimes")
# par(oma=c(0,0,0,0) + .7)
# par(mar=c(2,2,2,2))
# par(mfrow = c(5, 3))
# for(i in 1:ncol(traceplot_sims)){
#   plot(traceplot_sims[1:500,i],  type = "l", col = "black", main = colnames(traceplot_sims)[i], ylab = "", xlab = "")
#   lines(traceplot_sims[501:1000,i],  type = "l", col = "red")
#   lines(traceplot_sims[1001:1500,i],  type = "l", col = "blue")
# }
# dev.off()

### POSTESTIMATION --------

# load estimates
load("../mtdc-mp-analysis/onefactorMCMCsimsdf.rda")
load("../mtdc-mp-analysis/onefactorMCMCsumdf.rda")

# build result vectors
loadings <- factor_mcmc_sum_df[str_detect(rownames(factor_mcmc_sum_df), "lambda"),] %>% as.data.frame(stringsAsFactors = FALSE)
loadings_df <- data.frame(varname = names(fa_df), f1 = loadings$median[1:4], f1_95lo = loadings$ci95lo[1:4], f1_95hi = loadings$ci95hi[1:4], stringsAsFactors = FALSE)
factor_scores_1 <- factor_mcmc_sum_df[str_detect(rownames(factor_mcmc_sum_df), "factor.+1\\]"),] %>% as.data.frame(stringsAsFactors = FALSE)

# export table of factor loadings
loadings_df_tex <- loadings_df
loadings_df_tex <- merge(loadings_df_tex, variable_labels, by = "varname", all.x = TRUE)
loadings_df_tex
loadings_df_tex <- arrange(loadings_df_tex, desc(f1))
loadings_df_tex$f1_full <- paste0(round(loadings_df_tex$f1, 3), " [", 
                                  round(loadings_df_tex$f1_95lo, 3), ";",
                                  round(loadings_df_tex$f1_95hi, 3), "]")
loadings_df_tex <- dplyr::select(loadings_df_tex, label, f1_full)

colnames(loadings_df_tex) <- c("Variable", "Factor 1 [95\\% CI]")
cols_align <- c("r", "r", rep("c", ncol(loadings_df_tex)-1))
print(xtable(loadings_df_tex, align = cols_align, digits = 3, caption = "Estimated loadings from one-factor model.\\label{tab:factormodel}"), booktabs = TRUE, size = "scriptsize", caption.placement = "top", table.placement = "h!t", include.rownames = FALSE, include.colnames = TRUE, sanitize.text.function = identity, file = "../mtdc-mp-analysis/figures/loadings-onefactormodel.tex")


# generate median Bayesian factor scores
data$bfa_importance <- factor_scores_1$median

# generate rank of Bayesian factor scores
data$bfa_importance_rank <- rank(-data$bfa_importance, ties.method = "min")
dplyr::select(data, article, fa_importance_rank, bfa_importance_rank) %>% View() # conventional factor and bayesian factor seem quite similar

# compare conventional with Bayesian factor scores
plot(data$fa_importance_rank, data$bfa_importance_rank) # clear correlation, but also some divergence
cor(data$fa_importance_rank, data$bfa_importance_rank) # 0.94

# skewness of distributions
skew(data$bfa_importance) # 1.25, right-skewed data (makes sense because there are few extremely important mps and many Hinterbänkler)

## export dataset with factor scores ---------------------
write_csv2(data, file = "../mtdc-mp-analysis/results/fascores_2023_03.csv")



# generate data.frame of loadings
loadings_df_graph <- merge(loadings_df, variable_labels, by = "varname", all.x = TRUE)
loadings_df_graph <- dplyr::select(loadings_df_graph, -varname)
names(loadings_df_graph) <- c("Importance", "f1_95lo", "f1_95hi", "Variable")
loadings_df_graph <- arrange(loadings_df_graph, desc(Importance))
loadings_df_graph$Variable <- factor(loadings_df_graph$Variable, levels = rev(unique(loadings_df_graph$Variable)))

# make data.frame long
loadings.m <- melt(loadings_df_graph, id = "Variable", 
                   measure = c("Importance"), 
                   variable.name = "Factor", value.name = "Loading")
loadings.m$Loading_95lo <- c(loadings_df_graph$f1_95lo)
loadings.m$Loading_95hi <- c(loadings_df_graph$f1_95hi)


# plot
pdf(file="../mtdc-mp-analysis/figures/factor_loadings_2023_03.pdf", height=4, width=7, family="URWTimes")
par(oma=c(0,0,0,0) + .7)
par(mar=c(.5, .5,.5,.5))
ggplot(loadings.m, aes(Variable, abs(Loading), fill = Loading)) + 
  facet_wrap(~ Factor, nrow = 1) + # place the factors in separate facets
  geom_bar(stat = "identity") + # make the bars
  geom_errorbar(aes(ymin=Loading_95lo, ymax=Loading_95hi),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + 
  coord_flip() + # flip the axes so the test names can be horizontal  
  # define the fill color gradient: blue = positive, red = negative
  scale_fill_gradient2(name = "Loading", 
                       high = "#ba0020", mid = "grey", low = "white", 
                       midpoint = 0, guide = "none") +
  ylab("") + 
  xlab("") + # improve y-axis label
  scale_y_continuous(breaks=seq(0,1.1,.2)) +
  theme_bw(base_size=12) # use a black-and-white theme with set font size
dev.off()
