# Master Thesis Dominik Cramer
## parts of this script were taken from Simon Munzert (2018): Measuring the Imortance of Political Elites


#devtools::install_github("EllaKaye/BradleyTerryScalable")
#devtools::install_github("saschagobel/legislatoR")

# install packages from CRAN
p_needed <- c("WikidataR",
              "WikipediR",
              "MASS",
              "plyr",
              "dplyr",
              "purrr",
              "tidyr",
              "ISLR",
              "janitor",
              "rvest",
              "stringr",
              "networkD3",
              "igraph",
              "magrittr",
              "stargazer",
              "xtable",
              "doMC",
              "psych",
              "GPArotation",
              "corrplot",
              "reshape2",
              "ggplot2",
              "readr",
              "formattable",
              "htmltools",
              "webshot",
              "ggrepel",
              "haven",
              "fuzzyjoin",
              "pageviews",
              "gtools",
              "jsonlite",
              "lubridate",
              "rjags",
              "coda",
              "runjags",
              "R2jags",
              "ggridges",
#              "Matrix.utils", ## Package ‘Matrix.utils’ was removed from the CRAN repository. Archived on 2022-10-09 as issues were not corrected despite reminders.
#              "BradleyTerryScalable", ## Package ‘BradleyTerryScalable’ was removed from the CRAN repository. Archived on 2020-02-27 as check problems were not corrected despite reminders.
              "scales",
              "ggthemes",
              "broom",
              "legislatoR",
#              "coefplot2", ##This package also seems to have been removed.
#              "rtimes", ## Package ‘rtimes’ was removed from the CRAN repository. Archived on 2019-07-20 as requested by the maintainer.
              "pscl",
              "wikipediatrend",
              "httr",
              "zoo",
              "xml2"
)
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)



