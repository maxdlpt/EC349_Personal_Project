#-------------------------------------------------------------------------------
#                                 LOAD DATA
#-------------------------------------------------------------------------------
### Locate file paths through current Working Directory and load dataset
library(tidyverse)

wd <- getwd()
print(wd)

path_listings     <- file.path(wd, "data/listings.csv")
df_listings       <- read.csv(path_listings)

clear("path_listings")
clear("wd")
