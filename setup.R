## This file will install and load all the necessary packages

# vector of needed packages
packages <- c("ggplot2", "readr", "sf", "tidyverse", "scales", "dplyr", 
              "tidyr", "data.table", "checkmate", "readxl")

# install packages
if (length(setdiff(packages, rownames(installed.packages()))) != 0 ) {
  install.packages(setdiff(packages, rownames(installed.packages())))
} else {
  print("Necessary packages already installed.")
}


# load packages
lapply(packages, library, character.only = TRUE)