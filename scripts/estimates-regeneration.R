## script needs file for country and country population.
library(tidyverse)
library(readxl)
library(httr)


# Compute estimates from the CoronaSurveys responses
try(source("script-W.R"), silent = T)
try(source("script-W-smooth.R"), silent = T)  # Uses smooth_column.R
try(source("script-W-plots.R"), silent = T)