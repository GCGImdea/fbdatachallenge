## script needs file for country and country population.
library(tidyverse)
library(readxl)
library(httr)


# Compute estimates from the CoronaSurveys responses
try(source("script-W.R"), silent = T)  # Uses smooth_column.R
