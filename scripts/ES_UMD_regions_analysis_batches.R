## Libraries
library(dplyr)
library(ggplot2)
library(stringr)

## Smoothing Function ----

source("smooth_column-v2.R")

## Batching Function ----

batch_effect <- function(df_batch_in, denom2try, col_string_vec){
  ## Build a batched-smoothed version of a variable
  ##
  ## df_batch_in:input data frame
  ## denom2try: denominators to try (also called "d"). batch_size = population / d.
  ## col_string_vec: vector of strings with the names of the columns/variables
  ##                 to be batched and smoothed. 
  
  
  df_out <- data.frame()
  
  for (denom in denom2try) { # different denominators for batch size
    
    df_temp <- df_batch_in
    b_size <- df_temp[1, "population"]/denom
    
    for (col_string in col_string_vec) {
      
      print(paste0("Batching & smoothing --- column: ", 
                   col_string,
                   " ~~ d = ", denom))
      
      # change the name of column to be smoothed:
      colnames(df_temp)[colnames(df_temp) == col_string] = "signal_pct"
      
      # Obtain the actual number from signal_pct
      df_temp$number_signal_pct <- df_temp$total_responses*df_temp$signal_pct/100
      
      # create batched variable
      df_temp$batched_signal_pct <- NA
      
      # cumulative
      df_temp$cum_responses <- cumsum(df_temp$total_responses)
      df_temp$cum_number_signal_pct <- cumsum(df_temp$number_signal_pct)
      
      i_past = 1 # where last batch size was reached
      for (i in 1:(nrow(df_temp)-1)) {
        if (df_temp[i, "cum_responses"] >= b_size) {
          
          df_temp[ceiling((i+i_past)/2), "batched_signal_pct"] <- df_temp[i, "cum_number_signal_pct"]/df_temp[i, "cum_responses"]*100
          
          i_past = i
          
          df_temp[(i+1):nrow(df_temp), "cum_responses"] <- df_temp[(i+1):nrow(df_temp), "cum_responses"] - df_temp[i, "cum_responses"]
          df_temp[(i+1):nrow(df_temp), "cum_number_signal_pct"] <- df_temp[(i+1):nrow(df_temp), "cum_number_signal_pct"] - df_temp[i, "cum_number_signal_pct"]
          
        } # if-cum_responses-greater-b_size
      } # for-rows-df_temp
      
      df_temp <- df_temp %>% select(!c("cum_responses", "cum_number_signal_pct"))
      
      ## DEALING WITH NA's IN THE TAILS 
      # fill first and last NA's of "batched"-version with original signal:
      # 1. non-NA elements in "batched_pct_cli":
      first_non_NA <- min(which(!is.na(df_temp$batched_signal_pct)))
      last_non_NA <- max(which(!is.na(df_temp$batched_signal_pct)))
      # 2. save in temporary var. the bathched signal:
      df_temp$temp_batched_signal_pct <- df_temp$batched_signal_pct
      # 3.assign the values from "signal_pct":
      df_temp[-(first_non_NA:last_non_NA) , "batched_signal_pct"] <- df_temp[-(first_non_NA:last_non_NA) , "signal_pct"]
      
      ## smooth the batched signal:
      df_temp <- smooth_column(df_in = df_temp, 
                               col_s =  "batched_signal_pct", 
                               basis_dim = min(sum(!is.na(df_temp$batched_signal_pct)), 25),
                               link_in = "log",
                               monotone = F)
      
      # retrieve the batched signal and remove temporary:
      df_temp$batched_signal_pct <- df_temp$temp_batched_signal_pct
      df_temp <- df_temp %>% select(!temp_batched_signal_pct)
      
      # change name to column "batched_xxx_smooth":
      colnames(df_temp)[colnames(df_temp) == "batched_signal_pct"] <- 
        paste0("batched_", col_string)
      
      colnames(df_temp)[colnames(df_temp) == "batched_signal_pct_smooth"] <- 
        paste0("batched_", col_string, "_smooth")
      
      colnames(df_temp)[colnames(df_temp) == "batched_signal_pct_smooth_low"] <- 
        paste0("batched_", col_string, "_smooth_low")
      
      colnames(df_temp)[colnames(df_temp) == "batched_signal_pct_smooth_high"] <- 
        paste0("batched_", col_string, "_smooth_high")
      
      ## smooth the original signal:
      df_temp <- smooth_column(df_in = df_temp, 
                               col_s =  "signal_pct", 
                               basis_dim = min(sum(!is.na(df_temp$signal_pct)), 40),
                               link_in = "log",
                               monotone = F)
      
      # change name to column "xxx_smooth":
      colnames(df_temp)[colnames(df_temp) == "signal_pct"] <- 
        paste0(col_string)
      
      colnames(df_temp)[colnames(df_temp) == "signal_pct_smooth"] <- 
        paste0(col_string, "_smooth")
      
      colnames(df_temp)[colnames(df_temp) == "signal_pct_smooth_low"] <- 
        paste0(col_string, "_smooth_low")
      
      colnames(df_temp)[colnames(df_temp) == "signal_pct_smooth_high"] <- 
        paste0(col_string, "_smooth_high")
      
      
    } # loop-col_string_vec
    
    df_temp$b_size_denom <- denom 
    
    df_out <- rbind(df_out, df_temp)
  } # loop-denom_to_try
  
  return(df_out)
}


## Load data ----
data <- read.csv("../data/UMD_updated/Full Survey Data/region/esp_region_full.csv", 
                 fileEncoding = "UTF-8")

## Filter overall >> overall
# unsmoothed
dt <- data %>% 
  filter(gender=="overall", age_bucket == "overall") 

dt$date <- as.Date(dt$date)

## Consider data starting on 2020-05-08
dt <- dt %>% filter(date > "2020-05-07")

# retain only the unweighted percentages
dt <- dt[ , !str_detect(colnames(dt), pattern = "weighted")]

# remove redundant variables
dt <- dt %>% select(!c("X.1", 
                       "X", 
                       "country_agg", 
                       "GID_0", 
                       "GID_1",
                       "gender", 
                       "age_bucket", 
                       "weight_sums"))

# get name of variables representing percentages
all_pct_to_smooth <- colnames(dt)[str_detect(colnames(dt), pattern = "pct")]
all_pct_to_smooth <- all_pct_to_smooth[1:24]
print(all_pct_to_smooth)

## Get region population
country_geoid = "ES"
unique(dt$region_agg)
dt$region <- NA

## Add 'region' code
dt$region[dt$region_agg == "Andalucía"] <- "ESAN"
dt$region[dt$region_agg == "Aragón"] <- "ESAR"
dt$region[dt$region_agg == "Cantabria"] <- "ESCB"
dt$region[dt$region_agg == "Castilla y León"] <- "ESCL"
dt$region[dt$region_agg == "Castilla-La Mancha"] <- "ESCM"
dt$region[dt$region_agg == "Cataluña"] <- "ESCT"
dt$region[dt$region_agg == "Comunidad Foral de Navarra"] <- "ESNC"
dt$region[dt$region_agg == "Comunidad Valenciana"] <- "ESVC"
dt$region[dt$region_agg == "Comunidad de Madrid"] <- "ESMD"
dt$region[dt$region_agg == "Extremadura"] <- "ESEX"
dt$region[dt$region_agg == "Galicia"] <- "ESGA"
dt$region[dt$region_agg == "Islas Baleares"] <- "ESIB"
dt$region[dt$region_agg == "Islas Canarias"] <- "ESCN"
dt$region[dt$region_agg == "La Rioja"] <- "ESRI"
dt$region[dt$region_agg == "País Vasco"] <- "ESPV"
dt$region[dt$region_agg == "Principado de Asturias"] <- "ESAS"
dt$region[dt$region_agg == "Región de Murcia"] <- "ESMC"
dt$region[dt$region_agg == "Ceuta"] <- "ESMC"
dt$region[dt$region_agg == "Melilla"] <- "ESMC"
dt$region[dt$region_agg == "Ceuta y Melilla"] <- "ESCE_ML"

## Get region population
regions_tree <- read.csv(file = "../data/common_data/regions-tree-population.csv", as.is = T) %>%
  filter(countrycode == country_geoid) %>% 
  group_by(regioncode) %>% 
  summarise(population = sum(population))

# add "Ceuta y Melilla"
regions_tree <- rbind(regions_tree, data.frame(regioncode = "ESCE_ML", population = 171264) )

## Add region population
dt$population <- NA 
for (i in 1:nrow(regions_tree)) {
  dt$population[ dt$region == regions_tree$regioncode[i] ] <- regions_tree$population[i]
}

## Remove La Rioja (just 1 obs.)
dt <- dt %>% filter(region != "ESRI")

## Remove Navarra (have too many wholes)
dt <- dt %>% filter(region != "ESNC")

dt$region <- as.factor(dt$region)

## Compute batch-smoothed estimates ----
df_out <- data.frame()
selected_regions <- unique(dt$region)

for (i in 1:length(selected_regions)){
  
  region_code <- selected_regions[i]
  
  df_batch_in <- dt %>% filter(region == region_code)
  
  # remove a repeated date:
  if (length(unique(df_batch_in$date)) != nrow(df_batch_in)) {
    df_batch_in <- df_batch_in %>% distinct()
  }
  
  # date 2020-10-25 is repeated with diff. values:
  temp_ind <- which(df_batch_in$date == as.Date("2020-10-25"))
  if (length(temp_ind) > 1) {
    df_batch_in <- df_batch_in[-temp_ind[-1], ] 
  }
  
  print(paste0("--------- Region: ", 
               region_code, 
               "(",  i, 
               "/", 
               length(selected_regions), 
               ") ---------"))
  
  # print(summary(df_batch_in[ , 
  #                            c("region", 
  #                              "date", 
  #                              "total_responses", 
  #                              "pct_fever")]))
  
  df_batch_in <- batch_effect(df_batch_in,
                              denom2try = c(1000, 2000, 3000, 4000, 5000),
                              col_string_vec = all_pct_to_smooth)

  df_batch_in$region = region_code

  df_out <- rbind(df_out, df_batch_in)
  
}

## Savings ----

## Each region with selected batch sizes:

d_to_save = 2000

for (region_code in unique(df_out$region)){
  
  write.csv(df_out %>% filter(region == region_code, b_size_denom == d_to_save),
            paste0("../data/estimates-umd-batches/ES/", region_code  , "_UMD_data.csv"), 
            row.names = FALSE)
  
}

## All regions and selected batch size:
write.csv(df_out %>% filter(b_size_denom == d_to_save) ,
          "../data/estimates-umd-batches/ES/ES_UMD_data.csv", 
          row.names = FALSE)


## All regions and all batch sizes in a single file:
write.csv(df_out,
          "../data/estimates-umd-batches/ES/ES_UMD_data_by_batch_size.csv", 
          row.names = FALSE)

## Plot by batch size ----

for (batch_size in unique(df_out$b_size_denom)){
  
  df_batch <- df_out %>% filter(b_size_denom == batch_size)
  
  p1 <-  ggplot(data = df_batch, aes(x = date)) +
    geom_point(aes(y = batched_pct_cli), size = 1, colour = "red", alpha = 0.5) +
    geom_line(aes(y = batched_pct_cli_smooth), size = 1, alpha = 0.6, colour = "blue") +
    facet_wrap(~region) +
    theme_bw() + 
    xlab("Date") + ylab("% symptomatic cases") + 
    labs(title = "Spain: batched CSDC CLI (smooth)",
         subtitle = paste0("batch size = population / ", batch_size))
  # print(p1)
  ggsave(plot = p1,
         filename =  paste0("../data/estimates-umd-batches/ES/plots_by_batch_size/ES-pct_cli_batch_size_", batch_size,".png"),
         width = 7, height = 5)
  
}


## Plot of specific regions ----

for (region_code in unique(df_out$region)){
  
  df_region <- df_out %>% filter(region == region_code)
  df_region$d = paste0("d = ", df_region$b_size_denom)
  
  p2 <-  ggplot(data = df_region, aes(x = date)) +
    geom_point(aes(y = batched_pct_cli), size = 1, colour = "red", alpha = 0.5) +
    geom_line(aes(y = batched_pct_cli_smooth), size = 1, alpha = 0.6, colour = "blue") +
    facet_wrap(~d) +
    theme_bw() + 
    xlab("Date") + ylab("% symptomatic cases") + 
    labs(title = paste0(region_code, ": batched CSDC CLI (smooth)"),
         subtitle = "d = population / batch size")
  # print(p2)
  ggsave(plot = p2, 
         filename =  paste0("../data/estimates-umd-batches/ES/plots_by_region/", 
                            region_code, "_pct_cli_by_batch_size.png"), 
         width = 7, height = 5)
  
}
