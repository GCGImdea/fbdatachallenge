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
data <- read.csv("../data/UMD_updated/Full Survey Data/region/bra_region_full.csv", 
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
country_geoid = "BR"
unique(dt$region_agg)
dt$region <- NA

## Add 'region' code
dt$region[dt$region_agg == "Acre"] <- "BRAC"
dt$region[dt$region_agg == "Alagoas"] <- "BRAL"
dt$region[dt$region_agg == "Amap?"] <- "BRAP"
dt$region[dt$region_agg == "Amazonas"] <- "BRAM"
dt$region[dt$region_agg == "Bahia"] <- "BRBA"
dt$region[dt$region_agg == "Cear?"] <- "BRCE"
dt$region[dt$region_agg == "Distrito Federal"] <- "BRDF"
dt$region[dt$region_agg == "Esp?rito Santo"] <- "BRES"
dt$region[dt$region_agg == "Goi?s"] <- "BRGO"
dt$region[dt$region_agg == "Maranh?o"] <- "BRMA"
dt$region[dt$region_agg == "Mato Grosso"] <- "BRMT"
dt$region[dt$region_agg == "Mato Grosso do Sul"] <- "BRMS"
dt$region[dt$region_agg == "Minas Gerais"] <- "BRMG"
dt$region[dt$region_agg == "Paran?"] <- "BRPR"
dt$region[dt$region_agg == "Para?ba"] <- "BRPB"
dt$region[dt$region_agg == "Par?"] <- "BRPA"
dt$region[dt$region_agg == "Pernambuco"] <- "BRPE"
dt$region[dt$region_agg == "Piau?"] <- "BRPI"
dt$region[dt$region_agg == "Rio de Janeiro"] <- "BRRJ"
dt$region[dt$region_agg == "Rio Grande do Norte"] <- "BRRN"
dt$region[dt$region_agg == "Rio Grande do Sul"] <- "BRRS"
dt$region[dt$region_agg == "Rond?nia"] <- "BRRO"
dt$region[dt$region_agg == "Roraima"] <- "BRRR"
dt$region[dt$region_agg == "Santa Catarina"] <- "BRSC"
dt$region[dt$region_agg == "Sergipe"] <- "BRSE"
dt$region[dt$region_agg == "S?o Paulo"] <- "BRSP"
dt$region[dt$region_agg == "Tocantins"] <- "BRTO"

## Get region population
regions_tree <- read.csv(file = "../data/common-data/regions-tree-population.csv", as.is = T) %>%
  filter(countrycode == country_geoid) %>% 
  group_by(regioncode) %>% 
  summarise(population = sum(population))

## Add region population
dt$population <- NA 
for (i in 1:nrow(regions_tree)) {
  dt$population[ dt$region == regions_tree$regioncode[i] ] <- regions_tree$population[i]
}

## Remove "BRAC" and "BRRR" (have too many wholes)
dt <- dt %>% filter(region != "BRAC") %>% 
  filter(region != "BRRR") %>% 
  filter(region != "BRPI") %>% 
  filter(region != "BRTO")

dt$region <- as.factor(dt$region)
summary(dt$region)


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
  
  # ## fill in some wholes with NA's
  # df_batch_in <- left_join( data.frame(date = seq.Date( from = min(df_batch_in$date), 
  #                                                       to = max(df_batch_in$date), by = "days")),
  #                           df_batch_in, by = "date")
  
  print(paste0("--------- Region: ", 
               region_code, 
               "(", i, 
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
            paste0("../data/estimates-umd-batches/BR/", region_code  , "_UMD_data.csv"), 
            row.names = FALSE)
  
}

## All regions and selected batch size:
write.csv(df_out %>% filter(b_size_denom == d_to_save) ,
          "../data/estimates-umd-batches/BR/BR_UMD_data.csv", 
          row.names = FALSE)


## All regions and all batch sizes in a single file:
write.csv(df_out,
          "../data/estimates-umd-batches/PlotData/BR_UMD_data_by_batch_size.csv", 
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
    labs(title = "Brazil: batched CSDC CLI (smooth)",
         subtitle = paste0("batch size = population / ", batch_size))
  # print(p1)
  ggsave(plot = p1,
         filename =  paste0("../data/estimates-umd-batches/BR/plots_by_batch_size/BR-pct_cli_batch_size_", batch_size,".png"),
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
         filename =  paste0("../data/estimates-umd-batches/BR/plots_by_region/", 
                            region_code, "_pct_cli_by_batch_size.png"), 
         width = 7, height = 5)
  
}
