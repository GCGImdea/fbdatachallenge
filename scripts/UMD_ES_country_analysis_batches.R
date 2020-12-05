## Libraries
library(dplyr)
library(ggplot2)
library(stringr)

## Smoothing the estimated active cases

source("smooth_column-v2.R")

## Load data ----
use_unsmoothed_UMD = T # write F if using the UMD's smoothed data

if (use_unsmoothed_UMD) {
  ## Unsmoothed Data
  data <- read.csv("../data/UMD_updated/Full_Survey_Data/country/esp_country_full.csv", 
                   fileEncoding = "UTF-8")
  
  ## Filter overall >> overall
  dt <- data %>% 
    filter(gender=="overall", age_bucket == "overall")
  
  dt$date <- as.Date(dt$date)
  
  ## Consider data starting on 2020-05-08
  # dt <- dt %>% filter(date > "2020-05-07")
}else {
  ## Smoothed Data
  data_s <- read.csv("../data/UMD_updated/Full_Survey_Data/region/smoothed/esp_region_full_smoothed.csv",
                   fileEncoding = "UTF-8")
  
  ## Filter overall >> overall
  dt <- data_s %>%
    filter(gender=="overall", age_bucket == "overall") 
  
  dt$date <- as.Date(dt$date)
  
  ## Consider data starting on 2020-05-08
  # dt <- dt %>% filter(date > "2020-05-07")
}

# retain only the unweighted percentages
dt <- dt[ , !str_detect(colnames(dt), pattern = "weighted")]

# remove redundant variables
dt <- dt %>% select(!c("X.1", 
                       "X", 
                       "country_agg", 
                       "GID_0", 
                       "gender", 
                       "age_bucket", 
                       "weight_sums"))

# remove a repeated date:
if (length(unique(dt$date)) != nrow(dt)) {
  dt <- dt %>% distinct()
  # still: date == 2020-10-25 is repeated, different outcomes :o
  dt <- dt[-193, ]
}


# get name of variables representing percentages
all_pct_to_smooth <- colnames(dt)[str_detect(colnames(dt), pattern = "pct")]
all_pct_to_smooth <- all_pct_to_smooth[1:24]
print(all_pct_to_smooth)

## Get region population ----
regions_tree <- read.csv(file = "../data/common-data/regions-tree-population.csv", as.is = T) %>%
  filter(countrycode == "ES") %>% 
  group_by(regioncode) %>% 
  summarise(population = sum(population))

dt$population <- sum(regions_tree$population)


## Batched-estimated-cases ----

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

# df_out <- batch_effect(df_batch_in = dt, 
#                        denom2try = seq(1000, 5000, by = 500))

df_out <- batch_effect(df_batch_in = dt, 
                       denom2try = c(1000, 2000, 3000, 4000, 5000), 
                       col_string_vec = all_pct_to_smooth )

## Savings ----

write.csv(df_out,
          "../data/estimates-umd-batches/PlotData/ES_UMD_country_data_by_batch_size.csv",
          row.names = FALSE)

# select a single batch size:
df_save <- df_out %>% filter(b_size_denom == 1000)

write.csv(df_save,
          "../data/estimates-umd-batches/country/ES_UMD_country_data.csv",
          row.names = FALSE)

## Some plots ----
df_out$d = paste0("d = ", df_out$b_size_denom)
country = "Spain"

p1 <- ggplot(data = df_out, aes(x = date, colour = Legend)) +
  facet_wrap( ~ d, scales = "free_y" ) +
  geom_point(aes(y = batched_pct_cli, colour = "Batched CSDC CLI"), alpha = 0.5, size = 2) +
  geom_line(aes(y = batched_pct_cli_smooth, colour = "Batched CSDC CLI (smooth)"), 
            linetype = "solid", size =1, alpha = 0.6) +
  geom_ribbon(aes(ymin = batched_pct_cli_smooth_low, 
                  ymax = batched_pct_cli_smooth_high), 
              alpha = 0.1, color = "blue", size = 0.1, fill = "blue") +
  geom_point(aes(y = pct_cli, colour = "CSDC CLI"), alpha = 0.2, size = 2) +
  geom_line(aes(y = pct_cli_smooth, colour = "CSDC CLI (smooth)"), 
            linetype = "solid", size = 1, alpha = 0.6) +
  geom_ribbon(aes(ymin = pct_cli_smooth_low, 
                  ymax = pct_cli_smooth_high), 
              alpha = 0.1, color = "red", size = 0.1, fill = "red") +
  geom_point(aes(y = pct_cli, colour = "d = population / batch size"), alpha = 0) +
  theme_bw(base_size = 15) +
  scale_colour_manual(values = c("blue", "blue", "red", "red", "black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("blank", "solid", "blank", "solid", "blank"),
                        shape = c(1, NA, 1, NA, NA)), nrow = 2 )) +
  xlab("Date") + ylab("% symptomatic cases") + ggtitle(country) +
  theme(legend.position = "bottom")
p1
# aspect_ratio <- 2.5
ggsave(plot = p1, 
       filename =  "../data/estimates-umd-batches/ES/plots_by_batch_size/ES-country_pct_cli_by_batch_size.png", 
       width = 9, height = 6)

# Just pct_cli as dots and unprocessed
p1.1 <- ggplot(data = df_out, aes(x = date, colour = Legend)) +
  facet_wrap( ~ d, scales = "free_y" ) +
  geom_point(aes(y = pct_cli, colour = "CSDC CLI"), alpha = 0.2, size = 2) +
  geom_point(aes(y = pct_cli, colour = "d = population / batch size"), alpha = 0) +
  theme_bw(base_size = 15) +
  scale_colour_manual(values = c("red", "black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("blank", "blank"),
                        shape = c(1, NA)), nrow = 2 )) +
  xlab("Date") + ylab("% symptomatic cases") + ggtitle(country) +
  theme(legend.position = "bottom")
p1.1
ggsave(plot = p1.1, 
       filename =  "../data/estimates-umd-batches/ES/plots_by_batch_size/ES-country_pct_cli_unprocessed_by_batch_size.png", 
       width = 9, height = 6)

# Just pct_cli unprocessed and batched, both as dots
p1.2 <- ggplot(data = df_out, aes(x = date, colour = Legend)) +
  facet_wrap( ~ d, scales = "free_y" ) +
  geom_point(aes(y = batched_pct_cli, colour = "Batched CSDC CLI"), alpha = 0.8, size = 3) +
  geom_point(aes(y = pct_cli, colour = "CSDC CLI"), alpha = 0.2, size = 2) +
  geom_point(aes(y = pct_cli, colour = "d = population / batch size"), alpha = 0) +
  theme_bw(base_size = 15) +
  scale_colour_manual(values = c("blue", "red", "black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("blank", "blank", "blank"),
                        shape = c(1, 1, NA)), nrow = 2 )) +
  xlab("Date") + ylab("% symptomatic cases") + ggtitle(country) +
  theme(legend.position = "bottom")
p1.2
ggsave(plot = p1.2, 
       filename =  "../data/estimates-umd-batches/ES/plots_by_batch_size/ES-country_batched_pct_cli_by_batch_size.png", 
       width = 9, height = 6)

# Just pct_cli unprocessed and batched, both as dots
p1.3 <- ggplot(data = df_out, aes(x = date, colour = Legend)) +
  facet_wrap( ~ d, scales = "free_y" ) +
  geom_point(aes(y = batched_pct_cli, colour = "Batched CSDC CLI"), alpha = 0.8, size = 3) +
  geom_line(aes(y = batched_pct_cli_smooth, colour = "Batched CSDC CLI (smooth)"), 
            linetype = "solid", size =1, alpha = 0.6) +
  geom_ribbon(aes(ymin = batched_pct_cli_smooth_low, 
                  ymax = batched_pct_cli_smooth_high), 
              alpha = 0.1, color = "blue", size = 0.1, fill = "blue") +
  geom_point(aes(y = pct_cli, colour = "CSDC CLI"), alpha = 0.2, size = 2) +
  geom_point(aes(y = pct_cli, colour = "d = population / batch size"), alpha = 0) +
  theme_bw(base_size = 15) +
  scale_colour_manual(values = c("blue", "blue", "red", "black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("blank", "solid", "blank", "blank"),
                        shape = c(1, NA, 1, NA)), nrow = 2 )) +
  xlab("Date") + ylab("% symptomatic cases") + ggtitle(country) +
  theme(legend.position = "bottom")
p1.3
ggsave(plot = p1.3, 
       filename =  "../data/estimates-umd-batches/ES/plots_by_batch_size/ES-country_batched_pct_cli_and_curve_by_batch_size.png", 
       width = 9, height = 6)

p2 <- ggplot(data = df_out, aes(x = date, colour = Legend)) +
  facet_wrap( ~ d, scales = "free_y" ) +
  geom_point(aes(y = pct_anosmia_ageusia, colour = "Anosmia-Ageusia"), alpha = 0.2, size = 2) +
  geom_line(aes(y = batched_pct_anosmia_ageusia_smooth, colour = "Batched Anosmia-Ageusia (smooth)"), 
            linetype = "solid", size = 1, alpha = 0.6) +
  geom_ribbon(aes(ymin = batched_pct_anosmia_ageusia_smooth_low, 
                  ymax = batched_pct_anosmia_ageusia_smooth_high), 
              alpha = 0.1, color = "red", size = 0.1, fill = "red") +
  geom_line(aes(y = batched_pct_cli_smooth, colour = "Batched CLI (smooth)"), 
            linetype = "solid", size =1, alpha = 0.6) +
  geom_ribbon(aes(ymin = batched_pct_cli_smooth_low, 
                  ymax = batched_pct_cli_smooth_high), 
              alpha = 0.1, color = "blue", size = 0.1, fill = "blue") +
  geom_point(aes(y = pct_cli, colour = "CLI"), alpha = 0.5, size = 2) +
  geom_point(aes(y = pct_cli_smooth, colour = "d = population / batch size"), alpha = 0) +
  theme_bw(base_size = 15) +
  scale_colour_manual(values = c("red", "red", "blue", "blue", "black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("blank", "solid", "solid", "blank" , "blank"),
                        shape = c(1, NA, NA, 1, NA)), nrow = 2 )) +
  xlab("Date") + ylab("% cases") + ggtitle(country) +
  theme(legend.position = "bottom")
p2
ggsave(plot = p2, 
       filename =  "../data/estimates-umd-batches/ES/plots_by_batch_size/ES-country_cli_vs_anosmia_by_batch_size.png", 
       width = 9, height = 6)

# just with d = 1000
df_out1000 <- df_out %>% filter(b_size_denom == 1000)
p2.1 <- ggplot(data = df_out1000, aes(x = date, colour = Legend)) +
  facet_wrap( ~ d, scales = "free_y" ) +
  geom_point(aes(y = pct_anosmia_ageusia, colour = "Anosmia-Ageusia"), alpha = 0.2, size = 2) +
  geom_line(aes(y = batched_pct_anosmia_ageusia_smooth, colour = "Batched Anosmia-Ageusia smoothed"), 
            linetype = "solid", size = 1, alpha = 0.6) +
  geom_ribbon(aes(ymin = batched_pct_anosmia_ageusia_smooth_low, 
                  ymax = batched_pct_anosmia_ageusia_smooth_high), 
              alpha = 0.1, color = "red", size = 0.1, fill = "red") +
  geom_line(aes(y = batched_pct_cli_smooth, colour = "Batched CLI smoothed"), 
            linetype = "solid", size =1, alpha = 0.6) +
  geom_ribbon(aes(ymin = batched_pct_cli_smooth_low, 
                  ymax = batched_pct_cli_smooth_high), 
              alpha = 0.1, color = "blue", size = 0.1, fill = "blue") +
  geom_point(aes(y = pct_cli, colour = "CLI"), alpha = 0.5, size = 2) +
  geom_point(aes(y = pct_cli_smooth, colour = "d = population / batch size"), alpha = 0) +
  theme_bw(base_size = 15) +
  scale_colour_manual(values = c("red", "red", "blue", "blue", "black"),
                      guide = guide_legend( override.aes = list(
                        linetype = c("blank", "solid", "solid", "blank" , "blank"),
                        shape = c(1, NA, NA, 1, NA) ), nrow = 2 )) +
  xlab("Date") + ylab("% cases") + ggtitle(country) +
  theme(legend.position = "bottom") 
p2.1
ggsave(plot = p2.1, 
       filename =  "../data/estimates-umd-batches/ES/plots_by_batch_size/ES-country_cli_vs_anosmia_batch_size_1000.png", 
       width = 9, height = 6)

# just with d = 1000
df_out1000 <- df_out %>% filter(b_size_denom == 1000)
p2.2 <- ggplot(data = df_out1000, aes(x = date, colour = Legend)) +
  facet_wrap( ~ d, scales = "free_y" ) +
  geom_point(aes(y = pct_anosmia_ageusia, colour = "  pct_anosmia_ageusia"), alpha = 0.2, size = 2) +
  geom_line(aes(y = batched_pct_anosmia_ageusia_smooth, colour = " Batched pct_anosmia_ageusia smoothed"), 
            linetype = "solid", size = 1, alpha = 0.6) +
  geom_ribbon(aes(ymin = batched_pct_anosmia_ageusia_smooth_low, 
                  ymax = batched_pct_anosmia_ageusia_smooth_high), 
              alpha = 0.1, color = "red", size = 0.1, fill = "red") +
  geom_line(aes(y = pct_anosmia_ageusia_smooth, colour = " pct_anosmia_ageusia smoothed"), 
            linetype = "solid", size =1, alpha = 0.6) +
  geom_ribbon(aes(ymin = pct_anosmia_ageusia_smooth_low, 
                  ymax = pct_anosmia_ageusia_smooth_high), 
              alpha = 0.1, color = "blue", size = 0.1, fill = "blue") +
  #geom_point(aes(y = pct_cli, colour = "CLI"), alpha = 0.5, size = 2) +
  geom_point(aes(y = pct_cli_smooth, colour = "d = population / batch size"), alpha = 0) +
  theme_bw(base_size = 15) +
  scale_colour_manual(values = c("red", "red", "blue", "black"),
                      guide = guide_legend( override.aes = list(
                        linetype = c("blank", "solid", "solid", "blank"),
                        shape = c(1, NA, NA, NA) ), nrow = 2 )) +
  xlab("Date") + ylab("% cases") + ggtitle(country) +
  theme(legend.position = "bottom") 
p2.2
ggsave(plot = p2.2, 
       filename =  "../data/estimates-umd-batches/ES/plots_by_batch_size/ES-country_anosmia_batch_size_1000.png", 
       width = 9, height = 6)


# just with d = 1000
df_out1000 <- df_out %>% filter(b_size_denom == 1000)
p2.3 <- ggplot(data = df_out1000, aes(x = date, colour = Legend)) +
  facet_wrap( ~ d, scales = "free_y" ) +
  geom_point(aes(y = pct_anosmia_ageusia, colour = "pct_anosmia_ageusia"), alpha = 0.2, size = 2) +
  geom_line(aes(y = pct_anosmia_ageusia_smooth, colour = "pct_anosmia_ageusia smoothed"), 
            linetype = "solid", size = 1, alpha = 0.6) +
  geom_ribbon(aes(ymin = pct_anosmia_ageusia_smooth_low, 
                  ymax = pct_anosmia_ageusia_smooth_high), 
              alpha = 0.1, color = "red", size = 0.1, fill = "red") +
  geom_line(aes(y = pct_cli_smooth, colour = "pct_cli smoothed"), 
            linetype = "solid", size =1, alpha = 0.6) +
  geom_ribbon(aes(ymin = pct_cli_smooth_low, 
                  ymax = pct_cli_smooth_high), 
              alpha = 0.1, color = "blue", size = 0.1, fill = "blue") +
  geom_point(aes(y = pct_cli, colour = "pct_cli"), alpha = 0.5, size = 2) +
  #geom_point(aes(y = pct_cli_smooth, colour = "d = population / batch size"), alpha = 0) +
  theme_bw(base_size = 15) +
  scale_colour_manual(values = c("red", "red", "blue", "blue"),
                      guide = guide_legend( override.aes = list(
                        linetype = c("blank", "solid", "solid", "blank" ),
                        shape = c(1, NA, NA, 1) ), nrow = 2 )) +
  xlab("Date") + ylab("% cases") + ggtitle(country) +
  theme(legend.position = "bottom") 
p2.3
ggsave(plot = p2.3, 
       filename =  "../data/estimates-umd-batches/ES/plots_by_batch_size/ES-country_cli_vs_anosmia.png", 
       width = 9, height = 6)




p3 <- ggplot(data = df_out, aes(x = date, colour = Legend)) +
  facet_wrap( ~ d, scales = "free_y" ) +
  geom_point(aes(y = pct_anosmia_ageusia, colour = "Anosmia-Ageusia"), alpha = 0.2, size = 2) +
  geom_line(aes(y = batched_pct_anosmia_ageusia_smooth, colour = "Batched Anosmia-Ageusia (smooth)"), 
            linetype = "solid", size = 1, alpha = 0.6) +
  geom_ribbon(aes(ymin = batched_pct_anosmia_ageusia_smooth_low, 
                  ymax = batched_pct_anosmia_ageusia_smooth_high), 
              alpha = 0.1, color = "red", size = 0.1, fill = "red") +
  geom_line(aes(y = batched_pct_fever_smooth, colour = "Batched Fever (smooth)"), 
            linetype = "solid", size =1, alpha = 0.6) +
  geom_ribbon(aes(ymin = batched_pct_fever_smooth_low, 
                  ymax = batched_pct_fever_smooth_high), 
              alpha = 0.1, color = "blue", size = 0.1, fill = "blue") +
  geom_point(aes(y = pct_fever, colour = "Cases Fever"), alpha = 0.5, size = 2) +
  geom_point(aes(y = pct_fever_smooth, colour = "d = population / batch size"), alpha = 0) +
  theme_bw(base_size = 15) +
  scale_colour_manual(values = c("red", "red", "blue", "blue", "black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("blank", "solid", "solid", "blank" , "blank"),
                        shape = c(1, NA, NA, 1, NA)), nrow = 2 )) +
  xlab("Date") + ylab("% cases") + ggtitle(country) +
  theme(legend.position = "bottom")
p3
ggsave(plot = p3, 
       filename =  "../data/estimates-umd-batches/ES/plots_by_batch_size/ES-country_fever_vs_anosmia_by_batch_size.png", 
       width = 9, height = 6)

p4 <- ggplot(data = df_out, aes(x = date, colour = Legend)) +
  facet_wrap( ~ d, scales = "free_y" ) +
  geom_point(aes(y = pct_anosmia_ageusia, colour = "Anosmia-Ageusia"), alpha = 0.2, size = 2) +
  geom_line(aes(y = batched_pct_anosmia_ageusia_smooth, colour = "Batched Anosmia-Ageusia (smooth)"), 
            linetype = "solid", size = 1, alpha = 0.6) +
  geom_ribbon(aes(ymin = batched_pct_anosmia_ageusia_smooth_low, 
                  ymax = batched_pct_anosmia_ageusia_smooth_high), 
              alpha = 0.1, color = "red", size = 0.1, fill = "red") +
  geom_line(aes(y = batched_pct_ili_smooth, colour = "Batched ILI (smooth)"), 
            linetype = "solid", size =1, alpha = 0.6) +
  geom_ribbon(aes(ymin = batched_pct_ili_smooth_low, 
                  ymax = batched_pct_ili_smooth_high), 
              alpha = 0.1, color = "blue", size = 0.1, fill = "blue") +
  geom_point(aes(y = pct_ili, colour = "Cases ILI"), alpha = 0.5, size = 2) +
  geom_point(aes(y = pct_ili_smooth, colour = "d = population / batch size"), alpha = 0) +
  theme_bw(base_size = 15) +
  scale_colour_manual(values = c("red", "red", "blue", "blue", "black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("blank", "solid", "solid", "blank" , "blank"),
                        shape = c(1, NA, NA, 1, NA)), nrow = 2 )) +
  xlab("Date") + ylab("% cases") + ggtitle(country) +
  theme(legend.position = "bottom")
p4
ggsave(plot = p4, 
       filename =  "../data/estimates-umd-batches/ES/plots_by_batch_size/ES-country_ili_vs_anosmia_by_batch_size.png", 
       width = 9, height = 6)

# # Animated
# library(gganimate)
# library(gapminder)
# 
# p2 <- ggplot(data = df_out, aes(x = date, y = batched_pct_cli_smooth)) +
#   geom_point(colour = "blue") +
#   labs(title = 'Spain batched CSDC CLI (smooth): d = {frame_time}',
#        y = "% symptomatic cases", x = "Date") +
#   transition_time(b_size_denom) +
#   ease_aes('linear') +
#   theme_bw()
# p2
# anim_save("../data/estimates-umd-batches/ES/plots_by_batch_size/batch_size_effect_ES_country.gif")
