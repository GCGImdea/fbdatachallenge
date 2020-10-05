## Libraries
library(dplyr)
library(ggplot2)

## Smoothing the estimated active cases

smooth_column <- function(df_in, col_s, basis_dim = 15){
  require(mgcv)
  
  # add a number of "day" column:
  to.smooth <- df_in
  to.smooth$day <- 1:nrow(to.smooth)
  
  # change the name of column to be smoothed:
  colnames(to.smooth)[colnames(to.smooth) == col_s] = "y"
  
  # not NA elements to be smoothed:
  ind_not_na <- !is.na(to.smooth$y)
  
  # data to be smoothed:
  to.smooth <- to.smooth[ind_not_na, c("y", "day")]
  
  # Mono-smoothing with scam ----
  b1 <- gam(y ~ s(day, k = basis_dim, bs="ps"), data=to.smooth)
  
  # save to column "xxx_smooth":
  df_in$y_smooth <- NA
  # df_in[frst_n_zero:nrow(df_in) , "y_smooth"] <- b1$fitted.values
  newd <- data.frame(day = 1:nrow(df_in))
  df_in[ , "y_smooth"] <- predict(b1, newd)
  colnames(df_in)[colnames(df_in) == "y_smooth"] <- paste0(col_s, 
                                                           "_smooth")
  return(df_in)
}

## Load data
data <- read.csv("../data/UMD/Full Survey Data/country/esp_country_full_new.csv", 
                 fileEncoding = "UTF-8")

# data_s <- read.csv("../data/UMD/Full Survey Data/region/smoothed/esp_region_full_smoothed.csv", 
#                  fileEncoding = "UTF-8")

## Filter overall >> overall
# unsmoothed
dt <- data %>% 
  filter(gender=="overall", age_bucket == "overall") %>% 
  select(date, total_responses, pct_cli )

## Number of infected in each region
dt$number_cli <- dt$total_responses*dt$pct_cli/100

# # smoothed
# dt_s <- data_s %>% 
#   filter(gender=="overall", age_bucket == "overall") %>% 
#   select(country_agg, region_agg, date, rolling_total_responses, 
#          country_region_numeric, smoothed_pct_cli )
# 
# ## Use smoothed data
# dt <- dt_s
# colnames(dt)[colnames(dt)== "smoothed_pct_cli"] <- "pct_cli"

dt$date <- as.Date(dt$date)

## Consider data starting on 2020-05-08
# dt <- dt %>% filter(date > "2020-05-07")

## Get region population
regions_tree <- read.csv(file = "../data/common_data/regions-tree-population.csv", as.is = T) %>%
  filter(countrycode == "ES") %>% 
  group_by(regioncode) %>% 
  summarise(population = sum(population))

dt$population <- sum(regions_tree$population)

## Batched-estimated-cases ----

batch_effect <- function(df_batch_in, denom2try){
  df_out <- data.frame()
  for (denom in denom2try) { # different denominators for batch size
    
    df_temp <- df_batch_in
    
    b_size <- df_temp[1, "population"]/denom
    
    df_temp$batched_pct_cli <- NA
    
    # df_temp <- df_temp %>%  arrange(desc(date) )
    
    df_temp$cum_responses <- cumsum(df_temp$total_responses)
    df_temp$cum_number_cli <- cumsum(df_temp$number_cli)
    
    i_past = 1 # where last batch size was reached
    for (i in 1:(nrow(df_temp)-1)) {
      if (df_temp[i, "cum_responses"] >= b_size) {
        
        df_temp[ceiling((i+i_past)/2), "batched_pct_cli"] <- df_temp[i, "cum_number_cli"]/df_temp[i, "cum_responses"]*100
        
        i_past = i
        
        df_temp[(i+1):nrow(df_temp), "cum_responses"] <- df_temp[(i+1):nrow(df_temp), "cum_responses"] - df_temp[i, "cum_responses"]
        df_temp[(i+1):nrow(df_temp), "cum_number_cli"] <- df_temp[(i+1):nrow(df_temp), "cum_number_cli"] - df_temp[i, "cum_number_cli"]
        
      } # if-cum_responses-greater-b_size
    } # for-rows-df_temp
    
    df_temp <- smooth_column(df_temp, "batched_pct_cli", 
                             basis_dim = min(sum(!is.na(df_temp$batched_pct_cli)), 25))
    
    df_temp <- smooth_column(df_temp, "pct_cli", 
                             basis_dim = min(sum(!is.na(df_temp$batched_pct_cli)), 40))
    
    df_temp <- df_temp %>% 
      select(date, population, total_responses, pct_cli, pct_cli_smooth,
             number_cli, batched_pct_cli, batched_pct_cli_smooth) %>% 
      mutate(estimate_cli = population*(batched_pct_cli_smooth/100),
             cum_estimate_cli = cumsum(estimate_cli),
             number_cli_smooth = population*(pct_cli_smooth/100),
             cum_number_cli_smooth = cumsum(number_cli_smooth))
    
    df_temp$b_size_denom <- denom 
    
    df_out <- rbind(df_out, df_temp)
  }
  
  return(df_out)
}

df_out <- batch_effect(df_batch_in = dt, 
                       denom2try = seq(1000, 5000, by = 500))

## Savings ----

write.csv(df_out,
          "../data/estimates-umd-batches/ES/ES_UMD_country_data_by_batch_size.csv",
          row.names = FALSE)

# select a single batch size:
df_save <- df_out %>% filter(b_size_denom == 1000)

write.csv(df_save,
          "../data/estimates-umd-batches/ES/ES_UMD_country_data.csv",
          row.names = FALSE)

## Some plots ----
cols <- c("pct_cli_smooth" = "red", "batched_pct_cli_smooth" = "blue")

p1 <- ggplot(data = df_out, aes(x = date, group = b_size_denom)) +
  geom_point(aes(y = pct_cli, colour = "pct_cli_smooth"), alpha = 0.2, size = 2) +
  geom_line(aes(y = pct_cli_smooth, colour = "pct_cli_smooth")) + 
  geom_point(aes(y = batched_pct_cli, colour = "batched_pct_cli_smooth"), alpha = 0.5, size = 2) +
  geom_line(aes(y = batched_pct_cli_smooth, colour = "batched_pct_cli_smooth")) +
  facet_wrap( ~ b_size_denom ) +
  theme_bw() + scale_colour_manual(name = "Legend", values = cols)
p1
ggsave(plot = p1, 
       filename =  "../data/estimates-umd-batches/ES/plots_by_batch_size/ES-country_pct_cli_by_batch_size.png", 
       width = 9, height = 6)


# Animated
library(gganimate)
library(gapminder)

p2 <- ggplot(data = df_out, aes(x = date, y = batched_pct_cli_smooth)) +
  geom_point(colour = "blue") +
  labs(title = 'Batch Size Denominator: {frame_time}',
       y = "Batched pct_cli (Smooth)", x = "date") +
  transition_time(b_size_denom) +
  ease_aes('linear') +
  theme_bw()
p2
anim_save("../data/estimates-umd-batches/ES/plots_by_batch_size/batch_size_effect_ES_country.gif")
