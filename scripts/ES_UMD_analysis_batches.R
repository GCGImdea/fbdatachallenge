## Libraries
library(dplyr)
library(ggplot2)

## Functions needed ----

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
  b1 <- gam(y ~ s(day, k = basis_dim, bs="ps"),
            data=to.smooth)
  
  # save to column "xxx_smooth":
  df_in$y_smooth <- NA
  # df_in[frst_n_zero:nrow(df_in) , "y_smooth"] <- b1$fitted.values
  newd <- data.frame(day = 1:nrow(df_in))
  df_in[ , "y_smooth"] <- predict(b1, newd)
  colnames(df_in)[colnames(df_in) == "y_smooth"] <- paste0(col_s, 
                                                           "_smooth")
  return(df_in)
}

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


## Load data
data <- read.csv("../data/UMD/Full Survey Data/region/esp_region_full.csv", 
                 fileEncoding = "UTF-8")

# data_s <- read.csv("../data/UMD/Full Survey Data/region/smoothed/esp_region_full_smoothed.csv", 
#                  fileEncoding = "UTF-8")

## Filter overall >> overall
# unsmoothed
dt <- data %>% 
  filter(gender=="overall", age_bucket == "overall") %>% 
  select(country_agg, region_agg, date, total_responses, 
         country_region_numeric, pct_cli )

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
dt <- dt %>% filter(date > "2020-05-07")

## Get region population
country_geoid = "ES"
unique(dt$region_agg)
dt$region <- NA

summary(dt$region_agg)

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

df_out <- data.frame()
## Batched-estimated-cases
for (region_code in unique(dt$region)){
  
  df_batch_in <- dt %>% filter(region == region_code)
  
  df_batch_in <- batch_effect(df_batch_in, 
                              denom2try = seq(1000, 5000, by = 500))
  
  df_batch_in$region = region_code
  
  df_out <- rbind(df_out, df_batch_in)
  
}

df_out <- df_out %>% select(date, region, population, total_responses, 
                            pct_cli, pct_cli_smooth, number_cli,
                            batched_pct_cli, batched_pct_cli_smooth,
                            estimate_cli, cum_estimate_cli, number_cli_smooth, 
                            cum_number_cli_smooth, b_size_denom)

## Savings ----

## Each region with selected batch sizes:

for (region_code in unique(df_out$region)){
  
  write.csv(df_out %>% filter(region == region_code, b_size_denom == 1000),
            paste0("../data/estimates-umd-batches/ES/", region_code  , "_UMD_data.csv"), 
            row.names = FALSE)
  
}

## All regions and selected batch size:
write.csv(df_out %>% filter(b_size_denom == 1000) ,
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
    geom_line(aes(y = batched_pct_cli_smooth), size = 1, colour = "blue") +
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
    geom_line(aes(y = batched_pct_cli_smooth), size = 1, colour = "blue") +
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
