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
data <- read.csv("../data/UMD/Full Survey Data/region/bra_region_full.csv", 
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
country_geoid = "BR"
unique(dt$region_agg)
dt$region <- NA

summary(dt$region_agg)

## Add 'region' code
dt$region[dt$region_agg == "Acre"] <- "BRAC"
dt$region[dt$region_agg == "Alagoas"] <- "BRAL"
dt$region[dt$region_agg == "Amapá"] <- "BRAP"
dt$region[dt$region_agg == "Amazonas"] <- "BRAM"
dt$region[dt$region_agg == "Bahia"] <- "BRBA"
dt$region[dt$region_agg == "Ceará"] <- "BRCE"
dt$region[dt$region_agg == "Distrito Federal"] <- "BRDF"
dt$region[dt$region_agg == "Espírito Santo"] <- "BRES"
dt$region[dt$region_agg == "Goiás"] <- "BRGO"
dt$region[dt$region_agg == "Maranhão"] <- "BRMA"
dt$region[dt$region_agg == "Mato Grosso"] <- "BRMT"
dt$region[dt$region_agg == "Mato Grosso do Sul"] <- "BRMS"
dt$region[dt$region_agg == "Minas Gerais"] <- "BRMG"
dt$region[dt$region_agg == "Paraná"] <- "BRPR"
dt$region[dt$region_agg == "Paraíba"] <- "BRPB"
dt$region[dt$region_agg == "Pará"] <- "BRPA"
dt$region[dt$region_agg == "Pernambuco"] <- "BRPE"
dt$region[dt$region_agg == "Piauí"] <- "BRPI"
dt$region[dt$region_agg == "Rio de Janeiro"] <- "BRRJ"
dt$region[dt$region_agg == "Rio Grande do Norte"] <- "BRRN"
dt$region[dt$region_agg == "Rio Grande do Sul"] <- "BRRS"
dt$region[dt$region_agg == "Rondônia"] <- "BRRO"
dt$region[dt$region_agg == "Roraima"] <- "BRRR"
dt$region[dt$region_agg == "Santa Catarina"] <- "BRSC"
dt$region[dt$region_agg == "Sergipe"] <- "BRSE"
dt$region[dt$region_agg == "São Paulo"] <- "BRSP"
dt$region[dt$region_agg == "Tocantins"] <- "BRTO"

## Get region population
regions_tree <- read.csv(file = "../data/common_data/regions-tree-population.csv", as.is = T) %>%
  filter(countrycode == country_geoid) %>% 
  group_by(regioncode) %>% 
  summarise(population = sum(population))

## Add region population
dt$population <- NA 
for (i in 1:nrow(regions_tree)) {
  dt$population[ dt$region == regions_tree$regioncode[i] ] <- regions_tree$population[i]
}


df_out <- data.frame()
## Batched-estimated-cases
for (region_code in unique(dt$region)){
  
  df_batch_in <- dt %>% filter(region == region_code)
  
  df_batch_in <- batch_effect(df_batch_in, 
                              denom2try = seq(1500, 5000, by = 500))
  
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
  
  write.csv(df_out %>% filter(region == region_code, b_size_denom == 2000),
            paste0("../data/estimates-umd-batches/BR/", region_code  , "_UMD_data.csv"), 
            row.names = FALSE)
  
}

## All regions and selected batch size:
write.csv(df_out %>% filter(b_size_denom == 2000) ,
          "../data/estimates-umd-batches/BR/BR_UMD_data.csv", 
          row.names = FALSE)


## All regions and all batch sizes in a single file:
write.csv(df_out,
          "../data/estimates-umd-batches/BR/BR_UMD_data_by_batch_size.csv", 
          row.names = FALSE)

## Plot by batch size ----

for (batch_size in unique(df_out$b_size_denom)){
  
  df_batch <- df_out %>% filter(b_size_denom == batch_size)
  
  p1 <-  ggplot(data = df_batch, aes(x = date)) +
    geom_point(aes(y = batched_pct_cli), size = 1, colour = "red", alpha = 0.5) +
    geom_line(aes(y = batched_pct_cli_smooth), size = 1, colour = "blue") +
    facet_wrap(~region) +
    theme_bw() + ggtitle(paste0("Batched ( divided by ", batch_size,  ") percentage covid-like-illness"))
  # print(p1)
  ggsave(plot = p1,
         filename =  paste0("../data/estimates-umd-batches/BR/plots_by_batch_size/pct_cli_batch_size_", batch_size,".png"),
         width = 7, height = 5)
  
}


## Plot of specific regions ----

for (region_code in unique(df_out$region)){
  
  df_region <- df_out %>% filter(region == region_code)
  # b_size <- as.numeric(regions_tree[regions_tree$regioncode == region_code, "population"]/10000)
  
  p2 <-  ggplot(data = df_region, aes(x = date)) +
    geom_point(aes(y = batched_pct_cli), size = 1, colour = "red", alpha = 0.5) +
    geom_line(aes(y = batched_pct_cli_smooth), size = 1, colour = "blue") +
    facet_wrap(~b_size_denom) +
    theme_bw() + ggtitle(paste0(region_code, ": Batched percentage covid-like-illness"))
  # print(p2)
  ggsave(plot = p2, 
         filename =  paste0("../data/estimates-umd-batches/BR/plots_by_region/", 
                            region_code, "_pct_cli_by_batch_size.png"), 
         width = 7, height = 5)
  
}
