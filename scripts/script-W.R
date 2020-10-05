library(dplyr) 

# smoothed p_cases and CI:
# source("smooth_column.R")
# smooth_param <- 25

#######

# responses_path <- "../data/aggregate/"
# data_path <- "../data/common-data/regions-tree-population.csv"
estimates_path <- "../data/estimates-W/"

responses_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/aggregate/"
data_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/regions-tree-population.csv"
# estimates_path <- "./estimates-W/"

ci_level <- 0.95
max_ratio <- 1/3
num_responses <- 30
W <- 30

#with recent cases
provincial_regional_estimate_w_only <- function(countrycode = "ES",
                                                province = T,
                                                district = F,
                                                # W = 15,
                                                # max_ratio = .3,
                                                provinces_and_codes = read.csv(data_path),
                                                write_summary_file = T,
                                                write_daily_file = T){
  
  cat(paste0("::- script-W: Generating region based estimates for ", countrycode, "\n"))
  dt <- read.csv(paste0(responses_path, countrycode, "-aggregate.csv"), as.is = T)
  dt_region <- provinces_and_codes[provinces_and_codes$countrycode == countrycode, ]
  names(dt) <- tolower(names(dt))
  dt <- dt[, c("timestamp","region","reach","cases", "recentcases", "iso.3166.1.a2", "iso.3166.2", "cookie")]
  dt$date <- substr(dt$timestamp, 1, 10)
  
  # outlier detection
  dt <- dt[!is.na(dt$reach),]
  reach_cutoff <- boxplot.stats(dt$reach)$stats[5] # changed cutoff to upper fence
  dt <- dt[dt$reach <= reach_cutoff, ]
  
  dt$ratio <- dt$cases/dt$reach    # remove outlier based on 0.3
  dt <- dt[is.finite(dt$ratio), ]  # discard cases with zero reach
  dt <- dt[dt$ratio <= max_ratio, ]
  
  #change region name to province name for single-province regions
  #dt <- change_region_province(dt)
  dt$iso.3166.2[dt$iso.3166.2=="ESAS"] <- "ESO"
  dt$iso.3166.2[dt$iso.3166.2=="ESCB"] <- "ESS"
  dt$iso.3166.2[dt$iso.3166.2=="ESIB"] <- "ESPM"
  dt$iso.3166.2[dt$iso.3166.2=="ESMC"] <- "ESMU"
  dt$iso.3166.2[dt$iso.3166.2=="ESMD"] <- "ESM"
  dt$iso.3166.2[dt$iso.3166.2=="ESNC"] <- "ESNA"
  dt$iso.3166.2[dt$iso.3166.2=="ESRI"] <- "ESLO"
  
  dt$iso.3166.2[dt$iso.3166.2 == ""] <- countrycode
  
  # compute provincial estimates
  dates <- unique(dt$date)
  
  
  dt_region2 <- dt_region[, c("countrycode",  "regioncode",   "provincecode", "population")] ## bring autonomous cities code to lowest level
  
  r_c <- r_r <- r_r_recent <-  I_c_p_w_country <- I_c_p_m_country <- I_c_recent_p_w_country <- I_c_recent_p_m_country <- c()
  I_r_p_w_country <- I_r_p_m_country <- I_r_recent_p_w_country <- I_r_recent_p_m_country <- c()
  p_w_country <- p_m_country <- recent_p_w_country <- recent_p_m_country <- c()

  #--------------------------------
  
  dwhole <- data.frame(date=c(),
                       country=c(),
                       region=c(),
                       population=c(),
                       p_cases=c(),
                       p_cases_recent=c())
                   
  #----------------------------------
  
  for (j in dates){
    # get data from the past up to W days earlier
    subcondition <- (as.Date(dt$date) >= (as.Date(j)-(W/2))  & as.Date(dt$date) <= (as.Date(j)+(W/2)) )
    dt_date <- dt[subcondition, ]
    
    #Remove duplicated cookies keeping the most recent response
    dt_date <- dt_date[!duplicated(dt_date$cookie, fromLast=TRUE, incomparables = c("")),]
    
    #cat("working on date: ", j, " using ", nrow(dt_date), " rows\n")
    
    # compute provincia provinces
    if (province == T){
      dtprovs <- na.omit(dt_region2)
      provs <- unique(dtprovs$provincecode)
      p_w_provs <- p_m_provs <- recent_p_w_provs <- recent_p_m_provs <- sumreach_provs <- n_obs_provs <- rep(0, length(provs))
      for (i in seq_along(provs)) {
        provpop <- dtprovs$population[dtprovs$provincecode == provs[i]]
        dt_prov <- dt_date[dt_date$iso.3166.2 == provs[i], ]
        #Keep all the responses of the day or at most num_responses
        nr <- nrow(dt_prov[as.Date(dt_prov$date) == as.Date(j), ])
        dt_prov <- tail(dt_prov, max(num_responses,nr))
        
        if(nrow(dt_prov) != 0){
          p_w_provs[i] <- sum(dt_prov$cases)/sum(dt_prov$reach)
          p_m_provs[i]  <- mean(dt_prov$cases/dt_prov$reach)
          recent_p_w_provs[i] <- ifelse(all(is.na(dt_prov$recentcases)), 
                                        0, sum(dt_prov$recentcases, na.rm = T)/sum(dt_prov$reach[!is.na(dt_prov$recentcases)]))  #should we use the whole sum or partial sum
          recent_p_m_provs[i] <- ifelse(all(is.na(dt_prov$recentcases)), 
                                        0, mean(dt_prov$recentcases/dt_prov$reach, na.rm = T))
          sumreach_provs[i] <- sum(dt_prov$reach)
          n_obs_provs[i] <- nrow(dt_prov)
        }
        # else{
        #   p_w_provs[i] <- p_m_provs[i]  <- recent_p_w_provs[i] <- recent_p_m_provs[i] <- sumreach_provs[i] <- n_obs_provs[i] <- 0
        # }
        
      }
      dtestpropprovs <- data.frame(provincecode = provs,
                                   p_w_provs = p_w_provs,
                                   p_m_provs = p_m_provs,
                                   recent_p_w_provs = recent_p_w_provs,
                                   recent_p_m_provs = recent_p_m_provs,
                                   sumreach_provs  = sumreach_provs,
                                   n_obs_provs = n_obs_provs)
      
      dtprovs <- merge(dtprovs, dtestpropprovs, all = T, by = "provincecode")
    }
    
    # compute regional estimates
    dtregs <- by(dt_region2,
                 list(dt_region2$countrycode, dt_region2$regioncode), # groupby country and region and compute population
                 function(x){
                   data.frame(countrycode = unique(x$countrycode),
                              regioncode = unique(x$regioncode), 
                              population_region = sum(x$population),
                              stringsAsFactors = F)
                 })
    dtregs <- do.call(rbind, dtregs)
    regions <- unique(dtregs$regioncode)
    p_w_regs_only <- p_m_regs_only <- recent_p_w_regs_only <- recent_p_m_regs_only <- 
      sumreach_regs <- n_obs_regs <-  rep(0, length(regions))
    for (k in seq_along(regions)) {
      regpop <- dtregs$population_region[dtregs$regioncode == regions[k]]
      dt_reg <- dt_date[dt_date$iso.3166.2 == regions[k], ]
      #Keep all the responses of the day or at most num_responses
      nr <- nrow(dt_reg[as.Date(dt_reg$date) == as.Date(j), ])
      dt_reg <- tail(dt_reg, max(num_responses,nr))
      
      if(nrow(dt_reg) != 0){
        p_w_regs_only[k] <- sum(dt_reg$cases)/sum(dt_reg$reach)
        p_m_regs_only[k] <- mean(dt_reg$cases/dt_reg$reach)
        recent_p_w_regs_only[k] <- ifelse(all(is.na(dt_reg$recentcases)),
                                          NA, sum(dt_reg$recentcases, na.rm = T)/sum(dt_reg$reach[!is.na(dt_reg$recentcases)]))  # should we use partial sum?
        recent_p_m_regs_only[k] <- ifelse(all(is.na(dt_reg$recentcases)),
                                          NA, mean(dt_reg$recentcases/dt_reg$reach, na.rm = T))
        sumreach_regs[k] <- sum(dt_reg$reach)
        n_obs_regs[k] <- nrow(dt_reg)
      }
      # else{
      #   p_w_regs_only[k] <- p_m_regs_only[k] <- recent_p_w_regs_only[k] <- 
      #     recent_p_m_regs_only[k] <- sumreach_regs[k] <- n_obs_regs[k] <- 0
      # }
      
    }
    
    dtestpropregs <- data.frame(regioncode = regions,
                                p_w_regs_only = p_w_regs_only,
                                p_m_regs_only = p_m_regs_only,
                                recent_p_w_regs_only = recent_p_w_regs_only,
                                recent_p_m_regs_only = recent_p_m_regs_only,
                                sumreach_regs = sumreach_regs,
                                n_obs_regs = n_obs_regs)
    
    dtregs <- merge(dtregs, dtestpropregs, all = T, by = "regioncode")
    
    # aggregate provincia data into regional data
    if(province == T){
      # population variable refer to population at lowest level
      dt_est_prov_reg <- merge(dtprovs, dtregs, all = T, by = c("countrycode", "regioncode")) 
      # go over regions and computed aggregated means
      uregions <- unique(dt_est_prov_reg$regioncode)
      p_w_regs_rhs <-  p_m_regs_rhs <- recent_p_w_regs_rhs <- recent_p_m_regs_rhs <- 
        sumreach_regs_rhs1 <- sumreach_regs_rhs2 <- rep(0, length(uregions))
      for (l in seq_along(uregions)) {
        dt_est_reg <- dt_est_prov_reg[dt_est_prov_reg$regioncode == uregions[l], ]
        dt_est_reg1 <- na.omit(dt_est_reg[, c(1:6, 9:15)])
        weightreg1 <- dt_est_reg1$population/sum(dt_est_reg1$population)
        p_w_regs_rhs[l] <- sum(weightreg1 * dt_est_reg1$p_w_provs)
        p_m_regs_rhs[l] <- sum(weightreg1 * dt_est_reg1$p_m_provs)
        sumreach_regs_rhs1[l] <- sum(dt_est_reg1$sumreach_provs)
        dt_est_reg2 <- na.omit(dt_est_reg[, c(1:4, 7:15)])
        weightreg2 <- dt_est_reg2$population/sum(dt_est_reg2$population)
        recent_p_w_regs_rhs[l] <- sum(weightreg2 * dt_est_reg2$recent_p_w_provs)
        recent_p_m_regs_rhs[l] <- sum(weightreg2 * dt_est_reg2$recent_p_m_provs)
        sumreach_regs_rhs2[l] <- sum(dt_est_reg2$sumreach_provs)
      }
      
      dt_regs_rhs <- data.frame(regioncode = unique(dt_est_prov_reg$regioncode),
                                p_w_regs_rhs = p_w_regs_rhs,
                                p_m_regs_rhs = p_m_regs_rhs,
                                recent_p_w_regs_rhs = recent_p_w_regs_rhs,
                                recent_p_m_regs_rhs = recent_p_m_regs_rhs,
                                sumreach_regs_rhs1 = sumreach_regs_rhs1,
                                sumreach_regs_rhs2 = sumreach_regs_rhs2)
      
      dt_est_prov_reg <- merge(dt_est_prov_reg, dt_regs_rhs, by = c("regioncode"))
      
      dt_est_prov_reg$p_w_regs <- ((dt_est_prov_reg$sumreach_regs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs1)) *
                                     dt_est_prov_reg$p_w_regs_only) + ((dt_est_prov_reg$sumreach_regs_rhs1/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs1)) *
                                                                         dt_est_prov_reg$p_w_regs_rhs)
      
      dt_est_prov_reg$p_m_regs <- ((dt_est_prov_reg$sumreach_regs/(dt_est_prov_reg$sumreach_regs + dt_est_prov_reg$sumreach_regs_rhs1)) *
                                     dt_est_prov_reg$p_m_regs_only) + ((dt_est_prov_reg$sumreach_regs_rhs1/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs1)) *
                                                                         dt_est_prov_reg$p_m_regs_rhs)
      
      dt_est_prov_reg$recent_p_w_regs <- ((dt_est_prov_reg$sumreach_regs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs2)) *
                                            dt_est_prov_reg$recent_p_w_regs_only) + ((dt_est_prov_reg$sumreach_regs_rhs2/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs2)) *
                                                                                       dt_est_prov_reg$recent_p_w_regs_rhs)
      
      dt_est_prov_reg$recent_p_m_regs <- ((dt_est_prov_reg$sumreach_regs/(dt_est_prov_reg$sumreach_regs + dt_est_prov_reg$sumreach_regs_rhs2)) *
                                            dt_est_prov_reg$recent_p_m_regs_only) + ((dt_est_prov_reg$sumreach_regs_rhs2/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs2)) *
                                                                                       dt_est_prov_reg$recent_p_m_regs_rhs)
      
      
    }else{
      dt_est_prov_reg <- dtregs
      dt_est_prov_reg$p_w_regs <- dt_est_prov_reg$p_w_regs_only
      dt_est_prov_reg$p_m_regs <- dt_est_prov_reg$p_m_regs_only
      dt_est_prov_reg$recent_p_w_regs <- dt_est_prov_reg$recent_p_w_regs_only
      dt_est_prov_reg$recent_p_m_regs <- dt_est_prov_reg$recent_p_m_regs_only
      
    }
    
    ## aggregate regional estimates into provinvial estimates
    dt_country <- dt_date[dt_date$iso.3166.2 == countrycode, ]
    #Keep all the responses of the day or at most num_responses
    nr <- nrow(dt_country[as.Date(dt_country$date) == as.Date(j), ])
    dt_country <- tail(dt_country, max(num_responses,nr))
    
    p_w_country_only <- p_m_country_only <- recent_p_w_country_only <- recent_p_m_country_only <- 
      sumreach_country <- n_obs_country <- 0
    if(nrow(dt_country) != 0){
      p_w_country_only <- sum(dt_country$cases)/sum(dt_country$reach)
      p_m_country_only <- mean(dt_country$cases/dt_country$reach)
      recent_p_w_country_only <- ifelse(all(is.na(dt_country$recentcases)),
                                        NA, sum(dt_country$recentcases, na.rm = T)/sum(dt_country$reach[!is.na(dt_country$recentcases)]))
      recent_p_m_country_only <- ifelse(all(is.na(dt_country$recentcases)),
                                        NA, mean(dt_country$recentcases/dt_country$reach, na.rm = T))
      sumreach_country <- sum(dt_country$reach) 
      n_obs_country <- nrow(dt_country)
    }
    # else{
    #   p_w_country_only <- p_m_country_only <- recent_p_w_country_only <- recent_p_m_country_only <-
    #     sumreach_country <- n_obs_country <- 0
    # }
    
    dt_est_reg_count <- by(dt_est_prov_reg,
                           list(dt_est_prov_reg$countrycode, dt_est_prov_reg$regioncode), # groupby country and region and compute population
                           function(x){
                             data.frame(countrycode = unique(x$countrycode),
                                        regioncode = unique(x$regioncode), 
                                        population_region = unique(x$population_region),
                                        p_w_regs = unique(x$p_w_regs),
                                        p_m_regs = unique(x$p_m_regs),
                                        recent_p_w_regs = unique(x$recent_p_w_regs),
                                        recent_p_m_regs = unique(x$recent_p_m_regs),
                                        sumreach_regs = unique(x$sumreach_regs),
                                        stringsAsFactors = F)
                           })
    dt_est_reg_count <- do.call(rbind, dt_est_reg_count)
    dt_est_reg_count1 <- na.omit(dt_est_reg_count[,c(1,2,3,4,5,8)])
    weightcountry1 <- dt_est_reg_count1$population_region/sum(dt_est_reg_count1$population_region)
    p_w_country_rhs <- sum(weightcountry1 * dt_est_reg_count1$p_w_regs)
    p_m_country_rhs <- sum(weightcountry1 * dt_est_reg_count1$p_m_regs)
    dt_est_reg_count2 <- na.omit(dt_est_reg_count[,c(1,2,3,6,7,8)])
    weightcountry2 <- dt_est_reg_count2$population_region/sum(dt_est_reg_count2$population_region)
    recent_p_w_country_rhs <- sum(weightcountry2 * dt_est_reg_count2$recent_p_w_regs)
    recent_p_m_country_rhs <- sum(weightcountry2 * dt_est_reg_count2$recent_p_m_regs)
    sumreach_country_rhs1<-  sum(dt_est_reg_count1$sumreach_regs)
    sumreach_country_rhs2 <-  sum(dt_est_reg_count2$sumreach_regs)
    
    
    
    p_w_counts <- sum(((sumreach_country/(sumreach_country + sumreach_country_rhs1)) * p_w_country_only),  
      ((sumreach_country_rhs1/(sumreach_country + sumreach_country_rhs1)) * p_w_country_rhs), na.rm = T)
    
    p_m_counts <- sum(((sumreach_country/(sumreach_country + sumreach_country_rhs1)) * p_m_country_only),  
      ((sumreach_country_rhs1/(sumreach_country + sumreach_country_rhs1)) * p_m_country_rhs), na.rm = T)
    
    recent_p_w_counts <- sum(((sumreach_country/(sumreach_country + sumreach_country_rhs2)) * recent_p_w_country_only), 
      ((sumreach_country_rhs2/(sumreach_country + sumreach_country_rhs2)) * recent_p_w_country_rhs), na.rm = T)
    
    recent_p_m_counts <- sum(((sumreach_country/(sumreach_country + sumreach_country_rhs2)) * recent_p_m_country_only),  
      ((sumreach_country_rhs2/(sumreach_country + sumreach_country_rhs2)) * recent_p_m_country_rhs), na.rm = T)
    
    
    if (write_daily_file == T){
      dt_est_count <- data.frame(date=j,
                                 countrycode = countrycode,
                                 population_country = sum(dtregs$population_region),
                                 p_w_country_only = p_w_country_only,
                                 # p_m_country_only = p_m_country_only,
                                 recent_p_w_country_only = recent_p_w_country_only,
                                 # recent_p_m_country_only = recent_p_m_country_only,
                                 sumreach_country = sumreach_country,
                                 n_obs_country = n_obs_country,
                                 p_w_country_rhs = p_w_country_rhs,
                                 # p_m_country_rhs = p_m_country_rhs, 
                                 sumreach_country_rhs = sumreach_country_rhs1,
                                 recent_p_w_country_rhs = recent_p_w_country_rhs,
                                 # recent_p_m_country_rhs = recent_p_m_country_rhs, 
                                 recent_sumreach_country_rhs = sumreach_country_rhs2, 
                                 p_w_country =  p_w_counts,
                                 # p_m_country =  p_m_counts,
                                 recent_p_w_country =  recent_p_w_counts
                                 # recent_p_m_country =  recent_p_m_counts
                                 )
      dir.create(paste0(estimates_path, countrycode), showWarnings = F)
      # cat(paste0("::- script-W: Writing the region based daily estimate for ", countrycode, "..\n")) 
      dt_est_prov_reg_country <- merge(dt_est_count, dt_est_prov_reg, all = T, by = "countrycode")
      
      #----------------------------------------------
      write.csv(x = dt_est_prov_reg_country, file = paste0(estimates_path, countrycode, "/", countrycode,
                                                           "-", gsub("/", "_", j), "-estimate.csv"), row.names = FALSE)
      
      if (j == dates[length(dates)]){
        write.csv(x = dt_est_prov_reg_country, file = paste0(estimates_path, countrycode, "/", countrycode,
                                                           "-latest-estimate.csv"), row.names = FALSE)
      }
      
    }

    # Concatenate to dwhole the regional estimates for date j
    dt_est_prov_reg$date <- j
    df_aux <- dt_est_prov_reg[, c("date", "countrycode", "regioncode", "population_region", "p_w_regs", "recent_p_w_regs")]
    
    df_aux %>% rename(country = countrycode, region = regioncode, 
                      population=population_region, p_cases=p_w_regs, 
                      p_cases_recent=recent_p_w_regs) -> df_aux
    df_aux <- unique(df_aux)
    
    dwhole <- rbind(dwhole, df_aux)
    
    r_c <-  c(r_c, sumreach_country)
    r_r <-  c(r_r, sumreach_country_rhs1)
    r_r_recent <-  c(r_r_recent, sumreach_country_rhs2)
    I_c_p_w_country <- c(I_c_p_w_country, p_w_country_only)
    I_c_p_m_country <- c(I_c_p_m_country, p_m_country_only)
    I_c_recent_p_w_country <- c(I_c_recent_p_w_country, recent_p_w_country_only)
    I_c_recent_p_m_country <- c(I_c_recent_p_m_country, recent_p_m_country_only)
    
    I_r_p_w_country <- c(I_r_p_w_country, p_w_country_rhs)
    I_r_p_m_country <- c(I_r_p_m_country, p_m_country_rhs)
    I_r_recent_p_w_country <- c(I_r_recent_p_w_country, recent_p_w_country_rhs)
    I_r_recent_p_m_country <- c(I_r_recent_p_m_country , recent_p_m_country_rhs)
    
    p_w_country <- c(p_w_country, p_w_counts)
    p_m_country <- c(p_m_country, p_m_counts)
    recent_p_w_country <- c(recent_p_w_country, recent_p_w_counts)
    recent_p_m_country <- c(recent_p_m_country, recent_p_m_counts)
  }

  dir.create(paste0(estimates_path, countrycode), showWarnings = F)
  write.csv(x = dwhole, file = paste0(estimates_path, countrycode, "/", countrycode,
                                      "-region-estimate.csv"), row.names = FALSE)
  
  for (j in regions){
  df_aux <- dwhole[dwhole$region == j,]
  #   df_aux[["p_cases"]][is.na(df_aux[["p_cases"]])] <- 0
  #   if (sum(df_aux$p_cases != 0) > smooth_param) {
  #     df_aux <- smooth_column(df_aux, "p_cases", smooth_param)
  #   }
  #   df_aux[["p_cases_recent"]][is.na(df_aux[["p_cases_recent"]])] <- 0
  #   if (sum(df_aux$p_cases_recent != 0) > smooth_param) {
  #     df_aux <- smooth_column(df_aux, "p_cases_recent", smooth_param)
  #   }
  write.csv(x = df_aux, file = paste0(estimates_path, countrycode, "/", j,
                                      "-region-estimate.csv"), row.names = FALSE)
  }
  
  region_based_estimate <- data.frame(date = dates,
                                      #r_c = r_c,
                                      #r_r = r_r,
                                      #r_r_recent = r_r_recent,
                                      #I_c_p_w_country = I_c_p_w_country,
                                      #I_c_p_m_country = I_c_p_m_country,
                                      #I_c_recent_p_w_country = I_c_recent_p_w_country,
                                      #I_c_recent_p_m_country = I_c_recent_p_m_country,
                                      #I_r_p_w_country = I_r_p_w_country, 
                                      #I_r_p_m_country = I_r_p_m_country,
                                      #I_r_recent_p_w_country = I_r_recent_p_w_country, 
                                      #I_r_recent_p_m_country = I_r_recent_p_m_country,
                                      p_cases = p_w_country,
                                      p_cases_recent = recent_p_w_country
                                      # p_m_country = p_m_country,
                                      # recent_p_m_country = recent_p_m_country
                                      )
  
#---------------------------------------
  
  # region_based_estimate[["p_cases"]][is.na(region_based_estimate[["p_cases"]])] <- 0
  # if (sum(region_based_estimate$p_cases != 0) > smooth_param) {
  #   region_based_estimate <- smooth_column(region_based_estimate, "p_cases", smooth_param)
  # }
  # region_based_estimate[["p_cases_recent"]][is.na(region_based_estimate[["p_cases_recent"]])] <- 0
  # if (sum(region_based_estimate$p_cases_recent != 0) > smooth_param) {
  #   region_based_estimate <- smooth_column(region_based_estimate, "p_cases_recent", smooth_param)
  # }
  

#-------------------------------------------
  
  if(write_summary_file == T){
    dir.create(paste0(estimates_path, "PlotData/"), showWarnings = F)
    cat(paste0("::- script-W: Writing the region based estimate summary for ", countrycode, "..\n"))
    write.csv(region_based_estimate, paste0(estimates_path, "PlotData/",
                                            countrycode, "-estimate.csv"))
  } else{
    return(region_based_estimate)
    }

}


#interest <- c("BR", "CL", "CY", "DE", "EC", "FR", "GB", "PT", "UA", "US")
interest <- c("BR", "US")
dd <- sapply(interest, provincial_regional_estimate_w_only, province = F, write_daily_file = F)

#interest2 <- c("ES", "IT")
interest2 <- c("ES")
dd2 <- sapply(interest2, provincial_regional_estimate_w_only, province = T, write_daily_file = F)

