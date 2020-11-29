library(dplyr)
library(quantgen) 
library(stringr)

# append shift
my_append_shifts = function(df, shifts) {
  # Make sure that we have a complete record of dates for each geo_value (fill
  # with NAs as necessary)
  # df_all = df %>% group_by(geo_value) %>%
  #   summarize(time_value = seq.Date(as.Date(min(time_value)),
  #                                   as.Date(max(time_value)),
  #                                   by = "day")) %>% ungroup()
  # df = full_join(df, df_all, by = c("geo_value", "time_value"))
  # 
  # # Group by geo value, sort rows by increasing time
  # df = df %>% group_by(geo_value) %>% arrange(time_value) 
  
  # Load over shifts, and add lag value or lead value
  for (shift in shifts) {
    fun = ifelse(shift < 0, lag, lead)
    varname = sprintf("value%+d", shift)
    df = mutate(df, !!varname := fun(value, n = abs(shift)))
  }
  
  # Ungroup and return
  return(ungroup(df))
}

# define transformations
Logit = function(x, a = 0.01) log((x + a) / (1 - x + a))
Sigmd = function(y, a = 0.01) (exp(y) * (1 + a) - a) / (1 + exp(y))
trans = Logit
inv_trans = Sigmd

## append shift to data: 
lags = 1:2 * -7 
leads = 1:2 * 7

## days and lpsolver
n <- 30
lp_solver = "glpk"


# baseline delphi
delphi_baseline <- function(country_code = "PT"){
  dt <- read.csv(paste0('../data/all_giant_df/',
                        country_code, '_alldf.csv'), as.is = T)
  dt_population <- dt$population[1]
  dt$cases[is.na(dt$cases)] <- 0
  dt <- dt %>% 
    select(date, cases, deaths, population, pct_cli) %>% 
    mutate(cases7day =  zoo::rollmean(cases, 7, fill = NA, align = "right"))
  dt$date <- as.Date(dt$date)
  
  dt[, c(2,3,5,6)] <- dt[, c(2,3,5, 6)]/dt$population[1]
  # get the start day for pct_cli
  dt <- dt[dt$date >= min(dt$date[!is.na(dt$pct_cli)]), ]
  # transform data
  dt <- dt %>% mutate(cases_trans = trans(cases),
                      deaths_trans = trans(deaths), 
                      pct_cli_trans = trans(pct_cli),
                      cases7day_trans = trans(cases7day))
  
  ca <- dt %>% select(date, cases7day_trans) %>% 
  rename(value = cases7day_trans, time_value = date)%>% 
    my_append_shifts(shifts = c(lags, leads))
  fb <- dt %>% select(date, pct_cli_trans) %>% 
    rename(value = pct_cli_trans, time_value = date)%>% 
    my_append_shifts(shifts = lags)
  
  colnames(fb) = sub("^value", "fb", colnames(fb))
  colnames(ca) = sub("^value", "case", colnames(ca))
  
  # combine data
  z <- full_join(ca, fb, by = "time_value")
  start_day <- min(z$time_value)
  end_day <- max(z$time_value)
  
  ## modelling
  res_list = vector("list", length = length(leads))
  for (i in 1:length(leads)) { 
    lead = leads[i]; cat("***", lead, "***\n")
    
    # Create a data frame to store our forecast results. Code below populates its
    # rows in a way that breaks from typical dplyr operations, done for efficiency 
    
    res_list[[i]] = z %>% 
      filter(between(time_value, as.Date(start_day) - min(lags) + lead, 
                     as.Date(end_day) - lead)) %>%
      select(time_value) %>%
      mutate(err0 = as.double(NA), err1 = as.double(NA), err2 = as.double(NA),
             strawman = as.double(NA), case_model = as.double(NA), case_fb_model = as.double(NA),
             lead = lead) 
    valid_dates = unique(res_list[[i]]$time_value)
    
    for (k in 1:length(valid_dates)) {
      date = valid_dates[k]; cat(format(date), "... ")
      
      # Filter down to training set and test set
      z_tr = z %>% filter(between(time_value, date - lead - n, date - lead))
      z_te = z %>% filter(time_value == date)
      inds = which(res_list[[i]]$time_value == date)
      
      # Create training and test responses
      y_tr = z_tr %>% pull(paste0("case+", lead))
      y_te = z_te %>% pull(paste0("case+", lead))
      
      # Strawman model
      cat("0")
      y_hat = z_te %>% pull(case)
      res_list[[i]][inds,]$err0 = abs((inv_trans(y_hat) * dt_population) - (inv_trans(y_te)*dt_population)) + 1e-6
      res_list[[i]][inds,]$strawman = inv_trans(y_hat) * dt_population
      
      # Cases only model
      cat("1")
      x_tr_case = z_tr %>% select(starts_with("case") & !contains("+"))
      x_te_case = z_te %>% select(starts_with("case") & !contains("+"))
      x_tr = x_tr_case; x_te = x_te_case # For symmetry wrt what follows 
      ok = complete.cases(x_tr, y_tr)
      if (sum(ok) > 0) {
        obj = quantile_lasso(as.matrix(x_tr[ok,]), y_tr[ok], tau = 0.5,
                             lambda = 0, lp_solver = lp_solver)
        y_hat = as.numeric(predict(obj, newx = as.matrix(x_te)))
        res_list[[i]][inds,]$err1 = abs((inv_trans(y_hat) * dt_population) - (inv_trans(y_te)*dt_population)) 
        res_list[[i]][inds,]$case_model = inv_trans(y_hat) * dt_population
      }
      
      # Cases and Facebook model
      cat("2\n")
      x_tr_fb = z_tr %>% select(starts_with("fb"))
      x_te_fb = z_te %>% select(starts_with("fb"))
      x_tr = cbind(x_tr_case, x_tr_fb)
      x_te = cbind(x_te_case, x_te_fb)
      ok = complete.cases(x_tr, y_tr)
      if (sum(ok) > 0) {
        obj = quantile_lasso(as.matrix(x_tr[ok,]), y_tr[ok], tau = 0.5,
                             lambda = 0, lp_solver = lp_solver)
        y_hat = as.numeric(predict(obj, newx = as.matrix(x_te)))
        res_list[[i]][inds,]$err2 = abs((inv_trans(y_hat) * dt_population) - (inv_trans(y_te) * dt_population))
        res_list[[i]][inds,]$case_fb_model = inv_trans(y_hat) * dt_population
      }
      
      
    }
  }
  res <- do.call(rbind, res_list)
  res <- res %>%                               # Restrict to common time
    mutate(case_sae = err1 / err0,
           fb_sae = err2 / err0) %>%           # to strawman model
    ungroup() 
  res$real_date <- res$time_value + res$lead
  
  res <- res %>% select("time_value", "real_date", "err0", "err1", 
                        "err2", "strawman", "case_model", "case_fb_model",
                        "lead", "case_sae", "fb_sae")


  # read prediction 
  # dtpred <- read.csv(paste0("../data/estimates-symptom-lags/cutoffs/PlotData/", country_code, 
  #                    "-estimates-lag-daily.csv"), as.is = T)
  # dtpred$date <- as.Date(dtpred$date)
  # dtpred2 <- full_join(dtpred,
  #                      res %>% filter(lead == 7), # add only 7 lead predictions
  #                      by = c("date" = "real_date"))
  cat("Writing baseline for: ", country_code, "...\n")
  write.csv(res, paste0("../data/baseline_outputs/",
                            country_code, "-baseline.csv"))
}

# 
# interest <- list.files("../data/estimates-symptom-lags/cutoffs/PlotData/",
#                        pattern="*.csv", full.names=FALSE)
interest <- c("BR", "DE", "EC", "PT", "UA", "ES", "IT", "CL", "FR", "GB")
dd <- sapply(interest, delphi_baseline)
