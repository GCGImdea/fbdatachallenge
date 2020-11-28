library(dplyr)
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



dt <- read.csv('../data/all_giant_df/ES_alldf.csv', as.is = T)
dt_population <- dt$population[1] 

### covariates in building model: pct_cli, cases_7day

dt <- dt %>% 
  select(date, cases, deaths, population, pct_cli) %>% 
  mutate(cases7day =  zoo::rollmean(cases, 7, fill = NA, align = "right"))
dt$date <- as.Date(dt$date)

# convert to pct
dt[, c(2,3,5, 6)] <- dt[, c(2,3,5, 6)]/dt$population[1]
# select when pct_cli was available
dt <- dt[dt$date >= as.Date("2020-04-23"), ]

# transform data
Logit = function(x, a = 0.01) log((x + a) / (1 - x + a))
Sigmd = function(y, a = 0.01) (exp(y) * (1 + a) - a) / (1 + exp(y))
trans = Logit
inv_trans = Sigmd

dt <- dt %>% mutate(cases_trans = trans(cases),
                    deaths_trans = trans(deaths), 
                    pct_cli_trans = trans(pct_cli),
                    cases7day_trans = trans(cases7day))

## append shift to data: 
lags = 1:2 * -7 
leads = 1:2 * 7

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
n <- 30
lp_solver = "glpk"
#####################################modelling################3
library(quantgen) 
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
    res_list[[i]][inds,]$err0 = abs(inv_trans(y_hat) - inv_trans(y_te))
    
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
      res_list[[i]][inds,]$err1 = abs(inv_trans(y_hat) - inv_trans(y_te))
    }
    
    # Cases and Facebook model
     cat("2")
    x_tr_fb = z_tr %>% select(starts_with("fb"))
    x_te_fb = z_te %>% select(starts_with("fb"))
    x_tr = cbind(x_tr_case, x_tr_fb)
    x_te = cbind(x_te_case, x_te_fb)
    ok = complete.cases(x_tr, y_tr)
    if (sum(ok) > 0) {
      obj = quantile_lasso(as.matrix(x_tr[ok,]), y_tr[ok], tau = 0.5,
                           lambda = 0, lp_solver = lp_solver)
      y_hat = as.numeric(predict(obj, newx = as.matrix(x_te)))
      res_list[[i]][inds,]$err2 = abs(inv_trans(y_hat) - inv_trans(y_te))
    }
    
    
  }
}
res = do.call(rbind, res_list)
#################
library(tidyr)
library(ggplot2)

model_names = c("Cases", "Cases + Facebook")

# Restrict to common period for all 4 models, then calculate the scaled errors 
# for each model, that is, the error relative to the strawman's error
res_all = res %>%
  drop_na() %>%                                       # Restrict to common time
  mutate(err1 = err1 / err0, err2 = err2 / err0) %>%  # to strawman model
  mutate(dif12 = err1 - err2) %>%                 # Compute differences
  ungroup() %>% # relative to cases model
  select(-err0) 

# Calculate and print median errors, for all 4 models, and just 7 days ahead
res_err = res_all %>% 
  select(-starts_with("dif")) %>%
  pivot_longer(names_to = "model", values_to = "err",
               cols = -c(time_value, lead)) %>%
  mutate(lead = factor(lead, labels = paste(leads, "days ahead")),
         model = factor(model, labels = model_names))

knitr::kable(res_err %>% 
               group_by(model, lead) %>%
               summarize(err = median(err), n = length(unique(time_value))) %>% 
               arrange(lead) %>% ungroup() %>%
               rename("Model" = model, "Median scaled error" = err, 
                      "Target" = lead, "Test days" = n) %>%
               filter(Target == "7 days ahead"), 
             caption = paste("Test period:", min(res_err$time_value), "to",
                             max(res_err$time_value)),
             format = "html", table.attr = "style='width:70%;'")

