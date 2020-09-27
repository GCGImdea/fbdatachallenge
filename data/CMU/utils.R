library(readr)
library(purrr)
library(dplyr)
library(data.table)
library(collections)

get_file_properties <- function(filename) {
  short <- strsplit(filename, ".", fixed = TRUE)[[1]][1]
  parts <- strsplit(short, "_", fixed = TRUE)[[1]]

  filedate <- as.Date(paste(parts[3:5], collapse = "-"))
  recordeddate <- as.Date(paste(parts[7:9], collapse = "-"))

  return(data.frame(filename = filename,
                    date = filedate,
                    recorded = recordeddate))
}

get_big_df <- function(directory = "individual/", pattern = "*.csv.gz$") {
  files <- list.files(directory, pattern = pattern)

  files <- map_dfr(files, get_file_properties)

  latest_files <- files %>%
    group_by(date) %>%
    filter(recorded == max(recorded)) %>%
    ungroup() %>%
    pull(filename)

  big_df <- map_dfr(
    latest_files,
    function(f) {
      # stop readr from thinking commas = thousand separators
      read_csv(file.path(directory, f), locale = locale(grouping_mark = ""),
               col_types = cols(
                 A3 = col_character(),
                 A4 = col_number(),
                 B2 = col_character(),
                 C1 = col_character()))
    }
  )
}

#' Test if a specific selection is selected
#'
#' @param vec A list whose entries are character vectors, such as c("14", "15").
#' @param selection one string, such as "14"
#' @return a logical vector; for each list entry, whether selection is contained
#'   in the character vector.
is_selected <- function(vec, selection) {
  selections <- sapply(vec, function(resp) { selection %in% resp })

  return(ifelse(is.na(vec), NA, selections))
}

## Preferred column names for coded data
col_dict <- dict(list(
  'date' = 'date',
  'fips' = 'fips',
  'state' = 'state',
  'state_code' = 'state_code',
  'gender' = 'gender',
  'age_bucket' = 'age_bucket',
  'n' = 'n',
  'summed_n' = 'summed_n',
  'weight_sums' = 'weight_sums',
  'cli' = 'cli',
  'ili' = 'ili',
  'hh_cli' = 'hh_cli',
  'cmnty_cli' = 'cmnty_cli',
  'cli_anosmia_ageusia' = 'cli_anosmia_ageusia',
  'A1_1' = 'hh_fever',
  'A1_2' = 'hh_sore_throat',
  'A1_3' = 'hh_cough',
  'A1_4' = 'hh_shortness_of_breath',
  'A1_5' = 'hh_difficulty_breathing',
  'A2' = 'hh_cli_ct',
  'A4' = 'cmnty_cli_ct',
  'B2_1' = 'self_fever',
  'B2_2' = 'self_cough',
  'B2_3' = 'self_shortness_of_breath',
  'B2_4' = 'self_difficulty_breathing',
  'B2_5' = 'self_tiredness_or_exhaustion',
  'B2_6' = 'self_nasal_congestion',
  'B2_7' = 'self_runny_nose',
  'B2_8' = 'self_muscle_joint_aches',
  'B2_9' = 'self_sore_throat',
  'B2_10' = 'self_persistent_pain_pressure_in_chest',
  'B2_11' = 'self_nausea_vomiting',
  'B2_12' = 'self_diarrhea',
  'B2_13' = 'self_anosmia_ageusia',
  'B2_14' = 'self_other',
  'B2_15' = 'self_none_of_above',
  'B2_multi' = 'self_multiple_symptoms',
  'B5_1' = 'tested_and_positive',
  'B5_2' = 'tested_and_negative',
  'B5_3' = 'tested_no_result',
  'B5_4' = 'could_not_get_tested',
  'B5_5' = 'did_not_try_to_get_tested',
  'C3' = 'worked_outside_home',
  'C7' = 'avoid_contact_all_or_most_time',
  'C10_1_1' = 'outside_hh_contact_at_work_ct',
  'C10_2_1' = 'outside_hh_contact_shopping_ct',
  'C10_3_1' = 'outside_hh_contact_in_social_gatherings_ct',
  'C10_4_1' = 'outside_hh_contact_other_ct',
  'C11_1' = 'contact_covid_positive',
  'C1_1' = 'diabetes',
  'C1_2' = 'cancer',
  'C1_3' = 'heart_disease',
  'C1_4' = 'high_blood_pressure',
  'C1_5' = 'asthma',
  'C1_6' = 'chronic_lung_disease',
  'C1_7' = 'kidney_disease',
  'C1_8' = 'autoimmune_disorder',
  'C1_9' = 'no_above_medical_conditions',
  'C1_multi' = 'multiple_medical_conditions'
))

#' Do some coding and bucketing
code_df <- function(data) {
  data$date <- as.Date(data$StartDatetime)

  # AGE + GENDER
  data$age_bucket <- case_when(
    data$D2 == 1 | data$D2 == 2 ~ "18-34",
    data$D2 == 3 | data$D2 == 4 ~ "35-54",
    data$D2 == 5 | data$D2 == 6 | data$D2 == 7 ~ "55+",
    TRUE ~ as.character(data$D2)
  )
  data$gender <- case_when(
    data$D1 == 1 ~ "male",
    data$D1 == 2 ~ "female",
    data$D1 == 3 | data$D1 == 4 | data$D1 == 5 ~ "non-binary/other",
    TRUE ~ as.character(data$D1)
  )
  data$gender_overall = 'overall'
  data$age_bucket_overall = 'overall'

  ## remove NA gender, or age
  data <- data[!is.na(data$gender) & !is.na(data$age_bucket), ]

  if ("fips" %in% names(data)) {
    data$state = ifelse(nchar(data$fips) == 4, substr(data$fips, 1, 1), substr(data$fips, 1, 2))

    # remove NA location
    data <- data[!is.na(data$fips) & !is.na(data$state), ]

    data$state_code  <- case_when(
      data$state == "01" ~ "al",
      data$state == "02" ~ "ak",
      data$state == "04" ~ "az",
      data$state == "05" ~ "ar",
      data$state == "06" ~ "ca",
      data$state == "08" ~ "co",
      data$state == "09" ~ "ct",
      data$state == "10" ~ "de",
      data$state == "11" ~ "dc",
      data$state == "12" ~ "fl",
      data$state == "13" ~ "ga",
      data$state == "15" ~ "hi",
      data$state == "16" ~ "id",
      data$state == "17" ~ "il",
      data$state == "18" ~ "in",
      data$state == "19" ~ "ia",
      data$state == "20" ~ "ks",
      data$state == "21" ~ "ky",
      data$state == "22" ~ "la",
      data$state == "23" ~ "me",
      data$state == "24" ~ "md",
      data$state == "25" ~ "ma",
      data$state == "26" ~ "mi",
      data$state == "27" ~ "mn",
      data$state == "28" ~ "ms",
      data$state == "29" ~ "mo",
      data$state == "30" ~ "mt",
      data$state == "31" ~ "ne",
      data$state == "32" ~ "nv",
      data$state == "33" ~ "nh",
      data$state == "34" ~ "nj",
      data$state == "35" ~ "nm",
      data$state == "36" ~ "ny",
      data$state == "37" ~ "nc",
      data$state == "38" ~ "nd",
      data$state == "39" ~ "oh",
      data$state == "40" ~ "ok",
      data$state == "41" ~ "or",
      data$state == "42" ~ "pa",
      data$state == "44" ~ "ri",
      data$state == "45" ~ "sc",
      data$state == "46" ~ "sd",
      data$state == "47" ~ "tn",
      data$state == "48" ~ "tx",
      data$state == "49" ~ "ut",
      data$state == "50" ~ "vt",
      data$state == "51" ~ "va",
      data$state == "53" ~ "wa",
      data$state == "54" ~ "wv",
      data$state == "55" ~ "wi",
      data$state == "56" ~ "wy",
      data$state == "60" ~ "as",
      data$state == "66" ~ "gu",
      data$state == "69" ~ "mp",
      data$state == "72" ~ "pr",
      data$state == "78" ~ "vi",
      TRUE ~ as.character(data$state)
    )
  }

  # Wave 1, prior to April 15, has very strange results for item A2. > 90% of
  # people select yes. This is clearly bogus (probably a validation error in
  # Qualtrics), so we wipe A2 for April 15th and earlier.
  data$A2[data$date <= as.Date("2020-04-15")] <- NA

  # MULTI-SELECT RESPONSES
  data$B2_options <- strsplit(data$B2, ",", fixed = TRUE)
  data$C1_options <- strsplit(data$C1, ",", fixed = TRUE)

  data$hh_cli <- ifelse(data$A1_1 == 1 & (data$A1_3 == 1 | data$A1_4 == 1 | data$A1_5 == 1), 1, 0)
  data$hh_ili <- ifelse(data$A1_1 == 1 & (data$A1_2 == 1 | data$A1_3 == 1), 1, 0)
  data$cmnty_cli <- ifelse(data$A4 > 0, 1, 0)
  data$hh_cmnty_cli <- ifelse(data$hh_cli == 1 | data$cmnty_cli == 1, 1, 0)

  data$A1_1_logic <- data$A1_1 == 1
  data$A1_2_logic <- data$A1_2 == 1
  data$A1_3_logic <- data$A1_3 == 1
  data$A1_4_logic <- data$A1_4 == 1
  data$A1_5_logic <- data$A1_5 == 1

  data$B2_1_logic <- is_selected(data$B2_options, "1")
  data$B2_2_logic <- is_selected(data$B2_options, "2")
  data$B2_3_logic <- is_selected(data$B2_options, "3")
  data$B2_4_logic <- is_selected(data$B2_options, "4")
  data$B2_5_logic <- is_selected(data$B2_options, "5")
  data$B2_6_logic <- is_selected(data$B2_options, "6")
  data$B2_7_logic <- is_selected(data$B2_options, "7")
  data$B2_8_logic <- is_selected(data$B2_options, "8")
  data$B2_9_logic <- is_selected(data$B2_options, "9")
  data$B2_10_logic <- is_selected(data$B2_options, "10")
  data$B2_11_logic <- is_selected(data$B2_options, "11")
  data$B2_12_logic <- is_selected(data$B2_options, "12")
  data$B2_13_logic <- is_selected(data$B2_options, "13")
  data$B2_14_logic <- is_selected(data$B2_options, "14")
  data$B2_15_logic <- is_selected(data$B2_options, "15")
  data$B2_multi_logic <- ifelse(data$B2 %like% "," & !data$B2_15_logic, 1, 0)

  data$cli <- ifelse(data$B2_1_logic & (data$B2_2_logic | data$B2_3_logic | data$B2_4_logic), 1, 0)
  data$ili <- ifelse(data$B2_1_logic & (data$B2_2_logic | data$B2_9_logic), 1, 0)
  data$cli_anosmia_ageusia <- ifelse(is.na(data$cli) & is.na(data$B2_13_logic),
                                     NA,
                                     ifelse(data$cli == 1 | data$B2_13_logic, 1, 0))

  data$B5_1_logic <- ifelse(data$B5 == 1, 1, 0)
  data$B5_2_logic <- ifelse(data$B5 == 2, 1, 0)
  data$B5_3_logic <- ifelse(data$B5 == 3, 1, 0)
  data$B5_4_logic <- ifelse(data$B5 == 4, 1, 0)
  data$B5_5_logic <- ifelse(data$B5 == 5, 1, 0)

  data$C3_logic <- ifelse(data$C3 == 1, 1, 0)

  data$C7_logic <- ifelse(data$C7 == 1 | data$C7 == 2, 1, 0)

  data$C11_logic <- ifelse(data$C11 == 1, 1, 0)

  data$C1_1_logic <- is_selected(data$C1_options, "1")
  data$C1_2_logic <- is_selected(data$C1_options, "2")
  data$C1_3_logic <- is_selected(data$C1_options, "3")
  data$C1_4_logic <- is_selected(data$C1_options, "4")
  data$C1_5_logic <- is_selected(data$C1_options, "5")
  data$C1_6_logic <- is_selected(data$C1_options, "6")
  data$C1_7_logic <- is_selected(data$C1_options, "7")
  data$C1_8_logic <- is_selected(data$C1_options, "8")
  data$C1_9_logic <- is_selected(data$C1_options, "9")
  data$C1_multi_logic <- ifelse(nchar(data$C1) > 1 & !data$C1_9_logic, 1, 0)

  return(data)
}
