# CMU Symptom Survey Aggregated CSV Details

The exact survey text, for each version of the survey, is available
[here](https://cmu-delphi.github.io/delphi-epidata/symptom-survey/coding.html).
Note that not every survey item is shown to every respondent, depending on their
answers, and the exact wording of items can be important, so reviewing the
survey text is important for interpreting this data.

Documentation describing the survey weights, intended to help account for
sampling and non-response biases, will be made available
[here](https://cmu-delphi.github.io/delphi-epidata/symptom-survey/weights.html).

## Dimension Columns

1. `date` - the date the data was collected (in US, Pacific time)
2. `fips` - the FIPS county ID 
3. `state_code` - the 2 letter state code
4. `gender `- gender of survey respondent. Possible values: 
	a. male
	b. female
	c. non-binary or other. This category unfortunately must be excluded from
	the data; because it is uncommonly selected, releasing aggregates for it
	could make respondents identifiable. Even if the category was not reported
	separately, its values can be deduced by comparing the other categories,
	making it possible to calculate the responses submitted by individual
	respondents who selected this option. To avoid risk of re-identification and
	harm, we remove these responses from this aggregate data.
	d. overall (aggregation of all genders)
5. `age_bucket` - age_bucket of survey respondent. Possible values:
	a. 18-34
	b. 35-54
	c. 55+
	d. overall  (aggregation of all ages)
	
6. `n` - sample size of the date, fips, gender, and age_bucket aggregation.
   **Note:** Rows with `n < 50` are not reported in this dataset.
7. `summed_n` - in smoothed datasets, summed_n is the sum of `n` over the days
   used to calculate the smoothed estimates (7 day lagging sum)
8. `weight_sums` - Sum of the weights associated with the calculations

## Signals

1. `pct_cli` - Estimated percentage of people with **COVID-like illness**, which
   is defined as exhibiting a fever along with cough or shortness of breath or
   difficulty breathing.
2. `pct_ili` - Estimated percentage of people with **Influenza-like illness**,
   which is defined as exhibiting a fever with sore throat or cough
3.  `pct_cli_anosmia_ageusia` - estimated percentage of people with (1)
    COVID-like illness, which is defined as exhibiting a fever along with cough
    or shortness of breath or difficulty breathing OR (2) anosmia OR (3) ageusia
4. `pct_hh_cli` - Estimated percentage of people reporting **COVID-like illness
   in their household**
5. `pct_cmnty_cli` - Estimated percentage of people reporting knowing someone
   with **COVID-like illness in their local community**. This data only exists
   from 2020-04-16 onwards.
6. `pct_hh_fever` - Estimated percentage of people reporting that they or
   someone in their household experienced **a fever** in the past 24 hours
7. `pct_hh_sore_throat` - Estimated percentage of people reporting that they or
   someone in their household experienced **a sore throat** in the past 24 hours
8. `pct_hh_cough` - Estimated percentage of people reporting that they or
   someone in their household experienced **a cough** in the past 24 hours
9. `pct_hh_shortness_of_breath` - Estimated percentage of people reporting that
   they or someone in their household experienced **shortness of breath** in the
   past 24 hours
10. `pct_hh_difficulty_breathing` - Estimated percentage of people reporting
    that they or someone in their household experienced **difficulty breathing**
    in the past 24 hours
11. `mean_hh_cli_ct` - Estimated average number of people in respondent's
    household who are experiencing a **fever** and at least one of the
    following: **a sore throat**, **a cough**, **shortness of breath**, or
    **difficulty breathing**. This data only exists from 2020-04-16 onwards.
12. `mean_cmnty_cli_ct` - Estimated average number of people in respondent's
    local community who are experiencing a **fever** and at least one of the
    following: **a sore throat**, **a cough**, **shortness of breath**, or
    **difficulty breathing**. This data only exists from 2020-04-16 onwards.
13. `pct_self_fever` - Estimated percentage of people reporting that they have
    experienced **a fever** in the past 24 hours
14. `pct_self_cough` - Estimated percentage of people reporting that they have
    experienced **a cough** in the past 24 hours
15. `pct_self_shortness_of_breath` - Estimated percentage of people reporting
    that they have experienced **shortness of breath** in the past 24 hours
16. `pct_self_difficulty_breathing` - Estimated percentage of people reporting
    that they have experienced **difficulty breathing** in the past 24 hours
17. `pct_self_tiredness_or_exhaustion` - Estimated percentage of people
    reporting that they have experienced **tiredness or exhaustion** in the past
    24 hours
18. `pct_self_nasal_congestion` - Estimated percentage of people reporting that
    they have experienced **nasal congestion** in the past 24 hours
19. `pct_self_runny_nose` - Estimated percentage of people reporting that they
    have experienced **a runny nose** in the past 24 hours
20. `pct_self_muscle_joint_aches` - Estimated percentage of people reporting
    that they have experienced **muscle or joint aches** in the past 24 hours
21. `pct_self_sore_throat` - Estimated percentage of people reporting that they
    have experienced **a sore throat** in the past 24 hours
22. `pct_self_persistent_pain_pressure_in_chest` - Estimated percentage of
    people reporting that they have experienced **persistent pain or pressure in
    their chest** in the past 24 hours
23. `pct_self_nausea_vomiting` - Estimated percentage of people reporting that
    they have experienced **nausea or vomiting** in the past 24 hours
24. `pct_self_diarrhea` - Estimated percentage of people reporting that they
    have experienced **diarrhea** in the past 24 hours
25. `pct_self_anosmia_ageusia` - Estimated percentage of people reporting that
    they have experienced **loss of smell or taste** in the past 24 hours
26. `pct_self_other` - Estimated percentage of people reporting that they have
    experienced **another, not aforementioned symptom** in the past 24 hours
27. `pct_self_none_of_above` - Estimated percentage of people reporting that
    they have have not experienced any of the above symptoms in the past 24
    hours
28. `pct_self_multiple_symptoms` - Estimated percentage of people reporting that
    they have experienced **multiple of the above symptoms** in the past 24
    hours
29. `pct_tested_and_positive` - Estimated percentage of people reporting that
    they **have tested positive for COVID-19**. **Warning:** This survey item
    (B5) is only presented to respondents who report they are currently
    experiencing symptoms, so most responses are blank. (Wave 4 of the survey,
    not yet included in this data, will ask every respondent if they have been
    tested.)
30. `pct_tested_and_negative` - Estimated percentage of people reporting that
    they **have tested negative for COVID-19**
31. `pct_tested_no_result` - Estimated percentage of people reporting that they
    **have tested for COVID-19 but have not received a result**
32. `pct_could_not_get_tested` - Estimated percentage of people reporting that
    they **tried to get tested but could not get a test**
33. `pct_did_not_try_to_get_tested` - Estimated percentage of people reporting
    that they **have not tried to get tested for COVID-19**
34. `pct_worked_outside_home` - Estimated percentage of people reporting that
    **they have gone to work outside of their home**
35. `pct_avoid_contact_all_or_most_time` - Estimated percentage of people
    reporting that they are intentionally avoiding contact with people **"All of
    the time"** or **"most of the time"**
36. `mean_outside_hh_contact_at_work_ct` - Estimated average number of people
    the respondent has **had direct contact with outside of their household at
    work** in the past 24 hours.
37. `mean_outside_hh_contact_shopping_ct` - Estimated average number of people
    the respondent has **had direct contact with outside of their household
    while shopping for groceries and other essentials** in the past 24 hours.
38. `mean_outside_hh_contact_in_social_gatherings_ct` - Estimated average number
    of people the respondent has **had direct contact with outside of their
    household in social gatherings** in the past 24 hours.
40. `pct_contact_covid_positive` - Estimated percentage of people reporting that
    they **had direct contact with anyone who recently tested positive for
    COVID-19**.
41. `pct_diabetes` - Estimated percentage of people reporting that they have
    been diagnosed with **diabetes**
42. `pct_cancer` - Estimated percentage of people reporting that they have been
    diagnosed with **cancer (other than skin cancer)**
43. `pct_heart_disease` - Estimated percentage of people reporting that they
    have been diagnosed with **a heart disease**
44. `pct_high_blood_pressure` - Estimated percentage of people reporting that
    they have been diagnosed with **high blood pressure**
45. `pct_asthma` - Estimated percentage of people reporting that they have been
    diagnosed with **asthma**
46. `pct_chronic_lung_disease` - Estimated percentage of people reporting that
    they have been diagnosed with **a chronic lung disease such as COPD or
    emphysema**
47. `pct_kidney_disease` - Estimated percentage of people reporting that they
    have been diagnosed with **kidney disease**
48. `pct_autoimmune_disorder` - Estimated percentage of people reporting that
    they have been diagnosed with **an autoimmune disorder such as rheumatoid
    arthritis or Crohn's disease**
49. `pct_no_above_medical_conditions` - Estimated percentage of people reporting
    that they have not been diagnosed with **any of the above conditions**
50. `pct_multiple_medical_conditions` - Estimated percentage of people reporting
    that they have been diagnosed with **multiple of the above conditions**

## Signal Adjustments
1. `smoothed` - 7 day trailing average (today, and the 6 previous days)
2. `weighted` - Weighted to adjust for survey biases, using the survey weights
   described
   [here](https://cmu-delphi.github.io/delphi-epidata/symptom-survey/weights.html).

## Notes and Warnings

1. All of the `pct_` columns are percentages from 0 to 100. For example, a value
   of 1.0 in the dataset is 1% and a value of .01 in the dataset is .01%. This
   aligns with the live API.
2. Given sample size constraints, we will not be reporting on the gender-age
   granular aggregates at the county level. For example, this means we cannot
   query `(gender='female', age_bucket='18-34')` for counties. This is available
   for states.
3. The estimates here will *not* exactly match those reported by the COVIDcast
   API's `fb-survey` signal [described
   here](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/fb-survey.html).
   There are several reasons:
   - The API's weighted estimates use a mixing scheme, described in the
     documentation, to ensure that any individual respondent does not have
     disproportionately large weight when calculating an estimate. These
     aggregates do not apply this mixing procedure.
   - Criteria for inclusion in the API aggregates are more stringent and include
     validity checks to ensure the responses make sense (such as requiring that
     the number of people in the respondent's household is not absurdly large).
     The aggregates reported in these CSVs require only that respondents
     answered at least two questions.
   - For the household CLI and ILI signals, the API treats missing responses to
     the household symptom items as evidence there is no CLI or ILI in that
     household. (This may be corrected in a future version of the API signals.)
     In these CSVs, missing symptom responses cause a household to be excluded
     from calculations.
   - For the % CLI and % ILI signals, the API aggregates estimate the population
     proportion with COVID-like or influenza-like illness using an
     importance-sampling estimator. That is, by using the size of the household,
     the number of people with symptoms, and the symptoms, we can estimate the
     overall fraction of people with such symptoms. The `pct_cli` and `pct_ili`
     aggregates in these CSVs use the *individual* symptoms (item B2), so
     they're based on whether the respondent had symptoms, not the rest of the
     respondent's household.
   - The survey weights used by the API are slightly different from the survey
     weights used here, for technical reasons.
4. For data on or before April 15th, item A2 has been set to `NA`. An error in
   our survey setup caused anomalous data for this question, with over 90% of
   respondents selecting that someone in their household is sick; we suspect
   this is due to an error setting up item validation in the survey. For Wave 2
   and onward (deployed April 15th), this error was resolved, so item A2 is
   available.
