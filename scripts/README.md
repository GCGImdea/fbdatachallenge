## Information of the files in this folder:

- estimates-regeneration.R : This script invokes the other scripts to regenerate the estimates if the data has changed.

The above script invokes:

- script-W: Computes estimates from the CoronaSurveys responses as described in Section 9.2 in the master document (W estimate). This script uses smooth_column.R

### Other scripts:

- smooth_column.R: Contains a function to smooth a column in a data frame, generating a new column with the smoothed values. This smoothing is for *monotonically increasing* data.

- ES_UMD_analysis_batches.R: computes the **batched_pct_cli** estimates using the Spanish UMD *regional* data. Details in README file at "data/estimates-umd-batches/spain/" 

- ES_UMD_country_analysis_batches.R: computes the **batched_pct_cli** estimates using the Spanish UMD data, at *country level*. Details in README file at "data/estimates-umd-batches/spain/".

- ES_datadista_cases_fatalities.R: Merges cases and fatalities from "data/datadista_regional/". Smoothed quatities are also computed and stored in a single file: "/data/datadista_regional/smooth_cases_fatalities.csv"

- analysis.R

- andalucia_data.csv

- ccfr.R

- plot_example.R

- script-ccfr-based.R 
