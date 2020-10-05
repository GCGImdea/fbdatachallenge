# Processed UMD-data

## Methodology

1. We are focusing on **pct_cli**: estimated percentage of people with COVID-like illness.
2. We are breaking down this percentage and estimating the number of people with COVID-like illness by region or country (**pct_cli * population**).
3. Then, we aggregate responses until we get a batch size greater or equal to **population/b_size_denom**.
4. Next, the new percentages (**batched_pct_cli**) are computed for each batch.
5. Finally, these **batched_pct_cli** are smoothed (penalized splines) and stored in **batched_pct_cli_smoothed**.


## Datasets

We have 5 different data files:
* **BRXX_UMD_data.csv**: where **XX** is the ISO code for each brazilian region.
  + contains the **batched_pct_cli** estimates for region **XX** and a fixed **b_size_denom** (check current value in the corresponding column).
* **BR_UMD_data.csv**:  
  + aggreagates all the **BRXX_UMD_data** for a fixed **b_size_denom**.
* **BR_UMD_data_by_batch_size.csv**:  
  + aggreagates all the **BRXX_UMD_data** for several **b_size_denom**.
* **BR_UMD_country_data.csv**: 
  + contains the **batched_pct_cli** estimates at country level (using UMD's country data) for a fixed **b_size_denom** (check current value in the corresponding column).
* **BR_UMD_country_data_by_batch_size**:
  + aggreagates all the estimates at country level for several **b_size_denom**.
  
### Variables of interest  

The data files contains the same variables (columns), except for the regional data that adds a **region** column with the corresponding ISO code.

* **date**
* **region**
* **population**: population size by regional (files *BRXX_UMD_data...*) or country level (files *BR_UMD_country...*).
* **total_responses**: total number of responses per day.
* **pct_cli**: estimated percentage of people with COVID-like illness (**UMD's estimates**).
* **pct_cli_smooth**: smoothed by us using p-splines.
* **number_cli**: estimated number of people with symptoms (= pct_cli*population)
* **batched_pct_cli**: our batch estimates, as described in Steps 3-4 from the Methodology section.
* **batched_pct_cli_smooth**: smooth estimates, as described in Step 5 from the Methodology section.
* **estimate_cli**: estimated number of people with symptoms, according to our batched estimates (computed as population*(batched_pct_cli_smooth/100) ).
* **cum_estimate_cli**: cumulative estimate_cli.
* **number_cli_smooth**: a smoothed version of UMD's number_cli (= population*(pct_cli_smooth/100) ).
* **cum_number_cli_smooth**: cumulative number_cli_smooth.
* **b_size_denom**: quotient (denominator) used to compute the batch size, note that **batch_size = population/b_size_denom**

## Plots...

Folders containing some relevant plots of the estimated percentages and smoothed values. They are available by region and batch size (equivalent to **b_size_denom**).
