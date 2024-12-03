# Load up our data functions from get_data.R
source("src/get_data.R")

# Get the data for days in treatment for children and young people (CYP)
cyp_days_in_treatment <- 
  get_cyp_days_in_treatment_data()

# Get the expenditure data for CYP
cyp_expenditure <- 
  get_cyp_expenditure_data()

# Fetch the GDP deflator data for adjusting prices
gdp_deflator <- 
  get_gdp_deflator()

# Merge the days in treatment data with the expenditure data based on 'period'
d1 <- 
  data.table::merge.data.table(cyp_days_in_treatment, cyp_expenditure, by = "period")

# Merge the above result with the GDP deflator data
d1 <- 
  data.table::merge.data.table(d1, gdp_deflator, by = "period") 

# Get the GDP deflator for the year 2024-25 (our reference year for adjustment)
gdp_deflator_2425 <- 
  gdp_deflator[gdp_deflator[, "period"] == "2024-25", "gdp_deflator"]

# Adjust the expenditure to 2024-25 prices using the GDP deflator
d1[, expenditure_gbp2425 := total_cyp_net_current_exp * (gdp_deflator_2425 / gdp_deflator)]

# Calculate the average days per person and the average cost per day
d2 <- 
  d1[, .(
    mean_days_per_person = days_in_treatment / in_tx, 
    mean_cost_per_day = expenditure_gbp2425 / days_in_treatment
  )]

# Compute the average cost per person
d3 <- 
  d2[, .(mean_cost_per_person = mean_days_per_person * mean_cost_per_day)]

# Calculate the mean annual cost per person across all periods
mean_annual_cost_per_person <- 
  mean(dplyr::pull(d3, mean_cost_per_person))

# Combine the mean cost per person with the number of people in treatment
d4 <- 
  cbind(d3, d1[, .(in_tx)])

# Check that our estimated total cost matches the actual expenditure
testthat::test_that(
  "Cost per person per year times number in treatment should match actual expenditure",
  testthat::expect_equal(
    dplyr::pull(d4[, .(est_total_cost = mean_cost_per_person * in_tx)], est_total_cost),
    dplyr::pull(d1, expenditure_gbp2425)
  )
)

# Calculate the percentage difference between estimated and actual expenditure
scales::percent(
  (mean_annual_cost_per_person * mean(dplyr::pull(d1, in_tx)) - 
     mean(dplyr::pull(d1, expenditure_gbp2425))) / mean(dplyr::pull(d1, expenditure_gbp2425)),
  accuracy = 0.01
)

mean_annual_cost_per_person
