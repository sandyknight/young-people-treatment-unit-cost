library(openxlsx)
library(data.table)


# RO3 data ----------------------------------------------------------------

get_cyp_expenditure_data <- function() {
  library(data.table)

 # URL of the data source
  url <- "https://assets.publishing.service.gov.uk/media/6656fbf716cf36f4d63ebb6c/Revenue_Outturn_time_series_data_v1.0_.csv"

  # Read data from the URL
  dt <- fread(url)

  dt <- dt[LA_name == "England"]

  # Data item name from the data dictionary
  data_item_name <- "RO3_phsbsspc_net_cur_exp"

  # Select relevant columns
  select_cols <- c("year_ending", data_item_name)
  dt <- dt[, ..select_cols]

  # Change year format
  dt[, period := substr(year_ending, 1, 4)]
  dt[, period := paste(as.integer(period) - 1, substr(period, 3, 4), sep = "-")]

  # Remove old year column and clean names
  dt <- dt[, .(period, RO3_phsbsspc_net_cur_exp)]
  setnames(dt, c("period", "cyp_net_current_expenditure"))

  # Aggregate by financial year
  dt2 <- dt[, .(total_cyp_net_current_exp = sum(cyp_net_current_expenditure, na.rm = TRUE)), by = .(period)]

  # Convert from thousands of GBP to GBP
  dt2[, total_cyp_net_current_exp := total_cyp_net_current_exp * 1000]

  # Return the processed data
  return(dt2)
}


# Days in treatment data --------------------------------------------------

# Received by email from JK 2024-12-02T09:43:00

get_cyp_days_in_treatment_data <- function(
    file_path = "data/raw/YP contact days v1.xlsx",
    providers = c("YP only", "Adult only", "Both"),
    age_groups = c("CYP", "18-24")
    ) {
  library(data.table)
  library(openxlsx)
  library(janitor)

  # Check if the file exists
  if (!file.exists(file_path)) {
    stop("The file does not exist at the specified path: ", file_path)
  }

  # Get sheet names from the Excel file
  shtnms <- openxlsx::getSheetNames(file_path)

  # Read data from the specified sheet
  df <- read.xlsx(file_path, sheet = shtnms[5])

  # Convert to data.table and clean column names
  dt <- as.data.table(df)
  setnames(dt, janitor::make_clean_names(names(dt)))

  # Filter according to user defined parameters



  # Aggregate data by period
  dt2 <- dt[, .(
    in_tx = sum(in_tx, na.rm = TRUE),
    days_in_treatment = sum(days_in_treatment, na.rm = TRUE)
  ), by = period]

  # Return the processed data
  return(dt2)
}



# GDP deflator ------------------------------------------------------------

get_gdp_deflator <- function(){
url <-
  "https://obr.uk/download/october-2024-economic-and-fiscal-outlook-detailed-forecast-tables-economy/?tmstv=1733147138"

gdp_deflator <-
  read.xlsx(url, sheet = "1.7", rows = c(116:137) , cols = c(2,16), colNames = FALSE)

colnames(gdp_deflator) <-
  c("period", "gdp_deflator")

return(gdp_deflator)
}
