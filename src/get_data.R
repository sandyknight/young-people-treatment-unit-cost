library(data.table)

# Get the relevant data item names for substance misuse expenditure
# Downloads metadata ODS file, filter for substance misuse, removes "social support"
# expenditure since it's from another budget.
# Shortens the data item description
# This function is for use in get_expenditure_data()
get_data_dictionary <-
  function() {
    url <-
      "https://assets.publishing.service.gov.uk/media/674dbf32649db05b051ee245/Revenue_Outturn_metadata.ods"
    
    metadata_tmp <-
      tempfile()
    
    curl::curl_download(url = url, destfile = metadata_tmp)
    
    d <-
      readODS::read_ods(
        metadata_tmp,
        sheet = "Data_dictionary",
        skip = 3,
        .name_repair = janitor::make_clean_names
      ) |>
      data.table::as.data.table()
    
    d <-
      d[grepl(
        pattern = "substance misuse",
        x = description,
        ignore.case = TRUE,
        perl = TRUE
      ), ][grepl(
        pattern = "net current expenditure",
        x = description,
        ignore.case = TRUE,
        perl = TRUE
      ), ][!grepl(
        pattern = "social",
        x = description,
        ignore.case = TRUE,
        perl = TRUE
      ), ]
    
    
    d <-
      d[, .(data_item_name, description)]
    
    d[, description := stringr::str_remove(description, "NET current expenditure - Substance misuse - ")]
    
    return(d)
    
  }
# RO3 data ----------------------------------------------------------------

get_expenditure_data <- function() {
  data_dictionary <- get_data_dictionary()
  
  # URL of the data source
  url <-
    "https://assets.publishing.service.gov.uk/media/6656fbf716cf36f4d63ebb6c/Revenue_Outturn_time_series_data_v1.0_.csv"
  
  # Read data from the URL
  dt <- fread(url)
  
  dt <- dt[LA_name == "England"]
  
  # Data item name from the data dictionary
  data_item_name <- dplyr::pull(data_dictionary, data_item_name)
  
  # Select relevant columns
  select_cols <- c("year_ending", data_item_name)
  
  dt <- dt[, ..select_cols]
  
  dt <- data.table::melt(
    dt,
    id.vars = "year_ending",
    measure.vars = patterns("RO3"),
    variable.name = "data_item_name",
    value.name = "net_current_expenditure"
  )
  
  dt <-
    data.table::merge.data.table(dt, data_dictionary, by.x = "data_item_name", by.y = "data_item_name")
  
  dt[, period := substr(year_ending, 1, 4)][, period := paste(as.integer(period) - 1, substr(period, 3, 4), sep = "-")]
  
  dt[, net_current_expenditure := net_current_expenditure * 1000]
 
  dt[, year_ending := NULL]
   
  dt <- dt[, .(period, description, net_current_expenditure, data_item_name)]
  
  return(dt)
  
}



# Days in treatment data --------------------------------------------------

# Received by email from JK 2024-12-02T09:43:00

get_cyp_days_in_treatment_data <-
  function(file_path = "data/raw/YP contact days v1.xlsx") {
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
    
    # Filter for specific age groups
    #  dt <- dt[age %in% c("B: 12 to 15", "C: 16", "D: 17"), ]
    #  dt <- dt[provider_type == "YP only"]
    dt <- dt[age != "H: 25 and older", ]
    
    # Aggregate data by period
    dt2 <- dt[, .(
      in_tx = sum(in_tx, na.rm = TRUE),
      days_in_treatment = sum(days_in_treatment, na.rm = TRUE)
    ), by = period]
    
    # Return the processed data
    return(dt2)
  }



# GDP deflator ------------------------------------------------------------

get_gdp_deflator <- function() {
  url <-
    "https://obr.uk/download/october-2024-economic-and-fiscal-outlook-detailed-forecast-tables-economy/?tmstv=1733147138"
  
  gdp_deflator <-
    read.xlsx(
      url,
      sheet = "1.7",
      rows = c(116:137) ,
      cols = c(2, 16),
      colNames = FALSE
    )
  
  colnames(gdp_deflator) <-
    c("period", "gdp_deflator")
  
  return(gdp_deflator)
}
