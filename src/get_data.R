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
    data.table::merge.data.table(dt, d, by.x = "data_item_name", by.y = "data_item_name")
  
  dt[, period := substr(year_ending, 1, 4)][, period := paste(as.integer(period) - 1, substr(period, 3, 4), sep = "-")]
  
  dt[, net_current_expenditure := net_current_expenditure * 1000]
 
  dt[, year_ending := NULL]
   
  dt <- dt[, .(period, description, net_current_expenditure, data_item_name)]
  
  return(dt)
  
}



plot_expenditure_data_by_category <- function() {
  
dt <- get_expenditure_data()

t <- 
  dt[period == "2021-22", .(data_item_name, net_current_expenditure)]


setnames(t, c("data_item_name", "baseline"))

dt <- 
  merge.data.table(dt, t, by = "data_item_name")

rm(t)
print(dt)
dt |>
  dplyr::mutate(description = stringr::str_replace(description, "misuse ", "misuse\n")) |>
  ggplot(aes(x = period, y = net_current_expenditure)) +
  geom_col(width = 0.5, colour = "black") +
  geom_line(aes(
    y = baseline,
    x = rep(c(0, 2, 3, 4, 5, 6, 7.5), 5),
    group = description,
    colour = "Baseline (2021-22)"
  )) +
  facet_wrap( ~ description, scales = "free", ncol = 2) +
  tinythemes::theme_ipsum_rc() +
  scale_y_continuous(
    labels = function(x)
      scales::dollar(x, prefix = "£")
  ) +
  scale_colour_manual(values = c("Baseline (2021-22)" = "magenta")) +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0),
    legend.justification = 0,
    axis.text.x = element_text(
      angle = 30,
      vjust = 0.5,
      hjust = 0.5
    )
  ) +
  labs(
    x = "Financial year",
    y = "Reported net current expenditure",
    colour = NULL,
    title = "Reported net current expenditure on substance misuse",
    subtitle = "by expenditure category",
    caption = "Source:\nMHCLG, 'Local authority revenue expenditure and financing England: Revenue outturn multi-year data set'\nLast updated 3 December 2024"
  )

}

png(filename = "plots/substance_misuse_expenditure_plot.png", width = 31, height = 35, units = "cm", res = 250)
plot_expenditure_data_by_category()
dev.off()


# baseline <-
#   as.numeric(dt_aggregate[period == "2021-22", .(total)])
# 
# dt_aggregate |> 
#   ggplot(aes(x = period, y = total)) +
#   geom_col(width = 0.5, colour = "black") +
#   geom_hline(yintercept = baseline) +
#   scale_y_continuous(
#     labels = function(x)
#       scales::dollar(x, prefix = "£")
#   )


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
