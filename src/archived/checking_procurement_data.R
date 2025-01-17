library(data.table)
library(ggplot2)


# This script is just a quick filter of data from:
# https://www.contractsfinder.service.gov.uk/
# where I did a search for awarded contracts with keywords:
# ["substance misuse" OR "substance use" OR addiction]
# There were 561 results.


df <-
  readr::read_csv("data/raw/notices.csv") |>
  janitor::clean_names()


d <- data.table::as.data.table(df)

d <-
  d[grepl(pattern = "young|child|cyp", x = description, ignore.case = TRUE, perl = TRUE)]


d <-
  d[!grepl(pattern = "smoking|detained|prison|homeless|testing|forensic|tobacco|housing|lodging|accommodation|employ|violence|waste|smoke", x = title, ignore.case = TRUE, perl = TRUE)]

d <-
  d[!grepl(pattern = "department of|ministry of", x = organisation_name, ignore.case = TRUE)]


d <-
  d[, .(awarded_date, organisation_name, title, description, supplier_name_address_ref_type_ref_number_is_sme_is_vcse, awarded_value)]


data.table::setorder(d, awarded_value)

d |>
  dplyr::mutate(title = forcats::as_factor(title)) |>
  ggplot(aes(x = title, y = awarded_value)) +
  geom_col() +
  coord_flip()


d <-
  d[, .(organisation_name, title, awarded_value, description)]


d |>
  readr::write_excel_csv("data/processed/cyp_contracts.csv")
