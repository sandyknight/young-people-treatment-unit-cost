library(data.table)

df <- data.table::fread("adults_england_ndtms.csv")
sel_cols <- grep(pattern = "Parent", x = names(df), value = TRUE)


sel_cols <- grep(pattern = "All", sel_cols, ignore.case = FALSE, perl = TRUE, value = TRUE)

sel_cols <- c("Reporting period", "All in treatment", sel_cols)


df <- df[, ..sel_cols]

df[, all_parents := `Clients who are parents/carers - Parent living with children - All in treatment` + `Clients who are parents/carers - Parent not living with children - All in treatment`]

df <- df[, .(`Reporting period`, `All in treatment`, all_parents, `Clients who are parents/carers - Parent living with children - All in treatment`, `Clients who are parents/carers - Parent not living with children - All in treatment`)]


writexl::write_xlsx(df, path = "parents.xlsx")
