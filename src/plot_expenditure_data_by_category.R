#' Plot all substance misuse expenditure by category
#' 
#' @returns a ggplot
plot_expenditure_data_by_category <- function() {
 require(data.table) 
  require(ggplot2)
dt <- get_expenditure_data()

t <- 
  dt[period == "2021-22", .(data_item_name, net_current_expenditure)]


setnames(t, c("data_item_name", "baseline"))

dt <- 
  merge.data.table(dt, t, by = "data_item_name")

rm(t)

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
    plot.caption.position = "plot",
    plot.title.position = "plot",
    legend.justification = 0,
    axis.text.x = element_text(
      angle = 30,
      vjust = 0.5,
      hjust = 0.5
    )
  ) +
  labs(
    x = NULL,
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
