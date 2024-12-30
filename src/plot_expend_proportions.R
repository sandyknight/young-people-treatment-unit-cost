library(tidyverse)
R.utils::sourceDirectory("src")


df <- get_expenditure_data()


t <- 
df |> 
  as_tibble() |> 
  group_by(period) |> 
  summarise(total_expenditure = sum(net_current_expenditure))

df |> 
  as_tibble() |> 
  select(-data_item_name) |> 
  pivot_wider(names_from = description, values_from = net_current_expenditure) |> 
  left_join(t, by = "period") |> 
  mutate(across(2:6, ~.x/total_expenditure)) |> 
  select(-total_expenditure) |> 
  pivot_longer(cols = c(2:6), names_to = "category") |> 
  mutate(period = as_factor(period)) |> 
  ggplot(aes(x = period, y = value)) + 
  geom_col(aes(fill = category))


