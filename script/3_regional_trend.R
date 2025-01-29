
# loading packages --------------------------------------------------------

library(MASS)
library(tidyverse)
library(openxlsx)

# data --------------------------------------------------------------------

df_raw_number <- read.csv('../data/database/global_regional_number.csv')

df_raw_rate <- read.csv('../data/database/global_regional_rate.csv')

df_region_list <- read.csv('../data/region.csv')

## get incidence, incidence rate, DALYs, DALYs rate
df_global_number <- df_raw_number |> 
  filter(measure_name %in% c('DALYs (Disability-Adjusted Life Years)', 'Incidence'))

df_global_rate <- df_raw_rate |>
  filter(measure_name %in% c('DALYs (Disability-Adjusted Life Years)', 'Incidence'))

df_global <- bind_rows(df_global_number, df_global_rate) |> 
  mutate(location_name = str_remove(location_name, ' - WB'),
         # remove ' SDI' in region name
         location_name = str_remove(location_name, ' SDI'),
         # replace DALYs (Disability-Adjusted Life Years) with DALYs
         measure_name = str_replace(measure_name, 'DALYs \\(Disability-Adjusted Life Years\\)', 'DALYs'))

rm(df_raw_number, df_raw_rate, df_global_number, df_global_rate)

## regional data --------------------------------------------------------
df_region_number <- df_global |> 
  filter(sex_name == 'Both' & 
           age_name == '20+ years' &
           metric_name == 'Number' &
           location_name %in% df_region_list$Region) |>
  select(location_name, metric_name, measure_name, year, val, lower, upper) |> 
  arrange(measure_name, location_name, year) |> 
  rename(Index = location_name)

df_region_number_label <- df_region_number |> 
  filter(year %in% c(1990, 2019, 2021)) |> 
  # format number: romove decimals and add comma
  mutate(across(c(val, lower, upper), ~round(., 0)),
         across(c(val, lower, upper), ~format(., big.mark = ',', trim = TRUE)),
         Index = factor(Index, levels = df_region_list$Region),
         label = paste0(val, '<br>(', lower, '~', upper, ')')) |> 
  select(-val, -lower, -upper) |> 
  arrange(measure_name, Index, year)

df_region_rate <- df_global |> 
  filter(sex_name == 'Both' & 
           age_name == '20+ years' &
           metric_name == 'Rate' &
           location_name %in% df_region_list$Region) |>
  select(location_name, metric_name, measure_name, year, val, lower, upper) |>
  arrange(measure_name, location_name, year) |> 
  rename(Index = location_name)

df_region_rate_label <- df_region_rate |>
  filter(year %in% c(1990, 2019, 2021)) |> 
  # format number: romove decimals and add comma
  mutate(across(c(val, lower, upper), ~round(., 2)),
         across(c(val, lower, upper), ~format(., big.mark = ',', trim = TRUE)),
         Index = factor(Index, levels = df_region_list$Region),
         label = paste0(val, '<br>(', lower, '~', upper, ')')) |> 
  select(-val, -lower, -upper) |> 
  arrange(measure_name, Index, year)

## sex data ------------------------------------------------

df_sex_number <- df_global |> 
  filter(location_name == 'Global' & 
           age_name == '20+ years' &
           sex_name != 'Both' &
           metric_name == 'Number') |> 
  select(sex_name, metric_name, measure_name, year, val, lower, upper) |>
  arrange(measure_name, sex_name, year) |> 
  rename(Index = sex_name)

df_sex_number_label <- df_sex_number |> 
  filter(year %in% c(1990, 2019, 2021)) |> 
  # format number: romove decimals and add comma
  mutate(across(c(val, lower, upper), ~round(., 0)),
         across(c(val, lower, upper), ~format(., big.mark = ',', trim = TRUE)),
         label = paste0(val, '<br>(', lower, '~', upper, ')')) |>
  select(-val, -lower, -upper) |>
  arrange(measure_name, Index, year)

df_sex_rate <- df_global |> 
  filter(location_name == 'Global' & 
           age_name == '20+ years' &
           sex_name != 'Both' &
           metric_name == 'Rate') |>
  select(sex_name, metric_name, measure_name, year, val, lower, upper) |>
  arrange(measure_name, sex_name, year) |> 
  rename(Index = sex_name)

df_sex_rate_label <- df_sex_rate |> 
  filter(year %in% c(1990, 2019, 2021)) |> 
  # format number: romove decimals and add comma
  mutate(across(c(val, lower, upper), ~round(., 2)),
         across(c(val, lower, upper), ~format(., big.mark = ',', trim = TRUE)),
         label = paste0(val, '<br>(', lower, '~', upper, ')')) |>
  select(-val, -lower, -upper) |>
  arrange(measure_name, Index, year)

## age data --------------------------------------------------------

df_age_number <- df_global |> 
  filter(location_name == 'Global' &
           sex_name == 'Both' &
           metric_name == 'Number' &
           age_name %in% c('20-24 years', '25-29 years', '30-34 years', '35-39 years',
                           '40-44 years', '45-49 years', '50-54 years', '55+ years')) |>
  select(age_name, metric_name, measure_name, year, val, lower, upper) |>
  arrange(measure_name, age_name, year) |>
  rename(Index = age_name)

df_age_number_label <- df_age_number |>
  filter(year %in% c(1990, 2019, 2021)) |> 
  # format number: romove decimals and add comma
  mutate(across(c(val, lower, upper), ~round(., 0)),
         across(c(val, lower, upper), ~format(., big.mark = ',', trim = TRUE)),
         label = paste0(val, '<br>(', lower, '~', upper, ')')) |>
  select(-val, -lower, -upper) |>
  arrange(measure_name, Index, year)

df_age_rate <- df_global |>
  filter(location_name == 'Global' &
           sex_name == 'Both' &
           metric_name == 'Rate' &
           age_name %in% c('20-24 years', '25-29 years', '30-34 years', '35-39 years',
                           '40-44 years', '45-49 years', '50-54 years', '55+ years')) |>
  select(age_name, metric_name, measure_name, year, val, lower, upper) |>
  arrange(measure_name, age_name, year) |>
  rename(Index = age_name)

df_age_rate_label <- df_age_rate |>
  filter(year %in% c(1990, 2019, 2021)) |> 
  # format number: romove decimals and add comma
  mutate(across(c(val, lower, upper), ~formatC(., format = 'f', big.mark = ',', digits = 2)),
         across(c(val, lower, upper), ~format(., big.mark = ',', trim = TRUE)),
         label = paste0(val, '<br>(', lower, '~', upper, ')')) |>
  select(-val, -lower, -upper) |>
  arrange(measure_name, Index, year)
  
## bind data -----------------------------------------------------------

df_label <- bind_rows(df_region_number_label, df_region_rate_label,
                      df_sex_number_label, df_sex_rate_label,
                      df_age_number_label, df_age_rate_label) |>
  mutate(title = paste(metric_name, measure_name, year)) |> 
  select(title, Index, label) |> 
  pivot_wider(names_from = title,
              values_from = label) |> 
  mutate(Index = factor(Index, levels = c('Global', 'Female', 'Male',
                                          '20-24 years', '25-29 years', '30-34 years', '35-39 years',
                                          '40-44 years', '45-49 years', '50-54 years', '55+ years',
                                          'High', "High-middle", "Middle", "Low-middle", "Low",
                                          "East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean",
                                          "Middle East & North Africa", "South Asia", "Sub-Saharan Africa"))) |> 
  arrange(Index)

# AAPC -----------------------------------------------------------

## calculate AAPC
calculate_aapc <- function(data, measure_set) {
  data_filtered <- data |> 
    filter(measure == measure_set) |>
    arrange(year) |> 
    mutate(val = round(val))
  
  # get log value
  # data_filtered$log_val <- log(data_filtered$val)
  # model <- lm(log_val ~ year, data = data_filtered)
  # model <- glm(val ~ year, family = poisson(link = "log"), data = data_filtered)
  model <- MASS::glm.nb(val ~ year, data = data_filtered)
  
  # extract slope, se, p_value
  slope <- coef(model)["year"]
  slope_se <- summary(model)$coefficients["year", "Std. Error"]
  p_value <- summary(model)$coefficients["year", "Pr(>|z|)"]
  
  # calculate AAPC
  aapc <- (exp(slope) - 1) * 100
  ci_lower <- (exp(slope - 1.96 * slope_se) - 1) * 100
  ci_upper <- (exp(slope + 1.96 * slope_se) - 1) * 100

  
  # return result
  result <- list(
    AAPC = aapc,
    CI_lower = ci_lower,
    CI_upper = ci_upper,
    P_value = p_value
  )
  return(result)
}

df_global_trend <- rbind(df_region_number, df_sex_number, df_age_number) |>
  # combined Index, metric_name, measure_name
  mutate(measure = paste(Index, metric_name, measure_name, sep = '--'))

df_aapc_1 <- data.frame(measure = unique(df_global_trend$measure)) |> 
  rowwise() |> 
  mutate(aapc_results = map(measure, ~calculate_aapc(filter(df_global_trend, year <= 2019), .x))) |> 
  unnest_wider(col = aapc_results) |> 
  mutate(across(c(AAPC, CI_lower, CI_upper), ~formatC(., format = 'f', digits = 2)),
         P_value = case_when(P_value < 0.001 ~ '***',
                             P_value < 0.01 ~ '**',
                             P_value < 0.05 ~ '*',
                             TRUE ~ as.character(round(P_value, 3))),
         `AAPC (95%CI)` = paste0(AAPC, ' (', CI_lower, '~', CI_upper, ')', P_value)) |> 
  select(measure, `AAPC (95%CI)\n1990-2019` = `AAPC (95%CI)`)
  
df_aapc_2 <- data.frame(measure = unique(df_global_trend$measure)) |>
  rowwise() |>
  mutate(aapc_results = map(measure, ~calculate_aapc(filter(df_global_trend, year >= 2019), .x))) |>
  unnest_wider(col = aapc_results) |> 
  mutate(across(c(AAPC, CI_lower, CI_upper), ~formatC(., format = 'f', digits = 2)),
         P_value = case_when(P_value < 0.001 ~ '***',
                             P_value < 0.01 ~ '**',
                             P_value < 0.05 ~ '*',
                             TRUE ~ as.character(round(P_value, 3))),
         `AAPC (95%CI)` = paste0(AAPC, ' (', CI_lower, '~', CI_upper, ')', P_value)) |>
  select(measure, `AAPC (95%CI)\n2019-2021` = `AAPC (95%CI)`)
  
df_aapc <- left_join(df_aapc_1, df_aapc_2, by = 'measure') |>
  # split measure
  separate(measure, c('Index', 'metric_name', 'measure_name'), sep = '--') |>
  select(Index, measure_name, `AAPC (95%CI)\n1990-2019`, `AAPC (95%CI)\n2019-2021`) |>
  mutate(Index = factor(Index, levels = c('Global', 'Female', 'Male',
                                          'High', "High-middle", "Middle", "Low-middle", "Low",
                                          '20-24 years', '25-29 years', '30-34 years', '35-39 years',
                                          '40-44 years', '45-49 years', '50-54 years', '55+ years',
                                          "East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean",
                                          "Middle East & North Africa", "South Asia", "Sub-Saharan Africa"))) |> 
  arrange(Index)

# save --------------------------------------------------------------------

df_output_inci <- df_label |> 
  select(Index, contains('Incidence')) |> 
  left_join(filter(df_aapc, measure_name == 'Incidence'), by = 'Index') |> 
  select(Index,
         `Number Incidence 1990`, `Rate Incidence 1990`,
         `Number Incidence 2019`, `Rate Incidence 2019`,
         `Number Incidence 2021`, `Rate Incidence 2021`,
         `AAPC (95%CI)\n1990-2019`, `AAPC (95%CI)\n2019-2021`)

# save to md file
markdown_table <- knitr::kable(df_output_inci,
                               format = "markdown",
                               col.names = c('Group',
                                             '1990<br>Incidence',
                                             '1990<br>Incidence rate (per 100,000)',
                                             '2019<br>Incidence',
                                             '2019<br>Incidence rate (per 100,000)',
                                             '2021<br>Incidence',
                                             '2021<br>Incidence rate (per 100,000)',
                                             'AAPC (95%CI)<br>1990-2019',
                                             'AAPC (95%CI)<br>2019-2021'),
                               escape = FALSE)
write(markdown_table, '../outcome/table_2_incidence_trend.md')

df_output_daly <- df_label |> 
  select(Index, contains('DALYs')) |> 
  left_join(filter(df_aapc, measure_name == 'DALYs'), by = 'Index') |> 
  select(Index,
         `Number DALYs 1990`, `Rate DALYs 1990`,
         `Number DALYs 2019`, `Rate DALYs 2019`,
         `Number DALYs 2021`, `Rate DALYs 2021`,
         `AAPC (95%CI)\n1990-2019`, `AAPC (95%CI)\n2019-2021`)

# save to md file
markdown_table <- knitr::kable(df_output_daly,
                               format = "markdown",
                               col.names = c('Group',
                                             '1990<br>DALYs',
                                             '1990<br>DALYs rate (per 100,000)',
                                             '2019<br>DALYs',
                                             '2019<br>DALYs rate (per 100,000)',
                                             '2021<br>DALYs',
                                             '2021<br>DALYs rate (per 100,000)',
                                             'AAPC (95%CI)<br>1990-2019',
                                             'AAPC (95%CI)<br>2019-2021'),
                               escape = FALSE)
write(markdown_table, '../outcome/table_3_dalys_trend.md')
