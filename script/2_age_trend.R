
library(MASS)
library(tidyverse)

# data --------------------------------------------------------------------

rm(list = ls())

df_number <- read.csv('./data/database/global_regional_number.csv')

df_rate <- read.csv('./data/database/global_regional_rate.csv')

age_groups <- c('20-24 years', '25-29 years', '30-34 years',
                '35-39 years', '40-44 years', '45-49 years', '50-54 years',
                '55+ years')

## get global data
df_global_number <- df_number |> 
  filter(location_name == 'Global',
         sex_name == 'Both',
         age_name %in% age_groups,
         measure_name %in% c('DALYs (Disability-Adjusted Life Years)', 'Incidence')) |> 
  arrange(measure_name, age_name, year)

df_global_rate <- df_rate |> 
  filter(location_name == 'Global',
         sex_name == 'Both',
         age_name %in% age_groups,
         measure_name %in% c('DALYs (Disability-Adjusted Life Years)', 'Incidence')) |> 
  arrange(measure_name, age_name, year)

# global trends -----------------------------------------------------------

## calculate AAPC
calculate_aapc <- function(data, measure, start_year, end_year, age) {
  data_filtered <- data |> 
    filter(measure_name == measure & age_name == age &
             year >= start_year & year <= end_year) |> 
    arrange(year)
  
  # get log value
  model <- glm.nb(val ~ year, data = data_filtered)
  
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

## calculate AAPC
var_years <- c(1990, 1999, 2009, 2019, 2021)
var_measures <- c('Incidence', 'DALYs (Disability-Adjusted Life Years)')

df_aapc_incidence <- data.frame(year_start = var_years[-length(var_years)],
                                year_end = var_years[-1],
                                measure = 'Incidence') |> 
  rowwise() |> 
  mutate(combinations = list(unique(df_global_number$age_name)))|> 
  unnest(combinations) |> 
  rename(age_name = combinations) |> 
  mutate(aapc_results = pmap(list(year_start, year_end, age_name),
                             ~calculate_aapc(df_global_number, measure, ..1, ..2, ..3))) |> 
  unnest_wider(col = aapc_results) |> 
  # round result
  mutate(across(c(AAPC, CI_lower, CI_upper), ~round(., 2))) |>
  mutate(P_value = case_when(P_value < 0.001 ~ '<0.001',
                             TRUE ~ as.character(round(P_value, 3))))

df_aapc_dalys <- data.frame(year_start = var_years[-length(var_years)],
                            year_end = var_years[-1],
                            measure = 'DALYs (Disability-Adjusted Life Years)') |> 
  rowwise() |>
  mutate(combinations = list(unique(df_global_number$age_name))) |>
  unnest(combinations) |>
  rename(age_name = combinations) |>
  mutate(aapc_results = pmap(list(year_start, year_end, age_name),
                             ~calculate_aapc(df_global_number, measure, ..1, ..2, ..3))) |>
  unnest_wider(col = aapc_results) |>
  # round result
  mutate(across(c(AAPC, CI_lower, CI_upper), ~round(., 2))) |>
  mutate(P_value = case_when(P_value < 0.001 ~ '<0.001',
                             TRUE ~ as.character(round(P_value, 3))))

df_aapc <- bind_rows(df_aapc_incidence, df_aapc_dalys) |> 
  mutate(Year = paste0(year_start, '~', year_end),
         Measure = case_when(measure == 'Incidence' ~ 'Incidence',
                             measure == 'DALYs (Disability-Adjusted Life Years)' ~ 'DALYs'),
         p_value = as.numeric(str_remove(P_value, '<')),
         `p value` = case_when(p_value <= 0.001 ~ '***',
                               p_value < 0.01 ~ '**',
                               p_value < 0.05 ~ '*',
                               TRUE ~ ''),
         `AAPC (95%CI)` = paste0(AAPC, '(', CI_lower, '~', CI_upper, ')', `p value`)) |>
  select(Year, age_name, Measure, `AAPC (95%CI)`)

# visualization -----------------------------------------------------------

plot_val <- function(data, measure, age, ylab) {
  data_filtered <- data |> 
    filter(measure_name == measure)
  
  breaks <- pretty(c(0, range(data_filtered$upper)), n = 5)
  
  data_filtered <- data_filtered |> 
    filter(age_name == age)
  
  p <- ggplot(data_filtered, aes(x = year, y = val)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    scale_x_continuous(breaks = seq(1990, 2020, 10),
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = breaks,
                       limits = range(breaks),
                       expand = c(0, 0)) +
    labs(title = age,
         x = NULL,
         y = ylab) +
    theme_bw()+
    theme(plot.title.position = 'plot')
  
  return(p)
}

fig_incidence <- lapply(age_groups, plot_val,
                        data = df_global_rate,
                        measure = 'Incidence',
                        ylab = 'Incidence rate')
fig_incidence <- patchwork::wrap_plots(fig_incidence, ncol = 4)

fig_dalys <- lapply(age_groups, plot_val,
                    data = df_global_rate,
                    measure = 'DALYs (Disability-Adjusted Life Years)',
                    ylab = 'DALYs rate')
fig_dalys <- patchwork::wrap_plots(fig_dalys, ncol = 4)

ggsave(paste0('./outcome/appendix/5.png'),
       plot = fig_incidence,
       width = 11,
       height = 7.5)

ggsave(paste0('./outcome/appendix/7.png'),
       plot = fig_dalys,
       width = 11,
       height = 7.5)

fig_incidence <- lapply(age_groups, plot_val,
                        data = df_global_number,
                        measure = 'Incidence',
                        ylab = 'Incidence')
fig_incidence <- patchwork::wrap_plots(fig_incidence, ncol = 4)

fig_dalys <- lapply(age_groups, plot_val,
                    data = df_global_number,
                    measure = 'DALYs (Disability-Adjusted Life Years)',
                    ylab = 'DALYs')
fig_dalys <- patchwork::wrap_plots(fig_dalys, ncol = 4)

ggsave(paste0('./outcome/appendix/6.png'),
       plot = fig_incidence,
       width = 11,
       height = 7.5)

ggsave(paste0('./outcome/appendix/8.png'),
       plot = fig_dalys,
       width = 11,
       height = 7.5)

# table -------------------------------------------------------------------

df_aapc_incidence_table <- df_aapc |> 
  filter(Measure == 'Incidence') |> 
  select(-Measure) |> 
  pivot_wider(names_from = Year, values_from = `AAPC (95%CI)`) |> 
  select(age_name, `1990~1999`, `1999~2009`, `2009~2019`, `2019~2021`)

# save to md file
markdown_table <- knitr::kable(df_aapc_incidence_table,
                               format = "markdown",
                               col.names = c('Age group', '1990 to 1999', '1999 to 2009', '2009 to 2019', '2019 to 2021'),
                               align = 'lcccc')
write(markdown_table,
      './outcome/appendix/table 1.md')

df_aapc_dalys_table <- df_aapc |>
  filter(Measure == 'DALYs') |>
  select(-Measure) |>
  pivot_wider(names_from = Year, values_from = `AAPC (95%CI)`) |>
  select(age_name, `1990~1999`, `1999~2009`, `2009~2019`, `2019~2021`)

# save to md file
markdown_table <- knitr::kable(df_aapc_dalys_table,
                               format = "markdown",
                               col.names = c('Age group', '1990 to 1999', '1999 to 2009', '2009 to 2019', '2019 to 2021'),
                               align = 'lcccc')

write(markdown_table,
      './outcome/appendix/table 2.md')
