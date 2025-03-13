
# data --------------------------------------------------------------------

rm(list = ls())

df_number <- read.csv('./data/database/incidence_number_both.csv')
df_number <- read.csv('./data/database/dalys_number_both.csv') |> 
  rbind(df_number)

df_rate <- read.csv('./data/database/incidence_rate_both.csv')
df_rate <- read.csv('./data/database/dalys_rate_both.csv') |> 
  rbind(df_rate)

region_name <- read.csv('./data/region.csv') |> 
  filter(Region != 'Global') |>
  mutate(
    Is_SDI = Group == "Sociodemographic index",
    Is_Region = Group == "Region",
    Region = case_when(
      Group == "Sociodemographic index" ~ paste(Region, 'SDI'),
      Group == "Region" ~ paste(Region, '- WB'),
      TRUE ~ Region
    )
  ) |>
  pull(Region)

## get global data
df_global_number <- df_number |> 
  filter(sex_name == 'Both',
         age_name == '20+ years',
         location_name %in% region_name,
         measure_name %in% c('DALYs (Disability-Adjusted Life Years)', 'Incidence')) |> 
  arrange(measure_name, location_name, year)

df_global_rate <- df_rate |> 
  filter(sex_name == 'Both',
         age_name == '20+ years',
         location_name %in% region_name,
         measure_name %in% c('DALYs (Disability-Adjusted Life Years)', 'Incidence')) |>
  arrange(measure_name, location_name, year)

# global trends -----------------------------------------------------------

## calculate AAPC
calculate_aapc <- function(data, measure, start_year, end_year, locat) {
  data_filtered <- data |> 
    filter(measure_name == measure & location_name == locat &
             year >= start_year & year <= end_year) |> 
    arrange(year) |> 
    mutate(val = round(val))
  
  # print(paste0(locat, ' ', measure, ' ', start_year, ' ', end_year))
  
  # browser()
  
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
  mutate(combinations = list(unique(df_global_number$location_name)))|> 
  unnest(combinations) |> 
  rename(location_name = combinations) |> 
  mutate(aapc_results = pmap(list(measure, year_start, year_end, location_name),
                             ~calculate_aapc(df_global_number, ..1, ..2, ..3, ..4))) |> 
  unnest_wider(col = aapc_results) |> 
  # round result
  mutate(across(c(AAPC, CI_lower, CI_upper), ~round(., 2))) |>
  mutate(P_value = case_when(P_value < 0.001 ~ '<0.001',
                             TRUE ~ as.character(round(P_value, 3))))

df_aapc_dalys <- data.frame(year_start = var_years[-length(var_years)],
                            year_end = var_years[-1],
                            measure = 'DALYs (Disability-Adjusted Life Years)') |> 
  rowwise() |> 
  mutate(combinations = list(unique(df_global_number$location_name)))|> 
  unnest(combinations) |> 
  rename(location_name = combinations) |> 
  mutate(aapc_results = pmap(list(measure, year_start, year_end, location_name),
                             ~calculate_aapc(df_global_number, ..1, ..2, ..3, ..4))) |> 
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
  select(Year, location_name, Measure, `AAPC (95%CI)`)

# visualization -----------------------------------------------------------

plot_val <- function(data, measure, locat, ylab) {
  data_filtered <- data |> 
    filter(measure_name == measure)
  
  breaks <- pretty(c(0, range(data_filtered$upper)), n = 5)
  
  data_filtered <- data_filtered |> 
    filter(location_name == locat)
  
  p <- ggplot(data_filtered, aes(x = year, y = val)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    scale_x_continuous(breaks = seq(1990, 2020, 10),
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = breaks,
                       limits = range(breaks),
                       expand = c(0, 0)) +
    labs(title = str_remove(locat, ' - WB'),
         x = NULL,
         y = ylab) +
    theme_bw()+
    theme(plot.title.position = 'plot')
  
  return(p)
}

sdi_name <- region_name |> 
  str_subset('SDI')

reg_name <- region_name |> 
  str_subset('- WB')

fig_incidence <- lapply(sdi_name, plot_val,
                        data = df_global_rate,
                        measure = 'Incidence',
                        ylab = 'Incidence rate')
fig_incidence <- patchwork::wrap_plots(fig_incidence, ncol = 3)

fig_dalys <- lapply(sdi_name, plot_val,
                    data = df_global_rate,
                    measure = 'DALYs (Disability-Adjusted Life Years)',
                    ylab = 'DALYs rate')
fig_dalys <- patchwork::wrap_plots(fig_dalys, ncol = 3)

ggsave(paste0('./outcome/appendix/9.png'),
       plot = fig_incidence / fig_dalys,
       width = 11,
       height = 11)

fig_incidence <- lapply(reg_name, plot_val,
                        data = df_global_rate,
                        measure = 'Incidence',
                        ylab = 'Incidence rate')
fig_incidence <- patchwork::wrap_plots(fig_incidence, ncol = 3)

fig_dalys <- lapply(reg_name, plot_val,
                    data = df_global_rate,
                    measure = 'DALYs (Disability-Adjusted Life Years)',
                    ylab = 'DALYs rate')
fig_dalys <- patchwork::wrap_plots(fig_dalys, ncol = 3)

ggsave(paste0('./outcome/appendix/10.png'),
       plot = fig_incidence / fig_dalys,
       width = 11,
       height = 14)

# table -------------------------------------------------------------------

df_aapc_incidence_table <- df_aapc |> 
  filter(Measure == 'Incidence') |> 
  select(-Measure) |> 
  pivot_wider(names_from = Year, values_from = `AAPC (95%CI)`) |> 
  select(location_name, `1990~1999`, `1999~2009`, `2009~2019`, `2019~2021`) |> 
  mutate(location_name = factor(location_name, levels = c(region_name))) |> 
  arrange(location_name) |> 
  mutate(location_name = str_remove(location_name, ' - WB'))

# save to md file
markdown_table <- knitr::kable(df_aapc_incidence_table,
                               format = "markdown",
                               col.names = c('', '1990 to 1999', '1999 to 2009', '2009 to 2019', '2019 to 2021'),
                               align = 'lcccc')
# insert SDI before SDI region


write(markdown_table,
      './outcome/appendix/table 3.md')

df_aapc_dalys_table <- df_aapc |>
  filter(Measure == 'DALYs') |>
  select(-Measure) |>
  pivot_wider(names_from = Year, values_from = `AAPC (95%CI)`) |>
  select(location_name, `1990~1999`, `1999~2009`, `2009~2019`, `2019~2021`) |>
  mutate(location_name = factor(location_name, levels = c(region_name))) |>
  arrange(location_name) |>
  mutate(location_name = str_remove(location_name, ' - WB'))

# save to md file
markdown_table <- knitr::kable(df_aapc_dalys_table,
                               format = "markdown",
                               col.names = c('', '1990 to 1999', '1999 to 2009', '2009 to 2019', '2019 to 2021'),
                               align = 'lcccc')

write(markdown_table,
      './outcome/appendix/table 4.md')
