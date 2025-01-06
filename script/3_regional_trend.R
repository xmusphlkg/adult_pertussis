
# loading packages --------------------------------------------------------

library(tidyverse)

# data --------------------------------------------------------------------

df_raw_number <- read.csv('../data/database/global_regional_number.csv')

df_raw_rate <- read.csv('../data/database/global_regional_rate.csv')

## get incidence, incidence rate, DALYs, DALYs rate
df_global_number <- df_raw_number |> 
  filter(measure_name %in% c('DALYs (Disability-Adjusted Life Years)', 'Incidence'))

df_global_rate <- df_raw_rate |>
  filter(measure_name %in% c('DALYs (Disability-Adjusted Life Years)', 'Incidence'))

df_global <- bind_rows(df_global_number, df_global_rate)

rm(df_raw_number, df_raw_rate, df_global_number, df_global_rate)

## subset data
df_region_number <- df_global |> 
  filter(sex_name == 'Both' & 
           age_name == '20+ years' &
           metric_name == 'Number') |> 
  select(location_name, measure_name, year, val, lower, upper) |> 
  arrange(measure_name, location_name, year)

df_region_number_label <- df_region_number |> 
  filter(year %in% c(1990, 2019, 2021)) |> 
  # format number: romove decimals and add comma
  mutate(across(c(val, lower, upper), ~round(., 0)),
         across(c(val, lower, upper), ~format(., big.mark = ',', trim = TRUE)),
         label = paste0(val, '\n(', lower, '~', upper, ')')) |> 
  select(-val, -lower, -upper)

df_region_rate <- df_global |> 
  filter(sex_name == 'Both' & 
           age_name == '20+ years' &
           metric_name == 'Rate') |>
  select(location_name, measure_name, year, val, lower, upper) |>
  arrange(measure_name, location_name, year)

df_region_rate_label <- df_region_rate |>
  filter(year %in% c(1990, 2019, 2021)) |> 
  # format number: round to 2 decimals
  mutate(across(c(val, lower, upper), ~round(., 2)),
         label = paste0(val, '\n(', lower, '~', upper, ')')) |> 
  select(-val, -lower, -upper)

# global trends -----------------------------------------------------------

## calculate AAPC
calculate_aapc <- function(data, measure, start_year, end_year) {
  data_filtered <- data |> 
    filter(measure_name == measure & year >= start_year & year <= end_year) |> 
    arrange(year)
  
  # get log value
  # data_filtered$log_val <- log(data_filtered$val)
  # model <- lm(log_val ~ year, data = data_filtered)
  # model <- glm(val ~ year, family = poisson(link = "log"), data = data_filtered)
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


df_global_trend <- df_global |> 
  filter(sex_name == 'Both' & age_name == '20+ years') |> 
  arrange(measure_name, year) |> 
  filter(measure_name %in% c('DALYs (Disability-Adjusted Life Years)', 'Incidence'))

## calculate AAPC
var_years <- c(1990, 1999, 2009, 2019, 2021)
var_measures <- c('Incidence', 'DALYs (Disability-Adjusted Life Years)')

df_aapc_incidence <- data.frame(year_start = var_years[-length(var_years)],
                                year_end = var_years[-1],
                                measure = 'Incidence') |> 
  rowwise() |> 
  mutate(aapc_results = map2(year_start, year_end, ~calculate_aapc(df_global_trend, measure, .x, .y))) |> 
  unnest_wider(col = aapc_results) |> 
  # round result
  mutate(across(c(AAPC, CI_lower, CI_upper), ~round(., 2))) |>
  mutate(P_value = case_when(P_value < 0.001 ~ '<0.001',
                             TRUE ~ as.character(round(P_value, 3))))

df_aapc_dalys <- data.frame(year_start = var_years[-length(var_years)],
                            year_end = var_years[-1],
                            measure = 'DALYs (Disability-Adjusted Life Years)') |> 
  rowwise() |> 
  mutate(aapc_results = map2(year_start, year_end, ~calculate_aapc(df_global_trend, measure, .x, .y))) |> 
  unnest_wider(col = aapc_results) |> 
  # round result
  mutate(across(c(AAPC, CI_lower, CI_upper), ~round(., 2))) |>
  mutate(P_value = case_when(P_value < 0.001 ~ '<0.001',
                             TRUE ~ as.character(round(P_value, 3))))

df_aapc <- bind_rows(df_aapc_incidence, df_aapc_dalys) |> 
  mutate(Year = paste0(year_start, '~', year_end),
         Measure = case_when(measure == 'Incidence' ~ 'Incidence',
                             measure == 'DALYs (Disability-Adjusted Life Years)' ~ 'DALYs'),
         `AAPC (95%CI)` = paste0(AAPC, '(', CI_lower, '~', CI_upper, ')'),
         `p value` = P_value)

write.csv(df_aapc,
          '../outcome/table_1_global_aapc.csv',
          row.names = FALSE)

## incidence visualization ------------------------------------------------

data <- filter(df_global_trend, measure_name == 'Incidence')

jp_model <- joinpoint(data,
                      year,
                      val,
                      run_opt = run_opt,
                      export_opt = export_opt)
df_jp_apc <- jp_model$apc |> 
  # round result
  mutate(across(c(apc, apc_95_lcl, apc_95_ucl), ~round(., 2)),
         p_value = as.numeric(p_value),
         p_value_label = case_when(p_value < 0.001 ~ '***',
                                   p_value < 0.01 ~ '**',
                                   p_value < 0.05 ~ '*',
                                   TRUE ~ ''),
         legend = paste0(segment_start, '~', segment_end, '\n',
                         apc, '(', apc_95_lcl, '~', apc_95_ucl, ')', p_value_label))

# get breaks of y axis
breaks <- pretty(c(data$val, data$lower, data$upper))

fig_1 <- ggplot(data)+
  geom_vline(data = df_jp_apc,
             mapping = aes(xintercept = segment_end),
             alpha = 0.5,
             color = 'grey50')+
  geom_rect(data = df_jp_apc,
            aes(xmin = segment_start, xmax = segment_end,
                ymin = 0, ymax = 0.1*max(breaks),
                fill = legend),
            alpha = 0.5)+
  geom_line(mapping = aes(x = year, y = val),
            color = "#00798CFF") +
  geom_ribbon(mapping = aes(ymin = lower, ymax = upper,
                            x = year, y = val),
              fill ="#00798CFF",
              alpha = 0.7)+
  scale_x_continuous(limits = c(1990, 2021),
                     breaks = seq(1990, 2021, 5),
                     expand = expansion(add = c(0, 1))) +
  scale_y_continuous(limit = range(breaks),
                     breaks = breaks,
                     expand = expansion(mult = c(0, 0)),
                     labels = scientific_10) +
  scale_fill_manual(values = paletteer_d("PrettyCols::Lucent"))+
  theme_bw()+
  theme(plot.title.position = 'plot',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="inside",
        legend.position.inside = c(0.01, 0.11),
        legend.justification = c(0, 0),
        legend.key.spacing.y = unit(0.35, 'cm'))+
  guides(fill = guide_legend(nrow = 2))+
  labs(x = NULL,
       y = 'Incidence',
       fill = "APC (95% CI)",
       title = 'A')

## DALYs visualization -----------------------------------------------------

data <- filter(df_global_trend, measure_name == 'DALYs (Disability-Adjusted Life Years)')

jp_model <- joinpoint(data,
                      year,
                      val,
                      run_opt = run_opt,
                      export_opt = export_opt)

df_jp_apc <- jp_model$apc |>
  # round result
  mutate(across(c(apc, apc_95_lcl, apc_95_ucl), ~round(., 2)),
         p_value = as.numeric(p_value),
         p_value_label = case_when(p_value < 0.001 ~ '***',
                                   p_value < 0.01 ~ '**',
                                   p_value < 0.05 ~ '*',
                                   TRUE ~ ''),
         legend = paste0(segment_start, '~', segment_end, '\n',
                         apc, '(', apc_95_lcl, '~', apc_95_ucl, ')', p_value_label))

# get breaks of y axis
breaks <- pretty(c(data$val, data$lower, data$upper))

fig_2 <- ggplot(data)+
  geom_vline(data = df_jp_apc,
             mapping = aes(xintercept = segment_end),
             alpha = 0.5,
             color = 'grey50')+
  geom_rect(data = df_jp_apc,
            aes(xmin = segment_start, xmax = segment_end,
                ymin = 0, ymax = 0.1*max(breaks),
                fill = legend),
            alpha = 0.5)+
  geom_line(mapping = aes(x = year, y = val),
            color = "#00798CFF") +
  geom_ribbon(mapping = aes(ymin = lower, ymax = upper,
                            x = year, y = val),
              fill ="#00798CFF",
              alpha = 0.7)+
  scale_x_continuous(limits = c(1990, 2021),
                     breaks = seq(1990, 2021, 5),
                     expand = expansion(add = c(0, 1))) +
  scale_y_continuous(limit = range(breaks),
                     breaks = breaks,
                     expand = expansion(mult = c(0, 0)),
                     labels = scientific_10) +
  scale_fill_manual(values = paletteer_d("PrettyCols::Lucent"))+
  theme_bw()+
  theme(plot.title.position = 'plot',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="inside",
        legend.position.inside = c(0.5, 0.99),
        legend.justification = c(0.5, 1),
        legend.key.spacing.y = unit(0.35, 'cm'))+
  guides(fill = guide_legend(nrow = 1))+
  labs(x = NULL,
       y = 'DALYs (Disability-Adjusted Life Years)',
       fill = "APC (95% CI)",
       title = 'B')

# save --------------------------------------------------------------------

fig <- fig_1 + fig_2

ggsave('../outcome/fig_1_global_trend.pdf',
       plot = fig,
       width = 12,
       height = 5,
       device = cairo_pdf,
       family = 'Helvetica')
