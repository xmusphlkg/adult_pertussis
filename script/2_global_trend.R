
# loading packages --------------------------------------------------------

# devtools::install_github("DanChaltiel/nih.joinpoint")
library(nih.joinpoint)
library(segmented)
library(MASS)
library(tidyverse)
library(patchwork)
library(paletteer)
library(Cairo)

# join point setting
run_opt = run_options(model="ln",
                      max_joinpoints=5,
                      model_selection_method = 'permutation test',
                      ci_method = 'parametric',
                      n_cores=parallel::detectCores())
export_opt = export_options()

# data --------------------------------------------------------------------

df_raw <- read.csv('../data/database/global_regional_number.csv')

## get global data
df_global <- df_raw |> 
  filter(location_name == 'Global')

## fig axis
scientific_10 <- function(x) {
  ifelse(x == 0, 0, parse(text = gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x)))))
}

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
         `p value` = P_value) |> 
  select(Measure, Year, `AAPC (95%CI)`, `p value`) |>
  pivot_wider(names_from = Measure,
              values_from = c(`AAPC (95%CI)`, `p value`)) |> 
  select(Year, `AAPC (95%CI)_Incidence`, `p value_Incidence`,
         `AAPC (95%CI)_DALYs`, `p value_DALYs`)

# save to md file
markdown_table <- knitr::kable(df_aapc,
                               format = "markdown",
                               col.names = c('Year', 'Incidence, AAPC (95%CI)', 'p value', 'DALYs, AAPC (95%CI)', 'p value'))
write(markdown_table, '../outcome/table_1_global_trend.md')

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

# set colors
colors <- paletteer_d("PrettyCols::Lucent", n = nrow(df_jp_apc))
colors <- colors[order(order(df_jp_apc$apc))]

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
  scale_fill_manual(values = colors)+
  theme_bw()+
  theme(plot.title.position = 'plot',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="bottom",
        legend.justification.bottom = 'right',
        legend.title.position = 'top',
        legend.key.spacing.y = unit(0.35, 'cm'))+
  guides(fill = guide_legend(ncol = 3, byrow = TRUE))+
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
breaks <- pretty(c(data$val, data$lower, data$upper*1.2))

# set colors
colors <- paletteer_d("PrettyCols::Lucent", n = nrow(df_jp_apc))
colors <- colors[order(order(df_jp_apc$apc))]

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
  scale_fill_manual(values = colors)+
  theme_bw()+
  theme(plot.title.position = 'plot',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="bottom",
        legend.justification.bottom = 'right',
        legend.title.position = 'top',
        legend.key.spacing.y = unit(0.35, 'cm'))+
  guides(fill = guide_legend(nol = 3))+
  labs(x = NULL,
       y = 'DALYs (Disability-Adjusted Life Years)',
       fill = "APC (95% CI)",
       title = 'B')

# save --------------------------------------------------------------------

fig <- fig_1 / fig_2

ggsave('../outcome/fig_1_global_trend.pdf',
       plot = fig,
       width = 5,
       height = 8.5,
       device = cairo_pdf,
       family = 'Helvetica')
