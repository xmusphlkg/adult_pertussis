

library(tidyverse)

# data --------------------------------------------------------------------

rm(list = ls())

source('./script/function.R')

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

df_global_number_incidence <- df_global_number |>
  filter(measure_name == 'Incidence') |> 
  select(year, age_name, val, lower, upper)

df_global_number_dalys <- df_global_number |>
  filter(measure_name == 'DALYs (Disability-Adjusted Life Years)') |> 
  select(year, age_name, val, lower, upper)

df_global_rate <- df_rate |> 
  filter(location_name == 'Global',
         sex_name == 'Both',
         age_name %in% age_groups,
         measure_name %in% c('DALYs (Disability-Adjusted Life Years)', 'Incidence')) |> 
  arrange(measure_name, age_name, year)

df_global_rate_incidence <- df_global_rate |>
  filter(measure_name == 'Incidence') |> 
  select(year, age_name, val, lower, upper)

df_global_rate_dalys <- df_global_rate |>
  filter(measure_name == 'DALYs (Disability-Adjusted Life Years)') |> 
  select(year, age_name, val, lower, upper)

# visualization -----------------------------------------------------------

fig_incidence <- lapply(age_groups, plot_val,
                        data = df_global_rate,
                        measure = 'Incidence',
                        filter_col = 'age_name',
                        ylab = 'Incidence rate')
fig_incidence <- patchwork::wrap_plots(fig_incidence, ncol = 4)

fig_dalys <- lapply(age_groups, plot_val,
                    data = df_global_rate,
                    filter_col = 'age_name',
                    measure = 'DALYs (Disability-Adjusted Life Years)',
                    ylab = 'DALYs rate')
fig_dalys <- patchwork::wrap_plots(fig_dalys, ncol = 4)

ggsave(paste0('./outcome/appendix/fig_s6.png'),
       plot = fig_incidence,
       width = 11,
       height = 7.5)

ggsave(paste0('./outcome/appendix/fig_s8.png'),
       plot = fig_dalys,
       width = 11,
       height = 7.5)

fig_incidence <- lapply(age_groups, plot_val,
                        data = df_global_number,
                        measure = 'Incidence',
                        filter_col = 'age_name',
                        ylab = 'Incidence number')
fig_incidence <- patchwork::wrap_plots(fig_incidence, ncol = 4)

fig_dalys <- lapply(age_groups, plot_val,
                    data = df_global_number,
                    measure = 'DALYs (Disability-Adjusted Life Years)',
                    filter_col = 'age_name',
                    ylab = 'DALYs number')
fig_dalys <- patchwork::wrap_plots(fig_dalys, ncol = 4)

ggsave(paste0('./outcome/appendix/fig_s7.png'),
       plot = fig_incidence,
       width = 11,
       height = 7.5)

ggsave(paste0('./outcome/appendix/fig_s9.png'),
       plot = fig_dalys,
       width = 11,
       height = 7.5)

# model -------------------------------------------------------------------

## build joinpoint model for number

run_opt = run_options(model="ln",
                      max_joinpoints=5,
                      model_selection_method = 'permutation test',
                      ci_method = 'parametric',
                      dependent_variable_type = 'count',
                      n_cores=parallel::detectCores())
export_opt = export_options(aapc_full_range  = TRUE,
                            export_aapc = TRUE,
                            aapc_start_range1 = 1990,
                            aapc_end_range1 = 1999,
                            aapc_start_range2 = 1999,
                            aapc_end_range2 = 2009,
                            aapc_start_range3 = 2009,
                            aapc_end_range3 = 2019)
export_opt_new <- paste0(
  export_opt,
  "\nAAPC Start Range4=2019",
  "\nAAPC End Range4=2021",
  "\nAAPC Start Range5=1990",
  "\nAAPC End Range5=2019"
)

model_number_incidence <- joinpoint(df_global_number_incidence,
                                    year,
                                    val,
                                    by = age_name,
                                    run_opt = run_opt,
                                    export_opt = export_opt_new)

model_number_dalys <- joinpoint(df_global_number_dalys,
                                year,
                                val,
                                by = age_name,
                                run_opt = run_opt,
                                export_opt = export_opt_new)

## build joinpoint model for rate

run_opt = run_options(model="ln",
                      max_joinpoints=5,
                      model_selection_method = 'permutation test',
                      ci_method = 'parametric',
                      dependent_variable_type = 'crude rate',
                      n_cores=parallel::detectCores())

model_rate_incidence <- joinpoint(df_global_rate_incidence,
                                  year,
                                  val,
                                  by = age_name,
                                  run_opt = run_opt,
                                  export_opt = export_opt_new)

model_rate_dalys <- joinpoint(df_global_rate_dalys,
                              year,
                              val,
                              by = age_name,
                              run_opt = run_opt,
                              export_opt = export_opt_new)

# AAPC --------------------------------------------------------------------

df_aapc <- rbind(
  get_aapc(model_number_incidence) |>  mutate(Label = 'Incidence', Measure = 'Number'),
  get_aapc(model_number_dalys) |>  mutate(Label = 'DALYs', Measure = 'Number'),
  get_aapc(model_rate_incidence) |>  mutate(Label = 'Incidence', Measure = 'Rate'),
  get_aapc(model_rate_dalys) |>  mutate(Label = 'DALYs', Measure = 'Rate')
)

df_aapc_incidence_table <- df_aapc |> 
  filter(Label == 'Incidence') |> 
  mutate(`AAPC (95%CI)` = paste0(Value, p_value_label)) |> 
  select(Measure, Year, age_name, `AAPC (95%CI)`) |>
  pivot_wider(names_from = Year, values_from = `AAPC (95%CI)`) |> 
  select(Measure, age_name, `1990~1999`, `1999~2009`, `2009~2019`, `2019~2021`, `1990~2021`) |> 
  knitr::kable(format = "markdown",
               col.names = c('Measure', 'Age group', '1990 to 1999', '1999 to 2009', '2009 to 2019', '2019 to 2021', '1990 to 2021'),
               align = 'lcccc')

write(df_aapc_incidence_table,
      './outcome/appendix/table_s4_age_group.md')

write.csv(df_aapc,
          './outcome/appendix/table_s4_age_group.csv',
          row.names = FALSE)

df_aapc_dalys_table <- df_aapc |> 
  filter(Label == 'DALYs') |> 
  mutate(`AAPC (95%CI)` = paste0(Value, p_value_label)) |> 
  select(Measure, Year, age_name, `AAPC (95%CI)`) |>
  pivot_wider(names_from = Year, values_from = `AAPC (95%CI)`) |> 
  select(Measure, age_name, `1990~1999`, `1999~2009`, `2009~2019`, `2019~2021`, `1990~2021`) |> 
  knitr::kable(format = "markdown",
               col.names = c('Measure', 'Age group', '1990 to 1999', '1999 to 2009', '2009 to 2019', '2019 to 2021', '1990 to 2021'),
               align = 'lcccc')

write(df_aapc_dalys_table,
      './outcome/appendix/table_s5_age_group.md')

