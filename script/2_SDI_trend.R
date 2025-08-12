
# loading packages --------------------------------------------------------

# devtools::install_github("DanChaltiel/nih.joinpoint")
library(nih.joinpoint)
library(segmented)
library(tidyverse)
library(patchwork)
library(paletteer)
library(Cairo)

# data --------------------------------------------------------------------

rm(list = ls())

source('./script/function.R')

source('./script/joinpoint_setting.R')

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

df_global_number_incidence <- df_global_number |> 
  filter(measure_name == 'Incidence') |> 
  select(year, location_name, val, lower, upper)

df_global_number_dalys <- df_global_number |>
  filter(measure_name == 'DALYs (Disability-Adjusted Life Years)') |> 
  select(year, location_name, val, lower, upper)

df_global_rate <- df_rate |> 
  filter(sex_name == 'Both',
         age_name == '20+ years',
         location_name %in% region_name,
         measure_name %in% c('DALYs (Disability-Adjusted Life Years)', 'Incidence')) |>
  arrange(measure_name, location_name, year)

df_global_rate_incidence <- df_global_rate |>
  filter(measure_name == 'Incidence') |> 
  select(year, location_name, val, lower, upper)

df_global_rate_dalys <- df_global_rate |>
  filter(measure_name == 'DALYs (Disability-Adjusted Life Years)') |> 
  select(year, location_name, val, lower, upper)

# model -------------------------------------------------------------------

## build joinpoint model for number
model_number_incidence <- joinpoint(df_global_number_incidence,
                                    year,
                                    val,
                                    by = location_name,
                                    run_opt = run_opt_number,
                                    export_opt = export_opt_new)

model_number_dalys <- joinpoint(df_global_number_dalys,
                                year,
                                val,
                                by = location_name,
                                run_opt = run_opt_number,
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
                                  by = location_name,
                                  run_opt = run_opt_rate,
                                  export_opt = export_opt_new)

model_rate_dalys <- joinpoint(df_global_rate_dalys,
                              year,
                              val,
                              by = location_name,
                              run_opt = run_opt_rate,
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
  select(Measure, Year, location_name, `AAPC (95%CI)`) |>
  pivot_wider(names_from = Year, values_from = `AAPC (95%CI)`) |> 
  select(Measure, location_name, `1990~1999`, `1999~2009`, `2009~2019`, `2019~2021`, `1990~2021`) |>
  mutate(location_name = factor(location_name, levels = c(region_name))) |> 
  arrange(Measure, location_name) |>
  knitr::kable(format = "markdown",
               col.names = c('Measure', 'Region', '1990 to 1999', '1999 to 2009', '2009 to 2019', '2019 to 2021', '1990 to 2021'),
               align = 'lcccc')

write(df_aapc_incidence_table,
      './outcome/appendix/table_s6_region_group.md')

write.csv(df_aapc,
          './outcome/appendix/table_s6_region_group.csv',
          row.names = FALSE)

df_aapc_dalys_table <- df_aapc |> 
  filter(Label == 'DALYs') |> 
  mutate(`AAPC (95%CI)` = paste0(Value, p_value_label)) |> 
  select(Measure, Year, location_name, `AAPC (95%CI)`) |>
  pivot_wider(names_from = Year, values_from = `AAPC (95%CI)`) |> 
  select(Measure, location_name, `1990~1999`, `1999~2009`, `2009~2019`, `2019~2021`, `1990~2021`) |>
  mutate(location_name = factor(location_name, levels = c(region_name))) |> 
  arrange(Measure, location_name) |>
  knitr::kable(format = "markdown",
               col.names = c('Measure', 'Region', '1990 to 1999', '1999 to 2009', '2009 to 2019', '2019 to 2021', '1990 to 2021'),
               align = 'lcccc')

write(df_aapc_dalys_table,
      './outcome/appendix/table_s7_region_group.md')

# visualization -----------------------------------------------------------

sdi_name <- region_name |> 
  str_subset('SDI')

reg_name <- region_name |> 
  str_subset('- WB')

fig_incidence <- lapply(sdi_name, plot_val,
                        data = df_global_rate,
                        measure = 'Incidence',
                        filter_col = 'location_name',
                        ylab = 'Incidence rate')
fig_incidence <- patchwork::wrap_plots(fig_incidence, ncol = 3)

fig_dalys <- lapply(sdi_name, plot_val,
                    data = df_global_rate,
                    measure = 'DALYs (Disability-Adjusted Life Years)',
                    filter_col = 'location_name',
                    ylab = 'DALYs rate')
fig_dalys <- patchwork::wrap_plots(fig_dalys, ncol = 3)

ggsave(paste0('./outcome/appendix/fig_s10.png'),
       plot = fig_incidence / fig_dalys,
       width = 11,
       height = 11)

fig_incidence <- lapply(reg_name, plot_val,
                        data = df_global_rate,
                        measure = 'Incidence',
                        filter_col = 'location_name',
                        ylab = 'Incidence rate')
fig_incidence <- patchwork::wrap_plots(fig_incidence, ncol = 3)

fig_dalys <- lapply(reg_name, plot_val,
                    data = df_global_rate,
                    measure = 'DALYs (Disability-Adjusted Life Years)',
                    filter_col = 'location_name',
                    ylab = 'DALYs rate')
fig_dalys <- patchwork::wrap_plots(fig_dalys, ncol = 3)

ggsave(paste0('./outcome/appendix/fig_s11.png'),
       plot = fig_incidence / fig_dalys,
       width = 11,
       height = 14)
