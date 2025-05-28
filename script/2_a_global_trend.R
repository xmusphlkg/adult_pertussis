
# loading packages --------------------------------------------------------

# devtools::install_github("DanChaltiel/nih.joinpoint")
library(nih.joinpoint)
library(segmented)
library(tidyverse)
library(patchwork)
library(paletteer)
library(Cairo)

source('./script/function.R')

# data --------------------------------------------------------------------

df_number <- read.csv('./data/database/global_regional_number.csv')

df_rate <- read.csv('./data/database/global_regional_rate.csv')

## get global data
df_global_number <- df_number |> 
  filter(location_name == 'Global',
         sex_name == 'Both',
         age_name == '20+ years',
         measure_name %in% c('DALYs (Disability-Adjusted Life Years)', 'Incidence')) |> 
  arrange(measure_name, year) |> 
  mutate_at(vars(val, lower, upper), round)

df_global_number_incidence <- df_global_number |> 
  filter(measure_name == 'Incidence') |> 
  select(year, val, lower, upper)

df_global_number_dalys <- df_global_number |>
  filter(measure_name == 'DALYs (Disability-Adjusted Life Years)') |> 
  select(year, val, lower, upper)

df_global_rate <- df_rate |> 
  filter(location_name == 'Global',
         sex_name == 'Both',
         age_name == '20+ years',
         measure_name %in% c('DALYs (Disability-Adjusted Life Years)', 'Incidence')) |> 
  arrange(measure_name, year)

df_global_rate_incidence <- df_global_rate |>
  filter(measure_name == 'Incidence') |> 
  select(year, val, lower, upper)

df_global_rate_dalys <- df_global_rate |>
  filter(measure_name == 'DALYs (Disability-Adjusted Life Years)') |> 
  select(year, val, lower, upper)

# model -----------------------------------------------------------------------

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
                                    run_opt = run_opt,
                                    export_opt = export_opt_new)

model_number_dalys <- joinpoint(df_global_number_dalys,
                                year,
                                val,
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
                                  run_opt = run_opt,
                                  export_opt = export_opt_new)

model_rate_dalys <- joinpoint(df_global_rate_dalys,
                              year,
                              val,
                              run_opt = run_opt,
                              export_opt = export_opt_new)

# APC -------------------------------------------------

# fig1 for incidence and DALYs rate
fig_a <- plot_apc(model_rate_incidence, df_global_rate_incidence)+
  guides(fill = guide_legend(ncol=4, byrow=TRUE))+
  labs(x=NULL,
       y='Number of cases',
       fill="APC (95% CI)",
       title='A')

fig_b <- plot_apc(model_rate_dalys, df_global_rate_dalys)+
  guides(fill = guide_legend(ncol=4, byrow=TRUE))+
  labs(x=NULL,
       y='Number of DALYs',
       fill="APC (95% CI)",
       title='B')

fig <- fig_a + fig_b

ggsave('./outcome/fig_1_global_trend.pdf',
       plot = fig,
       width = 14,
       height = 5,
       device = cairo_pdf,
       family = 'Helvetica')

# fig s1 for incidence and DALYs number
fig_s1_a <- plot_apc(model_number_incidence, df_global_number_incidence)+
  guides(fill = guide_legend(ncol=4, byrow=TRUE))+
  labs(x=NULL,
       y='Number of cases',
       fill="APC (95% CI)",
       title='A')

fig_s1_b <- plot_apc(model_number_dalys, df_global_number_dalys)+
  guides(fill = guide_legend(ncol=4, byrow=TRUE))+
  labs(x=NULL,
       y='Number of DALYs',
       fill="APC (95% CI)",
       title='B')

fig_s1 <- fig_s1_a + fig_s1_b

ggsave('./outcome/appendix/fig_s1.png',
       plot = fig_s1,
       width = 14,
       height = 5)

rm(fig, fig_a, fig_b, fig_s1, fig_s1_a, fig_s1_b)

# AAPC ------------------------------------------------------------------------

df_aapc_incidence <- get_aapc(model_number_incidence) |> 
  mutate(var = 'Incidence')

df_aapc_dalys <- get_aapc(model_number_dalys) |> 
  mutate(var = 'DALYs')

df_aapc <- rbind(df_aapc_incidence, df_aapc_dalys)

# save to md file
markdown_table <- df_aapc |> 
  select(var, Year, Value, p_value) |>
  mutate(p_value = case_when(p_value < 0.001 ~ '<0.001',
                             TRUE ~ formatC(p_value, format = "f", digits = 2))) |> 
  pivot_wider(names_from = var,
              values_from = c(Value, p_value)) |>
  select(Year, Value_Incidence, p_value_Incidence,
         Value_DALYs, p_value_DALYs) |> 
  filter(Year != '1990~2019') |> 
  knitr::kable(format = "markdown",
               col.names = c('Year', 'Incidence, AAPC (95%CI)', 'p value', 'DALYs, AAPC (95%CI)', 'p value'))

write(markdown_table, './outcome/appendix/table_s1_global_trend.md')

write.csv(df_aapc |> 
            mutate(Measure = 'Number'),
          './outcome/appendix/table_s1_global_trend.csv',
          row.names = FALSE)

rm(df_aapc_incidence, df_aapc_dalys, df_aapc)

# Appendix -------------------------------------------------------------------

source('./script/2_b_age_trend.R')

source('./script/2_c_over_20_percent.R')
