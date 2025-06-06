
# loading packages --------------------------------------------------------

# devtools::install_github("DanChaltiel/nih.joinpoint")
library(nih.joinpoint)
library(segmented)
library(tidyverse)
library(patchwork)
library(paletteer)
library(Cairo)

source('./script/function.R')

source('./script/joinpoint_setting.R')

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
model_number_incidence <- joinpoint(df_global_number_incidence,
                                    year,
                                    val,
                                    run_opt = run_opt_number,
                                    export_opt = export_opt_new)

model_number_dalys <- joinpoint(df_global_number_dalys,
                                year,
                                val,
                                run_opt = run_opt_number,
                                export_opt = export_opt_new)

## build joinpoint model for rate
model_rate_incidence <- joinpoint(df_global_rate_incidence,
                                  year,
                                  val,
                                  run_opt = run_opt_rate,
                                  export_opt = export_opt_new)

model_rate_dalys <- joinpoint(df_global_rate_dalys,
                              year,
                              val,
                              run_opt = run_opt_rate,
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

df_aapc_incidence_number <- get_aapc(model_number_incidence) |> 
  mutate(var = 'Incidence', 
         Measure = 'Number')

df_aapc_dalys_number <- get_aapc(model_number_dalys) |> 
  mutate(var = 'DALYs',
         Measure = 'Number')

df_aapc_incidence_rate <- get_aapc(model_rate_incidence) |> 
  mutate(var = 'Incidence',
         Measure = 'Rate')

df_aapc_dalys_rate <- get_aapc(model_rate_dalys) |>
  mutate(var = 'DALYs',
         Measure = 'Rate')

df_aapc <- rbind(df_aapc_incidence_number,
                 df_aapc_dalys_number,
                 df_aapc_incidence_rate,
                 df_aapc_dalys_rate)

# save to md file
markdown_table <- df_aapc |> 
  filter(Measure == 'Number') |>
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

write.csv(df_aapc,
          './outcome/appendix/table_s1_global_trend.csv',
          row.names = FALSE)

rm(df_aapc_incidence, df_aapc_dalys, df_aapc)

# Appendix -------------------------------------------------------------------

source('./script/2_b_age_trend.R')

source('./script/2_c_over_20_percent.R')
