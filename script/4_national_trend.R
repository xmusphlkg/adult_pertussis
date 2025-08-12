
# loading packages --------------------------------------------------------

# devtools::install_github("DanChaltiel/nih.joinpoint")
library(nih.joinpoint)
library(segmented)
library(tidyverse)
library(patchwork)
library(paletteer)
library(Cairo)
library(sf)
library(openxlsx)

# data --------------------------------------------------------------------

rm(list = ls())

source('./script/function.R')

source('./script/joinpoint_setting.R')

df_raw_incidence <- read.csv('./data/database/incidence_rate_both.csv')

df_raw_dalys <- read.csv('./data/database/dalys_rate_both.csv')

# Load map data
df_map_iso <- read.csv('./data/iso_code.csv')

df_map <- st_read('./data/Map GS(2021)648 - geojson/globalmap.shp',
                  quiet = TRUE)

df_map_border <- st_read('./data/Map GS(2021)648 - geojson/china_border.shp',
                         quiet = TRUE)

df_wb_region <- read.xlsx('./data/CLASS_2025_07_02.xlsx') |> 
  # replace Middle East, North Africa, Afghanistan & Pakistan with Middle East & North Africa
  mutate(Region = ifelse(Region == 'Middle East, North Africa, Afghanistan & Pakistan',
                         'Middle East & North Africa', Region))

## get incidence rate, DALYs rate
df_all_rate <- df_raw_incidence |>
  rbind(df_raw_dalys) |>
  rename(location_id = location) |>
  filter(age_name == '20+ years' &
           location_id %in% df_map_iso$location_id) |> 
  mutate(# replace DALYs (Disability-Adjusted Life Years) with DALYs
         measure_name = str_replace(measure_name, 'DALYs \\(Disability-Adjusted Life Years\\)', 'DALYs')) |> 
  select(location_name, measure_name, year, val, lower, upper)

rm(df_raw_incidence, df_raw_dalys)

df_incidence_2021 <- df_all_rate |>
  filter(year == 2021 & measure_name == 'Incidence')

df_dalys_2021 <- df_all_rate |>
  filter(year == 2021 & measure_name == 'DALYs')

df_raw_incidence <- read.csv('./data/database/incidence_number_both.csv')

df_raw_dalys <- read.csv('./data/database/dalys_number_both.csv')

## get incidence number, DALYs number
df_all_number <- df_raw_incidence |>
  rbind(df_raw_dalys) |>
  rename(location_id = location) |>
  filter(age_name == '20+ years' &
           location_id %in% df_map_iso$location_id) |> 
  mutate(# replace DALYs (Disability-Adjusted Life Years) with DALYs
         measure_name = str_replace(measure_name, 'DALYs \\(Disability-Adjusted Life Years\\)', 'DALYs')) |> 
  select(location_name, measure_name, year, val, lower, upper)

rm(df_raw_incidence, df_raw_dalys)

df_global_number_incidence <- df_all_number |> 
  filter(measure_name == 'Incidence') |> 
  select(year, location_name, val, lower, upper)

df_global_number_dalys <- df_all_number |>
  filter(measure_name == 'DALYs') |> 
  select(year, location_name, val, lower, upper)

df_global_rate_incidence <- df_all_rate |>
  filter(measure_name == 'Incidence') |> 
  select(year, location_name, val, lower, upper)

df_global_rate_dalys <- df_all_rate |>
  filter(measure_name == 'DALYs') |> 
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

# AAPC -----------------------------------------------------------

df_aapc <- rbind(
  get_aapc(model_number_incidence) |>  mutate(Label = 'Incidence', Measure = 'Number'),
  get_aapc(model_number_dalys) |>  mutate(Label = 'DALYs', Measure = 'Number'),
  get_aapc(model_rate_incidence) |>  mutate(Label = 'Incidence', Measure = 'Rate'),
  get_aapc(model_rate_dalys) |>  mutate(Label = 'DALYs', Measure = 'Rate')
) |> 
  mutate(aapc = as.numeric(aapc))

write.csv(df_aapc,
          './outcome/national_aapc.csv',
          row.names = FALSE)

df_aapc <- read.csv('./outcome/national_aapc.csv')

df_incidence_aapc <- df_aapc |> 
  filter(Label == 'Incidence', Measure == 'Rate', Year == '1990~2019') |> 
  select(location_name, val = aapc)

df_dalys_aapc <- df_aapc |>
  filter(Label == 'DALYs', Measure == 'Rate', Year == '1990~2019') |>
  select(location_name, val = aapc)

# visualization -----------------------------------------------------------

df_names <- c('df_incidence_2021', 'df_dalys_2021', 'df_incidence_aapc', 'df_dalys_aapc')
legend_names <- c('Incidence rate (per 100,000), 2021',
                  'DALYs rate (per 100,000), 2021',
                  'AAPC of incidence rate, 1990-2019',
                  'AAPC of DALYs rate, 1990-2019')

## fig 2---------------------------------------------------------------------

plot_rate <- function(i, title){
  # get data
  data <- get(df_names[i]) |> 
    left_join(df_map_iso, by = c("location_name" = "location_name_1")) |> 
    left_join(df_wb_region, by = c("ISO3" = "Code")) |> 
    # convert Region to factor
    mutate(Region = factor(Region, levels = c('East Asia & Pacific',
                                              'Europe & Central Asia',
                                              'Latin America & Caribbean',
                                              'Middle East & North Africa',
                                              'North America',
                                              'South Asia',
                                              'Sub-Saharan Africa')))
  
  # add: Cook Islands, Niue, Tokelau
  data$Region[data$ISO3 %in% c('COK', 'NIU', 'TKL')] <- 'East Asia & Pacific'
  
  # breaks
  breaks <- pretty(data$val, n = 5)
  
  fig1 <- ggplot(data) +
    geom_jitter(aes(y = Region, x = val, color = val), height = 0.2, width = 0)+
    scale_color_gradientn(colors = paletteer_d("MetBrewer::Hiroshige", direction = -1),
                          limits = range(breaks),
                          breaks = breaks)  +
    scale_x_continuous(expand = expansion(mult = c(0, 0)),
                       limits = range(breaks),
                       breaks = breaks) +
    scale_y_discrete(limits = rev(levels(data$Region))) +
    theme_bw()+
    theme(plot.title.position = 'plot',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "right",
          legend.title = element_text(angle = 90),
          legend.title.position = 'left')+
    labs(y = NULL, x = legend_names[i],
         color = legend_names[i],
         title = LETTERS[title])+
    guides(color = guide_colorbar(barheight = 12))
  
  fig2 <- data |> 
    # find top 20 countries
    slice_max(val, n = 20) |>
    ungroup() |>
    ggplot(aes(y = reorder(Economy, val), x = val, fill = Region)) +
    geom_col(show.legend = T) +
    scale_x_continuous(expand = expansion(mult = c(0, 0)),
                       limits = range(breaks),
                       breaks = breaks) +
    scale_fill_manual(values = paletteer_d("ggsci::nrc_npg"),
                      drop = F) +
    theme_bw()+
    theme(plot.title.position = 'plot',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")+
    labs(y = NULL, x = legend_names[i],
         color = legend_names[i],
         title = LETTERS[title+1])+
    guides(fill = guide_legend(nrow = 1, byrow = TRUE))
  
  return(list(fig1, fig2))
}

fig_A <- plot_rate(1, 1)
fig_B <- plot_rate(2, 3)

fig_1 <- fig_A[[1]] + fig_B[[1]] +
  plot_layout(ncol = 2)

fig_2 <- fig_A[[2]] + fig_B[[2]] +
  plot_layout(ncol = 2, guides = 'collect')&
  theme(legend.position = 'bottom',
        legend.justification = 'center')

ggsave('./outcome/fig_2_national_trend.pdf',
       plot = cowplot::plot_grid(fig_1, fig_2, ncol = 1, rel_heights = c(1.4, 2)),
       width = 14,
       height = 8,
       device = cairo_pdf,
       family = 'Helvetica')

## fig 3---------------------------------------------------------------------

plot_aapc <- function(i, title){
  # get data
  data <- get(df_names[i]) |> 
    left_join(df_map_iso, by = c("location_name" = "location_name_1")) |> 
    left_join(df_wb_region, by = c("ISO3" = "Code")) |> 
    # convert Region to factor
    mutate(Region = factor(Region, levels = c('East Asia & Pacific',
                                              'Europe & Central Asia',
                                              'Latin America & Caribbean',
                                              'Middle East & North Africa',
                                              'North America',
                                              'South Asia',
                                              'Sub-Saharan Africa')))
  
  # add: Cook Islands, Niue, Tokelau
  data$Region[data$ISO3 %in% c('COK', 'NIU', 'TKL')] <- 'East Asia & Pacific'
  
  # fill Economy with location_name
  data$Economy[is.na(data$Economy)] <- data$location_name[is.na(data$Economy)]
  
  # breaks
  breaks <- seq(-20, 10, 5)
  
  fig1 <- ggplot(data) +
    geom_jitter(aes(y = Region, x = val, color = val), height = 0.2, width = 0)+
    scale_color_gradientn(colors = paletteer_d("MetBrewer::Hiroshige", direction = -1),
                          limits = range(breaks),
                          breaks = breaks)  +
    scale_x_continuous(expand = expansion(mult = c(0, 0)),
                       limits = range(breaks),
                       breaks = breaks) +
    scale_y_discrete(limits = rev(levels(data$Region))) +
    theme_bw()+
    theme(plot.title.position = 'plot',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "right",
          legend.title = element_text(angle = 90),
          legend.title.position = 'left')+
    labs(y = NULL, x = legend_names[i],
         color = legend_names[i],
         title = LETTERS[title])+
    guides(color = guide_colorbar(barheight = 12))
  
  fig2 <- data |> 
    # find top 10 countries
    slice_max(val, n = 10) |>
    ungroup() |>
    ggplot(aes(y = reorder(Economy, val), x = val, fill = Region)) +
    geom_col(show.legend = T) +
    scale_x_continuous(expand = expansion(mult = c(0, 0)),
                       limits = range(breaks),
                       breaks = breaks) +
    scale_fill_manual(values = paletteer_d("ggsci::nrc_npg"),
                      drop = F) +
    theme_bw()+
    theme(plot.title.position = 'plot',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    labs(y = NULL, x = legend_names[i],
         color = legend_names[i],
         title = LETTERS[title+1])+
    guides(fill = guide_legend(nrow = 1, byrow = TRUE))
  
  
  fig3 <- data |> 
    # find top 10 countries
    slice_min(val, n = 10) |>
    ungroup() |>
    ggplot(aes(y = reorder(Economy, val), x = val, fill = Region)) +
    geom_col(show.legend = T) +
    scale_x_continuous(expand = expansion(mult = c(0, 0)),
                       limits = range(breaks),
                       breaks = breaks) +
    scale_fill_manual(values = paletteer_d("ggsci::nrc_npg"),
                      drop = F) +
    theme_bw()+
    theme(plot.title.position = 'plot',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    labs(y = NULL, x = legend_names[i],
         color = legend_names[i],
         title = LETTERS[title+2])+
    guides(fill = guide_legend(nrow = 1, byrow = TRUE))
  
  return(list(fig1, fig2, fig3))
}

fig_C <- plot_aapc(3, 1)
fig_D <- plot_aapc(4, 4)

# save plot -----------------------------------------------------------

fig_1 <- fig_C[[1]] + fig_D[[1]] +
  plot_layout(ncol = 2)

fig_2 <- fig_C[[2]] + fig_C[[3]] +
  fig_D[[2]] + fig_D[[3]] +
  plot_layout(ncol = 2, heights = c(1, 1), byrow = F, guides = 'collect')&
  theme(legend.position = 'bottom',
        legend.justification = 'center')

ggsave('./outcome/fig_3_national_trend.pdf',
       plot = cowplot::plot_grid(fig_1, fig_2, ncol = 1, rel_heights = c(1, 2)),
       width = 14,
       height = 10,
       device = cairo_pdf,
       family = 'Helvetica')

write.xlsx(list('Incidence rate' = df_incidence_2021,
                'DALYs rate' = df_dalys_2021),
           './outcome/fig_2_national_trend.xlsx')

write.xlsx(list('AAPC of incidence' = df_incidence_aapc,
                'AAPC of DALYs' = df_dalys_aapc),
           './outcome/fig_3_national_trend.xlsx')
