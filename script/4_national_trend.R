
# loading packages --------------------------------------------------------

# devtools::install_github("DanChaltiel/nih.joinpoint")
library(nih.joinpoint)
library(segmented)
library(tidyverse)
library(patchwork)
library(paletteer)
library(Cairo)
library(sf)

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
)

df_incidence_aapc <- df_aapc |> 
  filter(measure_name == 'Incidence') |> 
  select(location_name, val = AAPC)

df_dalys_aapc <- df_aapc |>
  filter(measure_name == 'DALYs') |>
  select(location_name, val = AAPC)

# visualization -----------------------------------------------------------

df_names <- c('df_incidence_2021', 'df_dalys_2021', 'df_incidence_aapc', 'df_dalys_aapc')
legend_names <- c('Incidence rate\n(per 100,000), 2021',
                  'DALYs rate\n(per 100,000), 2021',
                  'AAPC of incidence,\n1990-2019',
                  'AAPC of DALYs,\n1990-2019')

i <- 1

## plot
plot_map <- function(i, title) {
  # get data
  data <- get(df_names[i]) |> 
    left_join(df_map_iso, by = c("location_name" = "location_name_1"))
  
  # check all location in the map
  check_result <- data$ISO3[!data$ISO3 %in% df_map$iso_a3]
  
  if(length(check_result) > 0) {
    print(paste('Missing location:', check_result))
  }

  # create legend group
  legend_breaks <- seq(from = pretty(data$val, n = 5)[1],
                       by = ceiling(diff(range(data$val, na.rm = T))/10),
                       length.out = 10)
  data$val_group <- cut(data$val,
                           breaks = legend_breaks,
                           right = FALSE,
                           include.lowest = TRUE)
  
  # join map data by ISO3
  data_map <- df_map |> 
    left_join(data, by = c('iso_a3' = 'ISO3'))
  
  # plot
  fig_base <- ggplot(data = data_map) +
    geom_sf(aes(fill = val_group)) +
    # add x, y tick labels
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8)) +
    scale_x_continuous(limits = c(-180, 180),
                       expand = c(0, 0)) + 
    scale_y_continuous(limits = c(-60, 75)) +
    scale_fill_manual(values = paletteer_d("MetBrewer::Hiroshige", direction = -1),
                      na.translate = FALSE,
                      na.value = 'white') +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())
  
  fig_main <- fig_base +
    theme(panel.border = element_blank(),
          legend.position = 'inside',
          legend.position.inside = c(0.01, 0.01),
          legend.justification = c(0, 0),
          plot.title = element_text(size = 30),
          plot.title.position = 'plot')+
    labs(title = title,
         fill = legend_names[i],
         x = NULL,
         y = NULL)+
    guides(fill = guide_legend(override.aes = list(color = NA)))
  
  # regional map
  for (j in 1:nrow(df_region)) {
    fig_region <- fig_base +
      coord_sf(xlim = c(df_region$Longitude.Min[j], df_region$Longitude.Max[j]),
                ylim = c(df_region$Latitude.Min[j], df_region$Latitude.Max[j]),
               expand = F) +
      theme(legend.position = 'none',
            plot.title = element_text(size = 10)) +
      labs(title = df_region$Area.Name[j])
    
    assign(paste0('fig_region_', j), fig_region)
    rm(fig_region)
  }
  
  # combine plot
  fig_region_567 <- plot_grid(plot_grid(fig_region_5, fig_region_6,
                                        nrow = 1),
                              fig_region_7,
                              ncol = 1)
  
  fig_region <- plot_grid(fig_region_1, fig_region_2, fig_region_3,
                          fig_region_4, fig_region_567,
                          nrow = 1)
  
  plot_grid(fig_main, fig_region, nrow = 2, ncol = 1, rel_heights = c(3, 1.2))
}

fig_A <- plot_map(1, LETTERS[1])
fig_B <- plot_map(2, LETTERS[2])
fig_C <- plot_map(3, LETTERS[1])
fig_D <- plot_map(4, LETTERS[2])

# save plot -----------------------------------------------------------

ggsave('./outcome/fig_2_national_trend.pdf',
       plot = fig_A + fig_B,
       width = 22,
       height = 8,
       device = cairo_pdf,
       family = 'Helvetica')

ggsave('./outcome/fig_3_national_trend.pdf',
       plot = fig_C + fig_D,
       width = 22,
       height = 8,
       device = cairo_pdf,
       family = 'Helvetica')

write.xlsx(list('Incidence rate' = df_incidence_2021,
                'DALYs rate' = df_dalys_2021),
           './outcome/fig_2_national_trend.xlsx')

write.xlsx(list('AAPC of incidence' = df_incidence_aapc,
                'AAPC of DALYs' = df_dalys_aapc),
           './outcome/fig_3_national_trend.xlsx')
