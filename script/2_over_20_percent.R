
# ratio data --------------------------------------------------------------------

rm(list = ls())

## loading map data
df_map_iso <- read.csv('./data/iso_code.csv')

df_region <- read.csv('./data/geographical.csv')

df_map <- st_read("./data/world.zh.json") |> 
  filter(iso_a3  != "ATA")

## modify map data
### combine 索马里兰 and 索马里
somalia_combined <- df_map |> 
  filter(name %in% c("索马里兰", "索马里")) |>
  summarise(geometry = st_union(geometry)) |> 
  mutate(name = "索马里",
         full_name = "索马里",
         iso_a3 = "SOM",
         iso_a2 = "SO",
         iso_n3 = '706')
df_map <- df_map |> 
  filter(!name %in% c("索马里兰", "索马里")) |>
  bind_rows(somalia_combined)

remove(somalia_combined)

df_age <- read.csv('./data/allage/IHME-GBD_2021_DATA-79cf5de6-1/IHME-GBD_2021_DATA-79cf5de6-1.csv')

# clean data
df_age <- df_age |>
  select(measure_name, location_id, location_name, age_name, metric_name, year, val) |>
  filter(measure_name %in% c('Incidence', 'DALYs (Disability-Adjusted Life Years)'),
         metric_name == 'Number',
         age_name %in% c('All ages', '20+ years'),
         location_id %in% df_map_iso$location_id) |> 
  # calculate the percentage of 20+ years
  group_by(location_id, location_name, measure_name, year) |>
  mutate(val_20 = sum(val[age_name == '20+ years']),
         val_all = sum(val[age_name == 'All ages']),
         val_20_ratio = val_20 / val_all) |> 
  ungroup() |>
  filter(age_name == '20+ years') |>
  select(measure_name, location_id, location_name, year, val_20_ratio) |> 
  rename(val = val_20_ratio)

# get 2021 data
df_incidence_2021 <- df_age |>
  filter(year == 2021, measure_name == 'Incidence')

df_dalys_2021 <- df_age |>
  filter(year == 2021, measure_name == 'DALYs (Disability-Adjusted Life Years)')

# get 2019 data
df_incidence_2019 <- df_age |>
  filter(year == 2019, measure_name == 'Incidence')

df_dalys_2019 <- df_age |>
  filter(year == 2019, measure_name == 'DALYs (Disability-Adjusted Life Years)')

# ratio map -------------------------------------------------------------------

df_names <- c('df_incidence_2021', 'df_dalys_2021', 'df_incidence_2019', 'df_dalys_2019')
legend_names <- c('Percentage of 20+ years\nin Incidence, 2019',
                  'Percentage of 20+ years\nin DALYs, 2019',
                  'Percentage of 20+ years\nin Incidence, 2021',
                  'Percentage of 20+ years\nin DALYs, 2021')
# create legend group
legend_breaks <- pretty(df_age$val, n = 10)

i <- 1
## plot
plot_map <- function(i) {
  # get data
  data <- get(df_names[i]) |> 
    left_join(df_map_iso, by = c("location_id" = "location_id")) |> 
    select(ISO3, val)
  
  # check all location in the map
  check_result <- data$ISO3[!data$ISO3 %in% df_map$iso_a3]
  
  if(length(check_result) > 0) {
    print(paste('Missing location:', check_result))
  }
  
  # join map data by ISO3
  data_map <- df_map |> 
    left_join(data, by = c('iso_a3' = 'ISO3'))
  
  # plot
  fig_base <- ggplot(data = data_map) +
    geom_sf(aes(fill = val)) +
    # add x, y tick labels
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8)) +
    scale_x_continuous(limits = c(-180, 180),
                       expand = c(0, 0)) + 
    scale_y_continuous(limits = c(-60, 75)) +
    scale_fill_gradientn(colors = paletteer_d("MetBrewer::Hiroshige", direction = 1),
                         breaks = legend_breaks,
                         labels = scales::percent_format(accuracy = 1),
                         limits = range(legend_breaks)) +
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
          plot.title.position = 'plot')+
    labs(title = legend_names[i],
         fill = NULL,
         x = NULL,
         y = NULL)
  
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
  
  fig <- plot_grid(fig_main, fig_region, nrow = 2, ncol = 1, rel_heights = c(3, 1.2))
  
  ggsave(paste0('./outcome/appendix/', i, '.png'),
         plot = fig,
         width = 11,
         height = 7.5)
  
  return(i)
}

fig_A <- plot_map(1)
fig_B <- plot_map(2)
fig_C <- plot_map(3)
fig_D <- plot_map(4)
