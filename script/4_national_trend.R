
# loading packages --------------------------------------------------------

library(MASS)
library(tidyverse)
library(openxlsx)
library(paletteer)
library(cowplot)
library(Cairo)
library(sf)

# data --------------------------------------------------------------------

df_raw_incidence <- read.csv('./data/database/incidence_rate_both.csv')

df_raw_dalys <- read.csv('./data/database/dalys_rate_both.csv')

## loading map data
df_map_iso <- read.csv('./data/iso_code.csv')

df_map <- st_read("./data/world.zh.json") |> 
  filter(iso_a3  != "ATA")

df_region <- read.csv('./data/geographical.csv')

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

## get incidence rate, DALYs rate
df_all <- df_raw_incidence |>
  rbind(df_raw_dalys) |>
  rename(location_id = location) |>
  filter(age_name == '20+ years' &
           location_id %in% df_map_iso$location_id) |> 
  mutate(# replace DALYs (Disability-Adjusted Life Years) with DALYs
         measure_name = str_replace(measure_name, 'DALYs \\(Disability-Adjusted Life Years\\)', 'DALYs')) |> 
  select(location_name, measure_name, year, val, lower, upper)

rm(df_raw_incidence, df_raw_dalys)

df_incidence_2021 <- df_all |>
  filter(year == 2021 & measure_name == 'Incidence')

df_dalys_2021 <- df_all |>
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

# AAPC -----------------------------------------------------------

## calculate AAPC
calculate_aapc <- function(data, measure_set) {
  data_filtered <- data |> 
    filter(measure == measure_set) |>
    arrange(year)
  
  model <- tryCatch({MASS::glm.nb(val ~ year, data = data_filtered)},
    error = function(e) {
      print(paste('Model fitting failed:', measure_set))
      return(NULL)
    }
  )
  
  # if model fitting failed or total number of cases lower than 10
  if (is.null(model) | sum(data_filtered$val) < 10) {
    return(list(AAPC = NA, CI_lower = NA, CI_upper = NA, P_value = NA))
  }
  
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

df_all_trend <- df_all_number |>
  mutate(measure = paste(location_name, measure_name, sep = '--'),
         val = round(val)) |> 
  # get data before 2019
  filter(year <= 2019)

df_aapc <- data.frame(measure = unique(df_all_trend$measure)) |> 
  rowwise() |> 
  mutate(aapc_results = map(measure, ~calculate_aapc(df_all_trend, .x))) |> 
  unnest_wider(col = aapc_results) |> 
  # round result
  mutate(across(c(AAPC, CI_lower, CI_upper), ~round(., 2)),
         P_value = case_when(P_value < 0.001 ~ '<0.001',
                             TRUE ~ as.character(round(P_value, 3))),
         `AAPC (95%CI)` = paste0(AAPC, '<br>(', CI_lower, '~', CI_upper, ')'),
         `p value` = P_value) |> 
  # split measure
  separate(measure, c('location_name', 'measure_name'), sep = '--')

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
