
# packages ----------------------------------------------------------------

# install.packages("remotes")
# remotes::install_github("tidyverts/fabletools")
# install.packages('fpp3', dependencies = TRUE)

library(tidyverse)
library(openxlsx)
library(forecast)
library(tsibble)
library(fabletools)
library(fpp3)
library(ggh4x)
library(paletteer)
library(sf)
library(cowplot)
library(shadowtext)
library(patchwork)

# data --------------------------------------------------------------------

df_raw_incidence_male <- read.csv('./data/database/incidence_number_male.csv')

df_raw_incidence_female <- read.csv('./data/database/incidence_number_female.csv')

df_raw_dalys_male <- read.csv('./data/database/dalys_number_male.csv')

df_raw_dalys_female <- read.csv('./data/database/dalys_number_female.csv')

df_global <- read.csv('./data/database/global_regional_number.csv') |> 
  filter(location_name == 'Global',
         measure_name %in% c('DALYs (Disability-Adjusted Life Years)', 'Incidence'),
         year > 2019,
         age_name %in% c('20+ years',
                         '20-24 years', '25-29 years', '30-34 years', '35-39 years',
                         '40-44 years', '45-49 years', '50-54 years', '55+ years'),
         sex_name == 'Both') |> 
  select(year, measure_name, age_name, val) |> 
  arrange(year, measure_name, age_name) |> 
  group_by(measure_name, age_name) |>
  summarise(val = sum(val),
            .groups = 'drop') |> 
  mutate(measure_name = recode(measure_name,
                               'DALYs (Disability-Adjusted Life Years)' = 'DALYs',
                               'Incidence' = 'Incidence'),
         age_name = recode(age_name,
                           '20+ years' = 'Total'))

df_map_iso <- read.csv('./data/iso_code.csv')

df_map <- st_read('./data/Map GS(2021)648 - geojson/globalmap.shp',
                  quiet = TRUE)

df_map_border <- st_read('./data/Map GS(2021)648 - geojson/china_border.shp',
                         quiet = TRUE)

df_wb_region <- read.xlsx('./data/CLASS_2025_07_02.xlsx') |> 
  # replace Middle East, North Africa, Afghanistan & Pakistan with Middle East & North Africa
  mutate(Region = ifelse(Region == 'Middle East, North Africa, Afghanistan & Pakistan',
                         'Middle East & North Africa', Region))

df_region_sdi <- read.xlsx('./data/IHME_GBD_SDI_2021_SDI_QUINTILES_Y2024M05D16.xlsx')

## clean SDI data
df_map_iso <- df_map_iso |>
  left_join(filter(df_region_sdi, !is.na(SDI.Quintile)),
            by = c('location_id' = 'Location.ID'))

## get incidence rate, DALYs rate
df_incidence <- rbind(df_raw_incidence_male, df_raw_incidence_female) |>
  rename(location_id = location) |>
  filter(age_name %in% c('20-24 years', '25-29 years', '30-34 years', '35-39 years',
                         '40-44 years', '45-49 years', '50-54 years', '55+ years') &
           location_id %in% df_map_iso$location_id) |> 
  select(location_id, location_name, sex_name, age_name, year, val) |> 
  left_join(select(df_map_iso, location_id, SDI.Quintile), by = 'location_id') |> 
  mutate(SDI.Quintile = if_else(is.na(SDI.Quintile), 'Missing', SDI.Quintile)) |> 
  rename(SDI = SDI.Quintile)

df_dalys <- rbind(df_raw_dalys_male, df_raw_dalys_female) |> 
  rename(location_id = location) |>
  filter(age_name %in% c('20-24 years', '25-29 years', '30-34 years', '35-39 years',
                         '40-44 years', '45-49 years', '50-54 years', '55+ years') &
           location_id %in% df_map_iso$location_id) |> 
  select(location_id, location_name,  sex_name, age_name, year, val) |> 
  left_join(select(df_map_iso, location_id, SDI.Quintile), by = 'location_id') |>
  mutate(SDI.Quintile = if_else(is.na(SDI.Quintile), 'Missing', SDI.Quintile)) |> 
  rename(SDI = SDI.Quintile)

rm(df_raw_incidence_male, df_raw_incidence_female, df_raw_dalys_female, df_raw_dalys_male)

# forecast -------------------------------------------------------------------

## incidence number ----------------------------------------------------------

# trans data from dataframe to tsibble
df_incidence_ts <- df_incidence |> 
  select(-SDI) |>
  as_tsibble(key = c( "location_name", "age_name", "sex_name"),
             index = "year")

df_incidence_ts <- df_incidence_ts |>
  aggregate_key(location_name / age_name / sex_name, val = sum(val))

# forecast
df_incidence_forecast <- df_incidence_ts |>
  filter(year <= 2019) |>
  model(ets = ETS(val, opt_crit = "mae", ic = 'bic')) |> 
  reconcile(ols = min_trace(ets, method = "ols")) |>
  forecast(h = 2)

df_incidence_forecast_ols <- df_incidence_forecast |>
  filter(.model == "ols")

df_incidence_forecast_age <- df_incidence_forecast_ols |>
  as_tibble() |>
  # trans list to character
  mutate_if(is.list, as.character) |>
  # drop aggregated
  filter(location_name != '<aggregated>' & age_name != '<aggregated>') |>
  filter(sex_name == '<aggregated>') |>
  group_by(age_name, year) |> 
  summarise(val = sum(.mean),
            .groups = 'drop')

df_incidence_forecast_total <- df_incidence_forecast_ols |>
  as_tibble() |>
  # trans list to character
  mutate_if(is.list, as.character) |>
  # find total
  filter(location_name == '<aggregated>') |> 
  select(year, val = '.mean') |> 
  mutate(age_name = 'Total')

df_incidence_forecast_total <- bind_rows(df_incidence_forecast_total, df_incidence_forecast_age)
  
df_incidence_forecast_location <- df_incidence_forecast_ols |>
  as_tibble() |>
  # trans list to character
  mutate_if(is.list, as.character) |>
  # drop aggregated
  filter(location_name != '<aggregated>' & age_name == '<aggregated>') |> 
  select(location_name, year, val = '.mean')

## DALYs number ------------------------------------------------------------

# trans data from dataframe to tsiibble
df_dalys_ts <- df_dalys |> 
  select(-SDI) |>
  as_tsibble(key = c( "location_name", "age_name", "sex_name"),
             index = "year")

df_dalys_ts <- df_dalys_ts |>
  aggregate_key(location_name / age_name / sex_name, val = sum(val))

# forecast
df_dalys_forecast <- df_dalys_ts |>
  filter(year <= 2019) |>
  model(ets = ETS(val, opt_crit = "mae", ic = 'bic')) |> 
  reconcile(ols = min_trace(ets, method = "ols")) |>
  forecast(h = 2)

df_dalys_forecast_ols <- df_dalys_forecast |>
  filter(.model == "ols")

df_dalys_forecast_age <- df_dalys_forecast_ols |>
  as_tibble() |>
  # trans list to character
  mutate_if(is.list, as.character) |>
  # drop aggregated
  filter(location_name != '<aggregated>' & age_name != '<aggregated>') |>
  filter(sex_name == '<aggregated>') |>
  group_by(age_name, year) |> 
  summarise(val = sum(.mean),
            .groups = 'drop')

df_dalys_forecast_total <- df_dalys_forecast_ols |>
  as_tibble() |>
  # trans list to character
  mutate_if(is.list, as.character) |>
  # find total
  filter(location_name == '<aggregated>') |> 
  select(year, val = '.mean') |> 
  mutate(age_name = 'Total')

df_dalys_forecast_total <- bind_rows(df_dalys_forecast_total, df_dalys_forecast_age)

df_dalys_forecast_location <- df_dalys_forecast_ols |>
  as_tibble() |>
  # trans list to character
  mutate_if(is.list, as.character) |>
  # drop aggregated
  filter(location_name != '<aggregated>' & age_name == '<aggregated>') |> 
  select(location_name, year, val = '.mean')
  
# save map ------------------------------------------------------------

df_list <- c('incidence', 'dalys')

legend_names <- c('Relative difference of incidence number', 'Relative difference of DALYs number')

i <- 1

## plot
plot_map <- function(data, i) {
  # get data
  data <- data |> 
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
  
  # add: Cook Islands, Niue, Tokelau, Timor-Leste
  data$Region[data$ISO3 %in% c('COK', 'NIU', 'TKL', 'TMP')] <- 'East Asia & Pacific'
  
  # fill Economy with location_name
  data$Economy[is.na(data$Economy)] <- data$location_name[is.na(data$Economy)]
  
  # breaks
  breaks <- seq(0, 1, 0.1)
  
  fig1 <- ggplot(data) +
    geom_jitter(aes(y = Region, x = val, color = val), show.legend = F, height = 0.2, width = 0)+
    scale_color_gradientn(colors = paletteer_d("MetBrewer::Hiroshige", direction = -1),
                          limits = range(breaks),
                          breaks = breaks)  +
    scale_x_continuous(expand = expansion(mult = c(0, 0)),
                       limits = range(breaks),
                       labels = scales::percent_format(),
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
         title = LETTERS[i])
  
  fig2 <- data |> 
    select(ISO3, val, Region) |>
    arrange(Region, ISO3) |>
    # add row number and column number
    mutate(id = 1:n(),
           col = floor((id - 1) / 10),
           row = id - col * 10,
           label = paste0(ISO3, '\n(', scales::percent(val, accuracy = 0.01), ')')) |>
    ggplot(aes(x = col, y = row)) +
    geom_tile(aes(fill = val), show.legend = T) +
    geom_shadowtext(aes(col = Region, label = label), size= 2.3,
                    fontface = 'bold', bg.color = 'white') +
    scale_fill_gradientn(colors = paletteer_d("MetBrewer::Hiroshige", direction = -1),
                         labels = scales::percent_format(),
                         limits = range(breaks),
                         breaks = breaks)+
    scale_color_manual(values = paletteer_d("ggsci::nrc_npg"),
                       drop = F)+
    scale_x_continuous(expand = expansion(mult = c(0, 0)),
                       limits = c(-0.5, 20.5))+
    scale_y_reverse(expand = expansion(mult = c(0, 0)),
                    limits = c(10.5, 0.5))+
    theme_bw()+
    theme(panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          legend.title.position = 'top',
          plot.title.position = 'plot')+
    labs(x = NULL, y = NULL,
         title = LETTERS[i + 2],
         fill = 'Relative difference of number (%)',
         color = 'Region')+
    guides(color = guide_legend(order = 2, nrow = 2, byrow = TRUE, override.aes = list(fill = 'white')),
           fill = guide_colorbar(order = 1, barwidth = 25))
  
  return(list(fig1, fig2))
}

# visual by location
data_total_location_incidence <- df_incidence |> 
  filter(year > 2019) |> 
  group_by(location_id, location_name, year) |>
  summarise(val = sum(val),
            .groups = 'drop') |>
  left_join(get(paste0('df_', df_list[i], '_forecast_location')),
            by = c('location_name', 'year')) |> 
  rename(Forecasted = val.y, Observed = val.x) |> 
  group_by(location_id, location_name) |>
  summarise(Observed = sum(Observed),
            Forecasted = sum(Forecasted),
            .groups = 'drop') |>
  mutate(val = (Forecasted - Observed)/Forecasted,
         color = if_else(val > 0, "Decrease", "Increase")) |> 
  # add location id
  left_join(select(df_map_iso, location_name, location_name_1, SDI.Quintile),
            by = c('location_name' = 'location_name_1'))

# visual by location
data_total_location_dalys <- df_dalys |> 
  filter(year > 2019) |> 
  group_by(location_id, location_name, year) |>
  summarise(val = sum(val),
            .groups = 'drop') |>
  left_join(get(paste0('df_', df_list[i], '_forecast_location')),
            by = c('location_name', 'year')) |> 
  rename(Forecasted = val.y, Observed = val.x) |> 
  group_by(location_id, location_name) |>
  summarise(Observed = sum(Observed),
            Forecasted = sum(Forecasted),
            .groups = 'drop') |>
  mutate(val = (Forecasted - Observed)/Forecasted,
         color = if_else(val > 0, "Decrease", "Increase")) |> 
  # add location id
  left_join(select(df_map_iso, location_name, location_name_1, SDI.Quintile),
            by = c('location_name' = 'location_name_1'))

fig2 <- plot_map(data_total_location_incidence, 1)

fig3 <- plot_map(data_total_location_dalys, 2)

fig_1 <- fig2[[1]] + fig3[[1]] +
  plot_layout(ncol = 2) &
  # add panel right space
  theme(plot.margin = margin(5, 10, 5, 5))

fig_2 <- fig2[[2]] + fig3[[2]] +
  plot_layout(ncol = 1, heights = c(1, 1), byrow = F, guides = 'collect')&
  theme(legend.position = 'bottom',
        legend.justification = 'center')

ggsave('./outcome/fig_5_forecast_trend.pdf',
       plot = cowplot::plot_grid(fig_1, fig_2, ncol = 1, rel_heights = c(1, 2.5)),
       width = 14,
       height = 12,
       device = cairo_pdf,
       family = 'Helvetica')

write.xlsx(list('Incidence' = data_total_location_incidence,
                'DALYs' = data_total_location_dalys),
           './outcome/fig_5_forecast_trend.xlsx',
           asTable = T)

data_total_location_dalys <- read.xlsx('./outcome/fig_5_forecast_trend.xlsx',
                                         sheet = 'DALYs')

data_total_location_incidence <- read.xlsx('./outcome/fig_5_forecast_trend.xlsx',
                                            sheet = 'Incidence')

# save line ----------------------------------------------------------------

legend_names <- c('Incidence number', 'DALYs number')

plot_fun <- function(i){
  ## real data
  data_total_age <- get(paste0('df_', df_list[i])) |> 
    # drop aggregated
    filter(year > 2010) |>
    group_by(age_name, year) |>
    summarise(val = sum(val),
              .groups = 'drop')
  
  data_total <- data_total_age |> 
    group_by(year) |>
    summarise(val = sum(val),
              .groups = 'drop') |>
    mutate(age_name = 'Total') |> 
    bind_rows(data_total_age)
  
  ## forecast data total
  data_total_forecast <- get(paste0('df_', df_list[i], '_forecast_total')) |> 
    # add 2019 point to line
    bind_rows(filter(data_total, year == 2019)) |>
    mutate(type = 'Forecasted')
  
  data_total <- data_total |>
    mutate(type = 'Observed') |> 
    bind_rows(data_total_forecast) |> 
    mutate(age_name = factor(age_name, levels = c('Total', '20-24 years', '25-29 years', '30-34 years',
                                                  '35-39 years', '40-44 years', '45-49 years', '50-54 years',
                                                  '55+ years'))) |> 
    arrange(age_name)
  
  data_total_diff <- data_total |>
    select(age_name, type, year, val) |>
    pivot_wider(names_from = type, values_from = val) |>
    mutate(diff = Forecasted - Observed,
           color = if_else(diff > 0, "Decrease", "Increase"))
  
  ## visual total
  breaks <- pretty(c(0, range(data_total$val)), n = 5)
  
  fig_1 <- ggplot(data = filter(data_total_diff, age_name == 'Total'),
         mapping = aes(x = year)) +
    stat_difference(aes(ymin = Observed, ymax = Forecasted),
                    alpha = 0.3,
                    levels = c("Decreased", "Increased"),
                    show.legend = F) +
    geom_vline(xintercept = 2019, linetype = 'dashed') +
    geom_line(data = filter(data_total, age_name == 'Total'),
              mapping = aes(y = val, color = type),
              show.legend = T) +
    scale_color_manual(values = c('Forecasted' = '#00798CFF', 'Observed' = '#EDAE49FF')) +
    scale_fill_manual(values = c('Increased' = '#D1495BFF', 'Decreased' = '#00A6A6FF')) +
    scale_x_continuous(breaks = seq(2011, 2021, 2)) +
    scale_y_continuous(labels = scientific_10,
                       expand = expansion(mult = c(0, 0)),
                       limits = range(breaks)) +
    theme_bw()+
    theme(panel.grid = element_blank(),
          legend.position = 'inside',
          legend.position.inside = c(0.01, 0.4),
          legend.justification = c(0, 1),
          plot.title.position = 'plot')+
    labs(title = LETTERS[i*2-1],
         color = NULL,
         x = NULL,
         y = legend_names[i])+
    guides(fill = "none", color = guide_legend(title = NULL))
  
  ## visual by age
  breaks <- pretty(c(0, range(data_total$val[data_total$age_name != 'Total'])), n = 5)
  
  fig_2 <- ggplot(data = filter(data_total_diff, age_name != 'Total'),
         mapping = aes(x = year)) +
    stat_difference(aes(ymin = Observed, ymax = Forecasted),
                    alpha = 0.3,
                    levels = c("Decreased", "Increased"),
                    show.legend = F) +
    geom_vline(xintercept = 2019, linetype = 'dashed') +
    geom_line(data = filter(data_total, age_name != 'Total'),
              mapping = aes(y = val, color = type),
              show.legend = F) +
    facet_wrap(~age_name, nrow = 2) +
    scale_color_manual(values = c('Forecasted' = '#00798CFF', 'Observed' = '#EDAE49FF')) +
    scale_fill_manual(values = c('Increased' = '#D1495BFF', 'Decreased' = '#00A6A6FF')) +
    scale_x_continuous(breaks = seq(2011, 2021, 2)) +
    scale_y_continuous(labels = scientific_10,
                       expand = expansion(mult = c(0, 0)),
                       limits = range(breaks)) +
    theme_bw()+
    theme(panel.grid = element_blank(),
          plot.title.position = 'plot')+
    labs(title = LETTERS[i*2],
         x = NULL,
         y = NULL)

  plot_grid(fig_1, fig_2, nrow = 1, rel_widths = c(1, 3.5))
}

fig_1 <- plot_fun(1)

fig_2 <- plot_fun(2)

ggsave('./outcome/fig_4_forecast_trend.pdf',
       plot = plot_grid(fig_1, fig_2, ncol = 1),
       width = 11,
       height = 6,
       device = cairo_pdf,
       family = 'Helvetica')

df_global_compare <- rbind(df_dalys_forecast_total |>
                             mutate(measure_name = 'DALYs'),
                           df_incidence_forecast_total |>
                             mutate(measure_name = 'Incidence')) |> 
  group_by(measure_name, age_name) |>
  summarise(val = sum(val),
            .groups = 'drop') |>
  left_join(df_global, by = c('measure_name', 'age_name')) |> 
  rename(observed = val.y, forecasted = val.x) |>
  mutate(val = (forecasted - observed)/forecasted,
         val = round(val, 4),
         color = if_else(val > 0, "Decrease", "Increase"))


write.xlsx(list('Total' = df_global_compare,
                'Incidence' = data_total_location_incidence,
                'DALYs' = data_total_location_dalys),
           './outcome/fig_4_forecast_trend.xlsx',
           asTable = T)
