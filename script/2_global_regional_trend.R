
# loading packages --------------------------------------------------------

library(tidyverse)
library(patchwork)

# data --------------------------------------------------------------------

df_raw <- read.csv('../data/database/global_regional_number.csv')

## get global data
df_global <- df_raw |> 
  filter(location_name == 'Global')

## get regional data
df_regional <- df_raw |> 
  filter(location_name != 'Global')

## fig axis
scientific_10 <- function(x) {
  ifelse(x == 0, 0, parse(text = gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x)))))
}

# global trends -----------------------------------------------------------

df_global_trend <- df_global |> 
  filter(sex_name == 'Both' & age_name == '20+ years') |> 
  arrange(measure_name, year) |> 
  filter(measure_name %in% c('DALYs (Disability-Adjusted Life Years)', 'Incidence'))

## incidence visualization ------------------------------------------------

data <- filter(df_global_trend, measure_name == 'Incidence')

fig_1 <- ggplot(data,
                mapping = aes(x = year, y = val))+
  geom_point() +
  geom_pointrange(mapping = aes(ymin = lower,
                                ymax = upper))+
  scale_x_continuous(limits = c(1990, 2021),
                     expand = expansion(add = c(1, 1))) +
  scale_y_continuous(limit = range(pretty(c(data$val, data$lower, data$upper))),
                     breaks = pretty(c(data$val, data$lower, data$upper)),
                     expand = expansion(mult = c(0, 0)),
                     labels = scientific_10) +
  theme_classic()+
  theme(plot.title.position = 'plot')+
  labs(x = NULL,
       y = 'Incidence',
       title = 'A')

data <- filter(df_global_trend, measure_name == 'DALYs (Disability-Adjusted Life Years)')

fig_2 <- ggplot(data,
                mapping = aes(x = year, y = val))+
  geom_point() +
  geom_pointrange(mapping = aes(ymin = lower,
                                ymax = upper))+
  scale_x_continuous(limits = c(1990, 2021),
                     expand = expansion(add = c(1, 1))) +
  scale_y_continuous(limit = range(pretty(c(data$val, data$lower, data$upper))),
                     breaks = pretty(c(data$val, data$lower, data$upper)),
                     expand = expansion(mult = c(0, 0)),
                     labels = scientific_10) +
  theme_classic()+
  theme(plot.title.position = 'plot')+
  labs(x = NULL,
       y = 'DALYs (Disability-Adjusted Life Years)',
       title = 'B')

fig_1 + fig_2





