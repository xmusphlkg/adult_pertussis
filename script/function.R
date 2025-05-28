
## fig axis
scientific_10 <- function(x) {
  ifelse(x == 0, 0, parse(text = gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x)))))
}

## get AAPC from jp_model
get_aapc <- function(jp_model) {
  data <- jp_model$aapc |>
    mutate(across(c(aapc, aapc_c_i_low, aapc_c_i_high), ~formatC(., format = "f", digits = 2)),
           p_value = as.numeric(p_value),
           p_value_label = case_when(p_value < 0.001 ~ '***',
                                     p_value < 0.01 ~ '**',
                                     p_value < 0.05 ~ '*',
                                     TRUE ~ ''),
           legend = paste0(start_obs, '~', end_obs, '\n',
                           aapc, '(', aapc_c_i_low, '~', aapc_c_i_high, ')', p_value_label),
           Year = paste(start_obs, end_obs, sep = '~'),
           Value = paste0(aapc, ' (', aapc_c_i_low, '~', aapc_c_i_high, ')'))
  
  return(data)
}

## visual aapc
plot_apc <- function(jp_model, data) {
  df_jp_apc <- jp_model$apc |>
    # round result
    mutate(across(c(apc, apc_95_lcl, apc_95_ucl), ~round(., 2)),
           p_value = as.numeric(p_value),
           p_value_label = case_when(p_value < 0.001 ~ '***',
                                     p_value < 0.01 ~ '**',
                                     p_value < 0.05 ~ '*',
                                     TRUE ~ ''),
           legend = paste0(segment_start, '~', segment_end, '\n',
                           apc, '(', apc_95_lcl, '~', apc_95_ucl, ')', p_value_label))
  
  # get breaks of y axis
  breaks <- pretty(c(data$val, data$lower, data$upper))
  
  # set colors
  colors <- paletteer_d("PrettyCols::Lucent", n = nrow(df_jp_apc))
  colors <- colors[order(order(df_jp_apc$apc))]
  
  fig <- ggplot(data)+
    geom_vline(data = df_jp_apc,
               mapping = aes(xintercept = segment_end),
               alpha = 0.5,
               color = 'grey50')+
    geom_rect(data = df_jp_apc,
              aes(xmin = segment_start, xmax = segment_end,
                  ymin = 0, ymax = 0.1*max(breaks),
                  fill = legend),
              alpha = 0.5)+
    geom_line(mapping = aes(x = year, y = val),
              color = "#00798CFF") +
    geom_ribbon(mapping = aes(ymin = lower, ymax = upper,
                              x = year, y = val),
                fill ="#00798CFF",
                alpha = 0.7)+
    scale_x_continuous(limits = c(1990, 2021),
                       breaks = seq(1990, 2021, 5),
                       expand = expansion(add = c(0, 1))) +
    scale_y_continuous(limit = range(breaks),
                       breaks = breaks,
                       label = scientific_10,
                       expand = expansion(mult = c(0, 0))) +
    scale_fill_manual(values = colors)+
    theme_bw()+
    theme(plot.title.position = 'plot',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="bottom",
          legend.justification.bottom = 'right',
          legend.title.position = 'top',
          legend.key.spacing.y = unit(0.35, 'cm'))
  
  return(fig)
}

# visualize val
plot_val <- function(data, measure, filter_col, filter_val, ylab) {
  data_filtered <- data |> 
    filter(measure_name == measure)
  
  breaks <- pretty(c(0, range(data_filtered$upper)), n = 5)
  
  data_filtered <- data_filtered |> 
    filter(!!sym(filter_col) == filter_val)
  
  p <- ggplot(data_filtered, aes(x = year, y = val)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    scale_x_continuous(breaks = seq(1990, 2020, 10),
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = breaks,
                       limits = range(breaks),
                       expand = c(0, 0)) +
    labs(title = filter_val,
         x = NULL,
         y = ylab) +
    theme_bw()+
    theme(plot.title.position = 'plot')
  
  return(p)
}
