
plot_contingency_table <- function(data, row, col) {

  tile_plot_count <- data |> 
    count({{row}}, {{col}}) |> 
    mutate(across(c({{row}}, {{col}}), as_factor)) |> 
    ggplot(aes({{col}}, {{row}}, fill = n)) + 
    geom_tile(color = "white", linewidth = 1.5, show.legend = FALSE) +
    geom_text(
      mapping = aes(
        label = n, 
        color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
      size = 3, 
      family = "Avenir") + 
    scale_fill_viridis_c(option = "mako", labels = scales::percent) + 
    theme_minimal(base_line_size = 0, base_family = "Optima") + 
    labs(subtitle = "Counts")
  
  tile_plot_row <- data |> 
    count({{row}}, {{col}}) |> 
    group_by({{row}}) |> 
    mutate(prop = n / sum(n)) |> 
    ungroup() |> 
    mutate(across(c({{row}}, {{col}}), as_factor)) |> 
    ggplot(aes({{col}}, {{row}}, fill = prop)) + 
    geom_tile(color = "white", linewidth = 1.5, show.legend = FALSE) +
    geom_text(
      mapping = aes(
        label = scales::percent(prop, accuracy = 0.2), 
        color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
      size = 3, 
      family = "Avenir") + 
    scale_fill_viridis_c(option = "mako", labels = scales::percent) + 
    theme_minimal(base_line_size = 0, base_family = "Optima") + 
    labs(subtitle = "Row Percentages")
  
  tile_plot_col <- data |> 
    count({{row}}, {{col}}) |> 
    group_by({{col}}) |> 
    mutate(prop = n / sum(n)) |> 
    ungroup() |> 
    mutate(across(c({{row}}, {{col}}), as_factor)) |> 
    ggplot(aes({{col}}, {{row}}, fill = prop)) + 
    geom_tile(color = "white", linewidth = 1.5, show.legend = FALSE) +
    geom_text(
      mapping = aes(
        label = scales::percent(prop, accuracy = 0.2), 
        color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
      size = 3, 
      family = "Avenir") + 
    scale_fill_viridis_c(option = "mako", labels = scales::percent) + 
    theme_minimal(base_line_size = 0, base_family = "Optima") + 
    labs(subtitle = "Column Percentages")
  
  patchwork::wrap_plots(
    tile_plot_count + labs(x = NULL), 
    tile_plot_row + labs(y = NULL) + theme(axis.text.y = element_blank()), 
    tile_plot_col + labs(y = NULL, x = NULL) + theme(axis.text.y = element_blank()), 
    nrow = 1
  )
  
}

plot_contingency_table(d, attend, polviews)
