
simulation_votes <- function(dem_prob_pop, sample_size, num_sims) {
  
  if (!is.numeric(dem_prob_pop) | !dplyr::between(dem_prob_pop, left = 0, right = 1)) {
    stop("`dem_prob_pop` must be a number between 0 and 1.", call. = FALSE)
  }
  
  if (!is.numeric(sample_size) | sample_size <= 0) {
    stop("`sample_size` must be a positive number.", call. = FALSE)
  }
  
  if (!is.numeric(num_sims) | num_sims <= 0) {
    stop("`num_sims` must be a positive number.", call. = FALSE)
  }
  
  one_sim_draw <- function(dem_prob_pop, sample_size) {
    sample(
      x = c("Dem", "Rep"), 
      size = sample_size, 
      prob = c(dem_prob_pop, 1 - dem_prob_pop), 
      replace = TRUE
    )
  }
  
  many_sims <- replicate(num_sims, one_sim_draw(dem_prob_pop, sample_size), simplify = FALSE)
  
  df <- tibble::enframe(many_sims, name = "id", value = "vote") |>
    tidyr::unnest_longer(col = "vote") 
  
  df$dem_prob_pop <- as.character(dem_prob_pop)
  df$sample_size <- as.character(sample_size)
  df$num_sims <- as.character(num_sims)
  
  return(df)
  
}
