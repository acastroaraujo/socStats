
validate_sim_parameters <- function(dem_prob, sample_size, num_sim) {

  if (!is.numeric(dem_prob) | !dplyr::between(dem_prob, left = 0, right = 1)) {
    stop("`dem_prob` must be a number between 0 and 1.", call. = FALSE)
  }
  
  if (!is.numeric(sample_size) | sample_size <= 0) {
    stop("`sample_size` must be a positive number.", call. = FALSE)
  }
  
  if (!is.numeric(num_sim) | num_sim <= 0) {
    stop("`num_sim` must be a positive number.", call. = FALSE)
  }
  
}

simulation_votes <- function(dem_prob, sample_size, num_sim) {
  
  validate_sim_parameters(dem_prob, sample_size, num_sim)
  
  out <- replicate(
    n = num_sim, 
    expr = {
      sample(
        x = c("Dem", "Rep"), 
        size = sample_size, 
        prob = c(dem_prob, 1 - dem_prob), 
        replace = TRUE
      )
    },
    simplify = FALSE
  )
  
  df <- tibble::enframe(out, name = "id", value = "vote") |> 
    tidyr::unnest_longer(col = "vote")
  
  return(df)
}
