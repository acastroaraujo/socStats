
library(gssr)
library(tidyverse)

gss19 <- gss_get_yr(2018) 
data("gss_doc")

vars <- c(
  "wrkslf", "attend",  "polviews", "cappun", "suicide3", "xmarsex", "degree", 
  "padeg", "madeg", "sex", "race", "born", "mcsds6", "condom", "marital", 
  "god"
)

dict <- gss_doc |> 
  filter(id %in% vars) |> 
  select(id, description, marginals)


gss_doc |> 
  filter(id %in% vars) |> 
  select(id, description, properties) |> 
  pull(properties)

d <- gss18 |> 
  select(all_of(vars))

d <- d |> 
  haven::zap_missing() |> 
  haven::as_factor()

readr::write_rds(list(df = d, dict = dict), "solutions8/data.rds")
