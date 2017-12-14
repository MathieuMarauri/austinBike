
# get all packages names from the mlr webpage


# Packages ----------------------------------------------------------------

library('rvest')


# Scrapp ------------------------------------------------------------------

packages <- read_html('https://mlr-org.github.io/mlr-tutorial/release/html/integrated_learners/index.html#regression-64') %>% 
  html_nodes("td[align=left] a") %>% 
  html_text() %>% 
  unique()

lapply(packages[1:6], install.packages)
lapply(packages[7:68], install.packages)
