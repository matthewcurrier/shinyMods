library(readxl)
library(janitor)
library(devtools)
library(here)

pss <- read_excel(here("files", "pss.xlsx"))|>
  clean_names()

usethis::use_data(pss, overwrite = TRUE)
