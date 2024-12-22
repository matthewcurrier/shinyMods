# https://walker-data.com/tidycensus/articles/other-datasets.html
library(tidycensus)
library(tidyverse)
library(tigris)
library(janitor)

options(tigris_use_cache = TRUE)

us_components_2023 <- get_estimates(geography = "state", product = "components", vintage = 2023)
us_components_2022 <- get_estimates(geography = "state", product = "components", vintage = 2022)

us_components <- bind_rows(us_components_2023, us_components_2022)

us_components2 <- us_components |>
  pivot_wider(names_from = variable, values_from = value) |>
  clean_names() |>
  arrange(name, year) |>
  group_by(name) |>
  mutate(
    births_change = births - lag(births),
    pct_births_change = (births_change / births),
    delta_internationalmig = internationalmig - lag(internationalmig),
    pct_internationalmig_change = (delta_internationalmig / internationalmig),
    delta_domesticmig = domesticmig - lag(domesticmig),
    pct_domesticmig_change = (delta_domesticmig / domesticmig)
    ) |>
  ungroup()

us_components3 <- us_components2 |>
  filter(year == 2023) |>
  select(
    name,
    year,
    births,
    deaths,
    starts_with("pct_")
    ) |>
  rename(
    m_births = births,
    m_deaths = deaths
  )
us_migration <- us_components3
usethis::use_data(us_migration, overwrite = TRUE)
