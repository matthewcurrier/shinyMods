# https://www.kaggle.com/datasets/grebublin/coronavirus-latlon-dataset
covid <- read_csv(here::here("misc", "covid.csv"))
covid <- covid |>
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year_month = format(date, "%Y-%m")
  ) |>
  group_by(country, state, lat, lon) |>
  summarise(across(c(death, confirmed, recovered), sum, na.rm = TRUE),
            .groups = 'drop') |>
  mutate(
    lat_jittered = jitter(lat, factor = 1.2, amount = 0.001),
    lon_jittered = jitter(lon, factor = 1.2, amount = 0.001)
    )


usethis::use_data(covid, overwrite = TRUE)
