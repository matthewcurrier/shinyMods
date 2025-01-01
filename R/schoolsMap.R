library(readr)
library(janitor)
library(sf)
library(leaflet)


sch <- read_csv("raw/public_elem_schools_2023.csv") |>
  clean_names() |>
  st_as_sf(
    coords = c("x", "y"),
    crs = 4326,
    remove = FALSE)


leaflet(sch[1:100, ]) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    radius = 8,
    color = "blue",
    fillOpacity = 0.7,
    popup = ~paste("Name:", sch_name, "<br>ID:", ncessch),
    label = ~sch_name
  )
