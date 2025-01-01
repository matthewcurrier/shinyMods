library(readr)
library(janitor)
library(sf)
library(dplyr)
library(tidyr)
library(leaflet)
library(gt)
library(stringr)  # Add this

# Function to create a gt table for one school
# create_school_table <- function(data, school_id) {
#   data |>
#     filter(objectid == school_id) |>
#     gt() |>
#     tab_header(
#       title = first(data$sch_name[data$objectid == school_id])
#     ) |>
#     cols_label(
#       grade = "Grade Level",
#       enrollment = "Enrollment"
#     ) |>
#     fmt_number(
#       columns = enrollment,
#       decimals = 0
#     ) |>
#     tab_options(
#       table.width = px(140),
#       container.height = px(220)
#     ) |>
#     # opt_table_font(
#     #   font.size = px(7),
#     #   font.family = "Arial"
#     # ) |>
#     as_raw_html()
# }

create_school_table <- function(data, school_id) {
  school_name <- first(data$sch_name[data$objectid == school_id])

  table_html <- data |>
    filter(objectid == school_id) |>
    select(grade, enrollment) |>
    mutate(
      grade = case_when(
        grade == "pk" ~ "Pre-K",
        grade == "kg" ~ "Kindergarten",
        grade == "totfenrol" ~ "Total",
        TRUE ~ str_c("Grade ", str_to_title(grade))
      ),
      enrollment = format(enrollment, big.mark = ",")
    ) |>
    knitr::kable(
      format = "html",
      col.names = c("Grade Level", "Enrollment"),
      table.attr = 'style="width:100%; font-size: 12px;"'
    )

  paste0(
    '<div style="max-width:140px;"><h4 style="margin:5px 0;">',
    school_name,
    '</h4>',
    table_html,
    '</div>'
  )
}


sch <- read_csv("raw/public_elem_schools_2023.csv") |>
  clean_names() |>
  filter(stabr == "IA")

sch_tbl <- sch |>
  select(
    objectid,
    sch_name,
    pk:totfenrol
  ) |>
  pivot_longer(
    cols = pk:totfenrol,
    names_to = "grade",
    values_to = "enrollment"
  )


school_tables <- sch_tbl |>
  group_by(
    objectid
  ) |>
  summarise(
    school_table = create_school_table(sch_tbl, objectid)
  )




  # distinct(objectid) |>
  # pull(objectid) |>
  # sapply(function(id) create_school_table(sch_tbl, id),
  #        simplify = FALSE)





sch2 <- sch |>
  st_as_sf(
    coords = c("x", "y"),
    crs = 4326,
    remove = FALSE)


sch3 <- sch2 |>
  select(
    objectid,
    x,
    y,
    sch_name,
    school_level,
    ncessch
  ) |>
  left_join(
    school_tables,
    by = "objectid"
  )


sch4_elem <- sch3 |>
  filter(school_level == "Elementary")

sch4_high <- sch3 |>
  filter(school_level == "High")




leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addMarkers(
    data = sch4_elem,
    popup = ~school_table,
    popupOptions = popupOptions(
      maxWidth = 140,
      maxHeight = 250
    ),
    label = ~sch_name,
    group = "Elem"
  ) |>
  addMarkers(
    data = sch4_high,
    popup = ~school_table,
    popupOptions = popupOptions(
      maxWidth = 140,
      maxHeight = 250
    ),
    label = ~sch_name,
    group = "High"
  ) |>
  # Add layer control
  addLayersControl(
    overlayGroups = c("Elem", "High"),
    position = "topright"
  )




# Create sample data
cities <- data.frame(
  name = c("New York", "Boston", "Chicago"),
  lat = c(40.7128, 42.3601, 41.8781),
  lng = c(-74.0060, -71.0589, -87.6298),
  type = c("Major", "Major", "Major")
)

towns <- data.frame(
  name = c("Albany", "Providence", "Milwaukee"),
  lat = c(42.6526, 41.8240, 43.0389),
  lng = c(-73.7562, -71.4128, -87.9065),
  type = c("Minor", "Minor", "Minor")
)



# Create map with grouped layers
leaflet() %>%
  addTiles() %>%
  # Add major cities as one group
  addMarkers(
    data = cities,
    ~lng, ~lat,
    popup = ~name,
    group = "Major Cities",
  ) %>%
  # Add towns as another group
  addMarkers(
    data = towns,
    ~lng, ~lat,
    popup = ~name,
    group = "Towns",
  ) %>%
  # Add layer control to toggle groups
  addLayersControl(
    overlayGroups = c("Major Cities", "Towns"),
    options = layersControlOptions(collapsed = FALSE)
  )

