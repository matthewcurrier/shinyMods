# R/utils.R
#' Get Census Data
#'
#' @param ethnicity Character string specifying the ethnicity to query
#' @return sf object with county geometries and population data
#' @import tidycensus
#' @import sf
#' @examples
#' \donttest {
#' library(tidycensus)
#' get_census_data("white")
#' }
get_census_data <- function(ethnicity) {
  # Variable codes for different ethnicities
  var_codes <- list(
    white = "B02001_002",
    black = "B02001_003",
    asian = "B02001_005",
    hispanic = "B03002_012"
  )

  # Get county geometry and demographic data
  counties <- get_acs(
    geography = "county",
    variables = var_codes[[ethnicity]],
    year = 2020,
    geometry = TRUE
  ) |>
    rename(population = estimate)

  return(counties)
}
