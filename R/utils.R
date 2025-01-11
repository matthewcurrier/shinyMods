#' Get Census Data
#'
#' @param ethnicity Character string specifying the ethnicity to query
#' @return sf object with county geometries and population data
#' @export
#' @examples
#' \donttest{
#' df <- shinyMods::get_census_data("white")
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
  counties <- tidycensus::get_acs(
    geography = "county",
    variables = var_codes[[ethnicity]],
    year = 2020,
    geometry = TRUE
  ) |>
    rename(population = estimate)
}


#' Basic summary statistics for ad campaign data
#'
#' @param df a dataframe with ad campaign data
#' @param ... additional arguments to pass to \code{\link{summarise}}
#' @return a dataframe with specific statistics summarising ad data
#' @export
#' @examples
#' \donttest{
#' my_summary <- summarise_metrics(df)
#' }
summarise_metrics <- function(df, ...) {
  # test if is dataframe
  # test if specific columns are present: impressions, clicks, spend, pageviews
  # What should be done with missing values? Then test is treatment for NA
  # values is correct
  summarise(
    df,
    impressions = sum(impressions, na.rm = TRUE),
    pageviews = sum(pageviews, na.rm = TRUE),
    clicks = sum(clicks, na.rm = TRUE),
    spend = sum(spend, na.rm = TRUE),
    cpc = sum(spend, na.rm = TRUE) / sum(clicks, na.rm = TRUE),
    cpm = sum(spend, na.rm = TRUE) / (sum(impressions, na.rm = TRUE) / 1000),
    cppv = sum(spend, na.rm = TRUE) / sum(pageviews, na.rm = TRUE),
    ctr = sum(clicks, na.rm = TRUE) / sum(impressions, na.rm = TRUE)
  )
}

#' Create a gt table
#'
#' @param df a dataframe with ad campaign data
#' @param tbl_title a character string specifying the title of the table
#' @param ... additional arguments to pass to \code{\link{summarise}}
#' @return a gt table object
#' @export
#' @examples
#' \donttest{
#' my_ad_stats_tbl <- gt_create_table_bld(df, tbl_title = "Ad Campaign Summary")
#' }
gt_create_table_bld <- function(gt_object, tbl_title = "Untitled", ...) {

  dims <- c("op_moment_name", "publisher_detailed", "channel_publisher", "op_primary_story_name")
  metrics <- c("pageviews", "impressions", "clicks", "spend", "cpc", "cpm", "cppv", "ctr")
  metrics_currency <- c("spend", "cpc", "cpm", "cppv")
  metrics_cost_per <- c("cpc", "cpm", "cppv")
  dims_links <- c("op_content_snapshot")

  gt_object |>
    tab_header(
      title = md(tbl_title)
    ) |>
    cols_align(
      columns = matches("^op_"),
      align = "left"
    ) |>
    cols_align(
      columns = tidyselect::any_of(metrics),
      align = "center"
    ) |>
    fmt_currency(
      columns = c(spend),
      decimals = 0,
      currency = "USD"
    ) |>
    fmt_percent(
      columns = any_of(c("ctr")),
      decimals = 3
    ) |>
    fmt_number(
      columns = c(pageviews, clicks, impressions),
      decimals = 0
    ) |>
    fmt_currency(
      columns = tidyselect::any_of(metrics_cost_per),
      currency = "USD",
      use_seps = TRUE,
      decimals = 0
    ) |>
    fmt_percent(
      columns = c(ctr),
      decimals = 3
    ) |>
    sub_values(
      columns = c(pageviews, impressions, clicks, spend, cpc, cpm, ctr),
      fn = function(x) if_else(is.nan(x) | is.na(x), TRUE, FALSE),
      replacement = "-"
    ) |>
    cols_label(
      pageviews ~ "pgvs",
      impressions ~ "impr",
      spend ~ "spnd"
    ) |>
    data_color(
      columns = c(cpc),
      palette = c(
        "#00A600", "#E6E600", "#E8C32E", "#D69C4E", "#Dc863B", "sienna", "sienna4",
        "tomato4", "brown"
      ),
      domain = c(0, 25),
      na_color = "#FFFFFF",
      apply_to = "fill"
    ) |>
    opt_table_font(
      font = c("Arial", default_fonts())
    ) |>
    tab_style(
      style = cell_text(
        color = "black",
        weight = "bold",
        transform = "uppercase"),
      locations = list(
        cells_column_spanners(everything()),
        cells_column_labels(everything())
      )) |>
    tab_options(
      table.border.top.style = "none"
    ) |>
    grand_summary_rows(
      columns = c(impressions, pageviews),
      fns = list(
        summary ~ sum(.)
      ),
      fmt = ~ fmt_number(., use_seps = TRUE, decimals = 0)
    ) |>
    grand_summary_rows(
      columns = c(spend),
      fns = list(
        summary ~ sum(.)
      ),
      fmt = ~ fmt_currency(., decimals = 0, currency = "USD", use_seps = TRUE)
    ) |>
    grand_summary_rows(
      columns = c(cpc),
      fns = list(
        summary ~ sum(spend) / sum(clicks)
      ),
      fmt = ~ fmt_currency(., currency = "USD", use_seps = TRUE, decimals = 0)
    ) |>
    grand_summary_rows(
      columns = c(cpm),
      fns = list(
        summary ~ sum(spend) / (sum(impressions) / 1000)
      ),
      fmt = ~ fmt_currency(., currency = "USD", use_seps = TRUE, decimals = 0)
    ) |>
    grand_summary_rows(
      columns = c(ctr),
      fns = list(
        summary ~ sum(clicks) / (sum(impressions))
      ),
      fmt = ~ fmt_percent(., decimals = 2)
    ) |>
    grand_summary_rows(
      columns = c(cppv),
      fns = list(
        summary ~ sum(spend) / (sum(pageviews))
      ),
      fmt = ~ fmt_currency(., currency = "USD", use_seps = TRUE, decimals = 0)
    ) |>
    tab_options(stub.border.width = 0)
}

#' Helper function for shiny server module
#'
#' @param name a dataframe with ad campaign data
#' @param calculation a character string specifying the title of the table
#' @param description a character string specifying the title of the table
#' @return a list
#' @examples
#' \donttest{
#' ametric <- create_metric("cpc", "sum(spend) / sum(clicks)", "Cost per click"
#' }

# Define a helper function to create metric definitions
create_metric <- function(name, calculation, description = NULL) {
  list(
    name = name,
    calculation = calculation,
    description = description
  )
}
