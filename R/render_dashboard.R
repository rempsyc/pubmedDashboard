#' @title Render complete pubmedDashboard dashboard
#' @param file_name Desired file name.
#' @param title Desired dashboard title.
#' @param author Desired displayed dashboard author.
#' @inheritParams save_process_pubmed_batch
#' @param query_pubmed Whether to query pubmed. This must be set to TRUE
#'  explicitely to avoid long operations. When the data is already downloaded
#'  and available in the data folder, this step is unnecessary.
#' @param tab_continent Whether to render the "Continent" tab.
#' @param tab_continent_year Whether to render the "Continent by year" tab.
#' @param tab_continent_journal Whether to render the "Continent by journal" tab.
#' @param tab_country Whether to render the "Country" tab.
#' @param tab_country_journal Whether to render the "Country by journal" tab.
#' @param tab_psychology Whether to render the "Psychology" tab.
#' @param tab_economics Whether to render the "Economics" tab.
#' @param tab_general Whether to render the "General" tab.
#' @param tab_figure1 Whether to render the "Figure 1" tab.
#' @param tab_missing Whether to render the "Missing" tab.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' \dontrun{
#' render_dashboard(
#'   file_name = "my_dashboard",
#'   title = "Wonderful Dashboard",
#'   author = "Rémi Thériault",
#'   pubmed_query_string = "passion [Title/Abstract]",
#'   journal = c("Journal of Personality and Social Psychology", "Health Psychology"),
#'   year_low = 2023,
#'   year_high = 2023,
#'   query_pubmed = TRUE,
#'   tab_figure1 = TRUE
#' )
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' setwd(.old_wd)
#' }
#' @export

render_dashboard <- function(file_name = "dashboard",
                             title = "title",
                             author = "author",
                             pubmed_query_string = "",
                             journal = NULL,
                             year_low = 2023,
                             year_high = 2023,
                             month_low = "01",
                             month_high = 12,
                             day_low = "01",
                             day_high = 31,
                             data_folder = "data",
                             batch_size = 5000,
                             api_key = NULL,
                             verbose = TRUE,
                             query_pubmed = FALSE,
                             tab_continent = TRUE,
                             tab_continent_year = TRUE,
                             tab_continent_journal = TRUE,
                             tab_country = TRUE,
                             tab_country_journal = TRUE,
                             tab_psychology = FALSE,
                             tab_economics = FALSE,
                             tab_general = FALSE,
                             tab_figure1 = FALSE,
                             tab_missing = TRUE) {
  insight::check_if_installed(c("rstudioapi", "rmarkdown"))
  rmarkdown::render(system.file("dashboard.Rmd", package = "pubmedDashboard"),
    output_dir = getwd(),
    output_file = file_name
  )
  if (interactive()) {
    rstudioapi::viewer(paste0(file_name, ".html"))
  }
}
