#' @title Generate table of journal paper percentages, by continent and journals
#' @param data The processed dataframe of data
#' @param datatable Whether to output a [DT::datatable] HTML table widget
#'  instead of a regular dataframe (defaults to TRUE).
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' \dontrun{
#' pubmed_query_string <- paste(
#'   "passion [Title/Abstract]",
#'   "AND Dualistic Model of Passion [Text Word]"
#' )
#'
#' save_process_pubmed_batch(
#'   pubmed_query_string,
#'   year_low = 2023,
#'   year_high = 2023
#' )
#' data <- read_bind_all_data()
#' table_continent_journal(data)
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' setwd(.old_wd)
#' }
#' @importFrom rlang .data
#' @export

table_continent_journal <- function(data, datatable = TRUE) {
  journal_paper_missing <- data %>%
    dplyr::group_by(.data$journal) %>%
    dplyr::summarize(Missing = sum(is.na(.data$continent)) / dplyr::n()) %>%
    dplyr::pull("Missing")

  x <- data %>%
    dplyr::mutate(missing = sum(is.na(.data$continent)) / dplyr::n()) %>%
    dplyr::filter(!is.na(.data$continent)) %>%
    dplyr::group_by(.data$journal) %>%
    dplyr::summarize(
      Papers = dplyr::n(),
      `North America` = sum(.data$continent == "Northern America") / dplyr::n(),
      Europe = sum(.data$continent == "Europe") / dplyr::n(),
      Asia = sum(.data$continent == "Asia") / dplyr::n(),
      Oceania = sum(.data$continent == "Oceania") / dplyr::n(),
      `Latin America` = sum(.data$continent == "Latin America and the Caribbean") / dplyr::n(),
      Africa = sum(.data$continent == "Africa") / dplyr::n(),
      `Missing*` = dplyr::first(missing),
    )

  if (nrow(x) != length(journal_paper_missing)) {
    warning(
      "The last couple missing values in the Missing* column may be incorrect, ",
      "as the number of rows does not match and were forced to fit."
    )
    journal_paper_missing <- journal_paper_missing[seq_len(nrow(x))]
  }

  x <- x %>%
    dplyr::mutate(
      `Missing*` = journal_paper_missing,
      dplyr::across("North America":"Missing*", ~ round(.x * 100, 2))
    ) %>%
    dplyr::rename_with(stringr::str_to_title)

  if (isTRUE(datatable)) {
    x <- DT::datatable(
      x,
      options = list(iDisplayLength = 5),
      caption = "Journal paper percentages, by continent and journal"
    )
  }
  x
}
