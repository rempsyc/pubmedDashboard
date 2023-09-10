#' @title Generate table of journal paper percentages, by continent and year
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
#' table_country_journal(data)
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' setwd(.old_wd)
#' }
#' @importFrom rlang .data
#' @export

table_country_journal <- function(data, datatable = TRUE) {
  x <- data %>%
    dplyr::group_by(.data$journal, .data$country) %>%
    dplyr::filter(!is.na(.data$country)) %>%
    dplyr::count(name = "Papers") %>%
    dplyr::mutate(percentage = as.numeric(round(.data$Papers / get_journal_papers2(
      data, .data$journal
    ) * 100, 2))) %>%
    dplyr::arrange(dplyr::desc(.data$journal), dplyr::desc(.data$Papers))

  df_country_journal_missing <- data %>%
    dplyr::filter(is.na(.data$country)) %>%
    dplyr::group_by(.data$journal) %>%
    dplyr::count(.data$journal, name = "Papers") %>%
    dplyr::arrange(dplyr::desc(.data$journal), dplyr::desc(.data$Papers)) %>%
    dplyr::left_join(by = "journal", data %>%
      dplyr::group_by(.data$journal) %>%
      dplyr::count(.data$journal, name = "all_papers") %>%
      dplyr::arrange(dplyr::desc(.data$journal))) %>%
    dplyr::mutate(
      percentage = as.numeric(round(.data$Papers / .data$all_papers * 100, 2)),
      country = "Missing*"
    ) %>%
    dplyr::select(-"all_papers")

  x <- x %>%
    dplyr::ungroup() %>%
    dplyr::add_row(df_country_journal_missing) %>%
    dplyr::arrange(dplyr::desc(.data$journal), dplyr::desc(.data$Papers)) %>%
    dplyr::rename_with(stringr::str_to_title)

  if (isTRUE(datatable)) {
    x <- DT::datatable(x,
      options = list(searching = FALSE, paging = FALSE),
      caption = "Journal paper percentages, by country and journal"
    )
  }
  x
}
