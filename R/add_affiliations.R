#' @title Add affiliations to pubmedDashboard dataframe
#' @param data The dataframe on which to add affiliations (department and
#' university).
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' d.fls <- batch_pubmed_download2(
#'   pubmed_query_string = paste(
#'     "passion [Title/Abstract]",
#'     "AND Dualistic Model of Passion [Text Word]",
#'     "AND ('2023/01/01' [Date - Publication] : '2023/12/31' [Date - Publication])"
#'    ),
#'    year_low = 2023,
#'    year_high = 2023
#'  )
#' articles.df <- all_articles_to_df(d.fls)
#' articles.df2 <- add_affiliation(articles.df)
#' articles.df2[2, ]
#' }
#' @export

add_affiliation <- function(data) {
  data <- data %>%
    dplyr::mutate(
      department = get_affiliation(.data$address, "department"),
      university = get_affiliation(.data$address, "university")
    )
  data <- data %>%
    dplyr::select(
      dplyr::all_of(c(
        "journal", "year", "university", "department", "address",
        "lastname", "firstname", "month", "day", "jabbrv", "title",
        "doi", "pmid", "abstract"
    )))
  data
}

