#' @title Match list of universities to countries in a dataframe
#' @param data The dataframe to use for matching.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' d.fls <- batch_pubmed_download2(
#'   pubmed_query_string = paste(
#'     "passion [Title/Abstract]",
#'     "AND Dualistic Model of Passion [Text Word]",
#'     "AND ('2023/01/01' [Date - Publication] : '2023/12/31' [Date - Publication])"
#' ))
#' articles.df <- all_articles_to_df(d.fls)
#' articles.df2 <- add_affiliation(articles.df)
#' articles.df3 <- match_university(articles.df2)
#' articles.df3[5, ]
#' }
#' @export
match_university <- function(data) {
  data <- data %>%
    dplyr::mutate(
      university_old = .data$university,
      university = partial_vlookup(
        .data$university,
        stringi::stri_unescape_unicode(pubmedDashboard::universities$university)
      ),
      university = ifelse(is.na(.data$university), partial_vlookup(
        .data$address, pubmedDashboard::universities$university
      ),
      .data$university
      ),
      .after = "university"
    )
  data <- data %>%
    dplyr::left_join(pubmedDashboard::universities, by = "university", multiple = "first") %>%
    dplyr::relocate("country_code", .after = "year")
  data
}
