#' @title Match list of universities to countries in a dataframe
#' @param data The dataframe to use for matching.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' \dontrun{
#' d.fls <- batch_pubmed_download2(
#'   pubmed_query_string = paste(
#'     "passion [Title/Abstract]",
#'     "AND Dualistic Model of Passion [Text Word]",
#'     "AND ('2023/01/01' [Date - Publication] : '2023/12/31' [Date - Publication])"
#'   ),
#'   data_folder = ""
#' )
#' articles.df <- all_articles_to_df(d.fls)
#' articles.df2 <- add_affiliation(articles.df)
#' articles.df3 <- match_university(articles.df2)
#' articles.df3[5, ]
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' setwd(.old_wd)
#' }
#' @export
match_university <- function(data) {
  unis <- pubmedDashboard::universities
  unis$university <- stringi::stri_unescape_unicode(unis$university)

  data <- data %>%
    dplyr::mutate(
      university_old = .data$university,
      university = partial_vlookup(
        .data$university,
        unis$university
      ),
      university = ifelse(is.na(.data$university), partial_vlookup(
        .data$address, unis$university
      ),
      .data$university
      ),
      .after = "university"
    )
  data <- data %>%
    dplyr::left_join(unis, by = "university", multiple = "first") %>%
    dplyr::relocate("country_code", .after = "year")
  data
}
