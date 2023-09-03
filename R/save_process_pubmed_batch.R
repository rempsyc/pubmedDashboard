#' @title Mega function to process and save PubMed data
#' @param pubmed_query_string The PubMed query string.
#' @param year_low The year the data should start.
#' @param year_high The year the data should end.
#' @param data_folder Where to save the data.
#' @param batch_size The download batch size.
#' @param api_key The api key for faster processing (optional).
#' @param verbose Whether to include progress messages.
#' @examples
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
#' }
#' @export
save_process_pubmed_batch <- function(pubmed_query_string,
                                      year_low,
                                      year_high,
                                      data_folder = "data",
                                      batch_size = 5000,
                                      api_key = NULL,
                                      verbose = TRUE) {
  if (verbose) {
    cat("1/5 - Downloading PubMed data...\n")
  }

  # Download data
  d.fls <- batch_pubmed_download2(
    pubmed_query_string = paste(
      pubmed_query_string,
      paste0(
        "AND ('", year_low, "/01/01' [Date - Publication] : '",
        year_high, "/12/31' [Date - Publication])"
      )
    ),
    year_low = year_low,
    year_high = year_high,
    data_folder = data_folder
  )

  if (verbose) {
    cat("2/5 - Converting XLM files to dataframe...\n")
  }

  # Convert XLM data to a data frame of first authors
  # articles.df <- table_articles_byAuth(d.fls, included_authors = "first")
  articles.df <- all_articles_to_df(d.fls)

  if (verbose) {
    cat("3/5 - Extracting affiliations...\n")
  }

  # Split address in university and department
  articles.df2 <- add_affiliation(articles.df)

  if (verbose) {
    cat("4/5 - Matching universities to countries...\n")
  }

  # Match universities and countries
  articles.df3 <- match_university(articles.df2)

  # world.cities2 <- world.cities %>%
  #   filter(!name %in% c("Ins", "Institut", "U"))

  if (verbose) {
    cat("5/5 - Identifying countries and continents...\n")
  }

  # Get full name country, continent, and region
  articles.df4 <- add_region(articles.df3)

  saveRDS(articles.df4, paste0(data_folder, "/articles_", year_low, "_", year_high, ".rds"))

  if (verbose) {
    cat(
      "Operation sucessfully completed. Congratulations!",
      "\nFile saved in", paste0(data_folder, "/articles_", year_low, "_", year_high, ".rds"), "\n"
    )
  }
}
