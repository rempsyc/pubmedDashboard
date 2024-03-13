#' @title Mega function to process and save PubMed data
#' @param pubmed_query_string The PubMed query string.
#' @param journal The list of desired journals.
#' @param year_low The year the data should start.
#' @param year_high The year the data should end.
#' @param month_low The year the data should start.
#' @param month_high The year the data should end.
#' @param day_low The year the data should start.
#' @param day_high The year the data should end.
#' @param data_folder Where to save the data.
#' @param suffix What suffix to add to the name file.
#' @param batch_size The download batch size.
#' @param api_key The api key for faster processing (optional).
#' @param verbose Whether to include progress messages.
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
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' setwd(.old_wd)
#' }
#' @export
save_process_pubmed_batch <- function(pubmed_query_string = "",
                                      journal = NULL,
                                      year_low = 2024,
                                      year_high = 2024,
                                      month_low = "01",
                                      month_high = 12,
                                      day_low = "01",
                                      day_high = 31,
                                      data_folder = "data",
                                      suffix = "",
                                      batch_size = 5000,
                                      api_key = NULL,
                                      verbose = TRUE) {
  if (is.null(journal)) {
    journal <- ""
  } else {
    journal <- paste0(
      "OR '", journal, "' [Journal]",
      collapse = " "
    )
    journal <- sub("OR", "", journal)
    if (!missing(pubmed_query_string)) {
      journal <- paste0(" AND", journal)
    }
  }

  pubmed_query_string <- paste0(
    pubmed_query_string,
    journal,
    paste0(
      " AND ('", year_low, "/", month_low, "/", day_low, "' [Date - Publication] : '",
      year_high, "/", month_high, "/", day_high, "' [Date - Publication])"
    )
  )

  if (verbose) {
    cat(
      paste("pubmed_query_string =\n", pubmed_query_string), "\n",
      "1/5 - Downloading PubMed data...", print_time()
    )
  }

  # Download data
  d.fls <- batch_pubmed_download2(
    pubmed_query_string = pubmed_query_string,
    year_low = year_low,
    year_high = year_high,
    data_folder = data_folder
  )

  if (verbose) {
    cat("2/5 - Converting XLM files to dataframe...", print_time())
  }

  # Convert XLM data to a data frame of first authors
  # articles.df <- table_articles_byAuth(d.fls, included_authors = "first")
  articles.df <- all_articles_to_df(d.fls)

  if (nrow(articles.df) == 0) {
    message("all_articles_to_df() found no result. Returning empty data frame.")
    return(articles.df)
  }

  if (verbose) {
    cat("3/5 - Extracting affiliations...", print_time())
  }

  # Split address in university and department
  articles.df2 <- add_affiliation(articles.df)

  if (verbose) {
    cat("4/5 - Matching universities to countries...", print_time())
  }

  # Match universities and countries
  articles.df3 <- match_university(articles.df2)

  # world.cities2 <- world.cities %>%
  #   filter(!name %in% c("Ins", "Institut", "U"))

  if (verbose) {
    cat("5/5 - Identifying countries and continents...", print_time())
  }

  # Get full name country, continent, and region
  articles.df4 <- add_region(articles.df3)

  if(!missing(suffix)) {
    suffix <- paste0("_", suffix)
  }

  file_name <- paste0(data_folder, "/articles_", year_low, "_", year_high, suffix, ".rds")
  saveRDS(articles.df4, file_name)

  if (verbose) {
    start <- year_low
    end <- year_high

    # if (!missing(year_low)) {
    #   start <- paste0(start, "-", year_low)
    # }
    if (!missing(month_high)) {
      end <- paste0(end, "-", month_high)
    }
    if (!missing(day_low)) {
      start <- paste0(start, "-", day_low)
    }
    if (!missing(day_high)) {
      end <- paste0(end, "-", day_high)
    }

    success_message <- c(
      "Operation successfully completed. Congratulations!", print_time(),
      "File saved in", paste0(data_folder, "/articles_", start, "_", end, suffix, ".rds\n\n")
    )

    cat(success_message)
  }
}

print_time <- function() {
  paste0("[", format(Sys.time(), "%X"), "]", "\n")
}
