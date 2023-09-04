#' @title Read local pubmedDashboard data files and bind them in a single dataframe
#' @param data_folder The folder in which the data lives
#' @export
read_bind_all_data <- function(data_folder = "data") {
  filenames <- list.files("data/", pattern = "articles_.*.rds", full.names = TRUE)
  ldf <- lapply(filenames, readRDS)
  df <- dplyr::bind_rows(ldf)
  df <- rempsyc::best_duplicate(df, "pmid")
  df
}
