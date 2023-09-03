test_that("save_process_pubmed_batch works", {
  pubmed_query_string <- paste(
    "passion [Title/Abstract]",
    "AND Dualistic Model of Passion [Text Word]"
  )

  testthat::expect_no_warning(
    save_process_pubmed_batch(
      pubmed_query_string,
      year_low = 2023,
      year_high = 2023,
      data_folder = tempdir()
    )
  )
})
