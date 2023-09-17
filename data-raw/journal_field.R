## code to prepare `journal_field` dataset goes here

psychology <- c(
  "Developmental Psychology",
  "Journal of Personality and Social Psychology",
  "Journal of Abnormal Psychology",
  "Journal of Family Psychology",
  "Health Psychology",
  "Journal of Educational Psychology",
  # Above journals are the original
  "Journal of Experimental Social Psychology",
  "Collabra. Psychology",
  "Journal of Experimental Psychology. General",
  "The Journal of Applied Psychology",
  "Psychological Methods",
  "Advances in Methods and Practices in Psychological Science",
  "Psychological Science"
)

economics <- c(
  "Journal of Economic Psychology",
  "Journal of Experimental and Behavioral Economics",
  "Experimental Economics",
  "Journal of Development Economics",
  "World Development",
  "Quarterly Journal of Economics",
  "Econometrica",
  "Behavioral Public Policy"
)

general <- c(
  "Nature Human Behaviour",
  "PLOS One",
  "Science (New York, N.Y.)",
  "Nature"
)

journal_field <- data.frame(
  journal = c(psychology, economics, general)
)
journal_field$field <- ifelse(
  journal_field$journal %in% psychology, "psychology", ifelse(
    journal_field$journal %in% economics, "economics", ifelse(
      journal_field$journal %in% general, "general", NA)))
journal_field$original_journal <- journal_field$journal %in% psychology[1:6]

usethis::use_data(journal_field, overwrite = TRUE)

