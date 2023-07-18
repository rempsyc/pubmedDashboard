## code to prepare `universities` dataset goes here

# Download university + country data
universities <- read.csv(
  "https://raw.githubusercontent.com/endSly/world-universities-csv/master/world-universities.csv",
  header = FALSE)
names(universities) <- c("country_code", "university", "website")
universities <- universities[1:2]

universities <- universities %>%
  dplyr::mutate(
    country_code = replace(country_code,
                           university == "University of the Netherlands Antilles, Curacao", "CW"),
    country_code = replace(country_code,
                           university == "University of Sint Eustatius School of Medicine", "SX"),
    country_code = replace(country_code,
                           university == "St.James's School of Medicine, Bonaire" |
                             university == "American University of the Caribbean, Sint Maarten" |
                             university == "International University School of Medicine (IUSOM)", "BQ"))

# Correct/add a few university countries manually
universities2 <- tibble::tribble(
  ~country_code, ~university,
  "AR", "CEMIC, CONICET",
  "AU", "Institute for Positive Psychology and Education",
  "AU", "Melbourne School of Psychological Sciences",
  "AT", "University of Vienna",
  "BE", "University of Liege"
  ,"BR", "Institute D'Or for Research and Teaching",
  "CA", "Montreal Behavioural Medicine Centre (MBMC)",
  "CA", "University of Quebec",
  "CA", "University of Montreal",
  "CA", "Universite Laval",
  "CH", "Jacobs Center for Productive Youth Development",
  "CH", "University of Zurich",
  "DE", "University of Tubingen",
  "DE", "Max Planck Institute for Human Development",
  "DE", "Max Planck Institute",
  "DE", "Humboldt University",
  "DE", "Heidelberg University",
  "DE", "University of Cologne",
  "DE", "University of Bonn",
  "DE", "University of Münster",
  "DE", "University of Tübingen",
  "DE", "University of Göttingen",
  "DE", "Heidelberg University",
  "DE", "Leipzig University",
  "DE", "University of Marburg",
  "FR", "CNRS",
  "FR", "INSEAD",
  "GB", "Manchester Centre for Health Psychology",
  "GB", "York St John University",
  "GB", "Moray House School of Education and Sport",
  "GB", "Cambridge",
  "IL", "Technion-Israel Institute of Technology",
  "KR", "SKK Graduate School of Business",
  "KR", "Sungkyunkwan University",
  "NL", "Vrije Universiteit Amsterdam",
  "SG", "Yale-NUS College",
  "SG", "Lee Kong Chian School of Business",
  "US", "University of Colorado",
  "US", "Stony Brook University",
  "US", "Rutgers Business School-Newark and New Brunswick",
  "US", "Columbia Business School",
  "US", "Office of Population Research",
  "US", "School of Family Life",
  "US", "Tepper School of Business",
  "US", "Stephen M. Ross School of Business",
  "US", "Fuqua School of Business",
  "US", "Jones Graduate School of Business",
  "US", "Questrom School of Business",
  "US", "Booth School of Business",
  "US", "Annenberg School for Communication and Journalism",
  "US", "National Center for Posttraumatic Stress Disorder at VA Boston Healthcare System",
  "US", "Cincinnati Children's Hospital Medical Center",
  "US", "University of Wisconsin",
  "US", "Stanford",
  "US", "University of Wisconsin-Madison",
  "US", "Stony Brook University",
  "VN", "SWPS University of Social Sciences and Humanities"
  )

universities <- dplyr::bind_rows(universities, universities2)

usethis::use_data(universities, overwrite = TRUE)

