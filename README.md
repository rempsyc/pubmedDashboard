
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pubmedDashboard

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/pubmedDashboard)](https://CRAN.R-project.org/package=pubmedDashboard)
<!-- badges: end -->

The goal of `pubmedDashboard` is to facilitate the creation of pretty
data visualization dashboards using the `flexdashboard` and `easyPubMed`
packages.

## Installation

You can install the development version of pubmedDashboard like so:

``` r
# If `remotes` isn't installed, use `install.packages("remotes")`
remotes::install_github("rempsyc/pubmedDashboard")
```

# Basic Examples

`pubmedDashboard` helps parse the address to identify department and
university of affiliation, as well as country.

``` r
library(pubmedDashboard)

address <- c("Department of Psychology, Cornell University, Ithaca, New York 14853-7601.",
             "Dipartimento di Psicologia Generale, Università di Padova, Italy.",
             "Universität Mannheim, Federal Republic of Germany.",
             "Département de psychologie, Université du Québec à Montréal.")

get_affiliation(address, "department")
#> [1] "Department of Psychology"            "Dipartimento di Psicologia Generale"
#> [3] NA                                    "Département de psychologie"

get_affiliation(address, "university")
#> [1] "Cornell University"              "Università di Padova"           
#> [3] "Universität Mannheim"            "Université du Québec à Montréal"

get_country(address)
#> [1] "United States" "Italy"         "Germany"       NA
```

# Mega Function

One simple function allows to download the paper data from PubMed,
convert the XLM data to a dataframe, extract affiliations, match
universities to countries, identify countries and continents, and save
the file to disk for later reuse.

``` r
pubmed_query_string <- paste(
  "passion [Title/Abstract]",
  "OR Dualistic Model of Passion [Text Word]")

save_process_pubmed_batch(
  pubmed_query_string,
  year_low = 2023,
  year_high = 2030)
#> 1/5 - Downloading PubMed data...
#> [1] "PubMed data batch 1 / 1 downloaded..."
#> 2/5 - Converting XLM files to dataframe...
#> 3/5 - Extracting affiliations...
#> 4/5 - Matching universities to countries...
#> 5/5 - Identifying countries and continents...
#> Operation sucessfully completed. Congratulations! 
#> File saved in data/articles_2023_2030.rds
```

# Example Dashboards

## Neglected 95% Dashboard

[![](https://rempsyc.github.io/pubmedDashboard/images/figures/n95.png)](https://remi-theriault.com/dashboards/neglected_95)

## Passion Dashboard

[![](https://rempsyc.github.io/pubmedDashboard/images/figures/passion.png)](https://remi-theriault.com/dashboards/passion)
