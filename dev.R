library(devtools)
library(magrittr)
document()
install()

libs2use <- c("lobstr", "haven", "stringr",
              "DBI", "odbc", "lubridate", "ggplot2",
              "ggalluvial", "RMySQL")

use_package("tidyverse", type = "depends")
libs2use %>% purrr::walk(use_package)