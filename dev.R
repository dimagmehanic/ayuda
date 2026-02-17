library(devtools)
library(magrittr)
document()
install()
load_all()

libs2use <- c("lobstr", "haven", "stringr",
              "DBI", "odbc", "lubridate", "ggplot2",
              "RMySQL", "getPass")

use_package("tidyverse", type = "depends")
libs2use %>% purrr::walk(use_package)