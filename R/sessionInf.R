####----------------------------------------------------------------------####
# Program Name: sessionInf.R
# Compound/Project ID/Study/PA: pilot01
# Developer: Dmytro Hasan
#
# OS / R Version: 
#
# Purpose: to save session information into sessionInformation.txt
#
# Input:  
# Output: sessionInformation.txt
#
# External files called: 
#
# Additional Notes:
#
#-----------------------------------------------------------------------------
# ~Change Log~
#-----------------------------------------------------------------------------
 
getdata <- function(location, ext="xpt"){
    files <- list.files(path = location , pattern = paste("*.", ext, sep = ""),
                      full.names = TRUE, recursive = TRUE) %>% as.list()
    names(files) <- files %>% str_split("/") %>% map(last) %>% str_split("[.]") %>% map(first) %>% unlist()
    files
}
  