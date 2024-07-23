#devtools::install_github("ropensci/tabulizer")
library(tabulapdf)
library(tidyverse)
library(pdftables)



f <- "E:/Shishir/FieldData/RTI/SHP RTI data/Download_KREDL/alothydro.pdf"

test = convert_pdf(f, output_file = NULL, format = "csv",
           message = TRUE, api_key = "pe8dnn8k0u59")

f <- "E:/Shishir/FieldData/RTI/SHP RTI data/Download_KREDL/comdhydro.pdf"

test = convert_pdf(f, output_file = NULL, format = "csv",
                   message = TRUE, api_key = "pe8dnn8k0u59")

f <- "E:/Shishir/FieldData/RTI/SHP RTI data/Download_KREDL/canclhydro.pdf"

test = convert_pdf(f, output_file = NULL, format = "csv",
                   message = TRUE, api_key = "pe8dnn8k0u59")

