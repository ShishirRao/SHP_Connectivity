#devtools::install_github("ropensci/tabulizer")
library(tabulapdf)
library(tidyverse)
library(pdftables)
library(tesseract)



f <- "E:/Shishir/FieldData/RTI/SHP RTI data/Download_KREDL/alothydro.pdf"

test = convert_pdf(f, output_file = NULL, format = "csv",
           message = TRUE, api_key = "pe8dnn8k0u59")

f <- "E:/Shishir/FieldData/RTI/SHP RTI data/Download_KREDL/comdhydro.pdf"

test = convert_pdf(f, output_file = NULL, format = "csv",
                   message = TRUE, api_key = "pe8dnn8k0u59")

f <- "E:/Shishir/FieldData/RTI/SHP RTI data/Download_KREDL/canclhydro.pdf"

test = convert_pdf(f, output_file = NULL, format = "csv",
                   message = TRUE, api_key = "pe8dnn8k0u59")

f <- "E:/Shishir/FieldData/RTI/SHP RTI data/softcopies from KREDL/SHP Rejected  List Annexure-6.pdf"
test = convert_pdf(f, output_file = NULL, format = "csv",
                   message = TRUE, api_key = "pe8dnn8k0u59")




img_file <- pdftools::pdf_convert(f, format = 'tiff',  dpi = 400)

# Extract text from png image
text <- ocr(img_file)
writeLines(text, "E:/Shishir/FieldData/RTI/SHP RTI data/softcopies from KREDL/rejected.csv")



