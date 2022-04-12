library(devtools)
library(roxygen2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


current.code <- as.package("Midterm")
load_all(current.code)
document(current.code)


