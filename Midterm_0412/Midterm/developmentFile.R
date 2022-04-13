library(devtools)
library(roxygen2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


current.code <- as.package("Midterm")
load_all(current.code)
document(current.code)
devtools::check(current.code)

# Create y for testing
y_test = rpois(1000,4)

#Test functions
log_lik_test = logLik(y_test, 1)

mle_y_test = mle(y_test)

std_err_test = standardError(y_test, "bootstrap")

estimatePois_test = estimatePois(y_test, "basic")

#new("PoisMLE", y = 1,mle=-1, ll=1, se=1, SEtype="") This line I used to test
