
library(devtools)
library(roxygen2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## This is run once when the package strcuture is first created


## This can be run many times as the code is updates

current.code <- as.package("integrateIt")
load_all(current.code)
document(current.code)



x = 1:10
f = 1:10
limits = c(1,10)


integrateIt(x, f, limits, "Simpson")

integrateIt(x, f, limits, "Trapezoid")


fun = function(x) sin(x)
tolerance = 0.001
rule = "Trapezoid"
start = 10
limits = c(1,10)

###########

tolTest(fun=fun, tolerance=tolerance, rule=rule, start=start, limits=limits)

  