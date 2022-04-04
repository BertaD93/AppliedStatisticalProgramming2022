
#' Tolarence test
#'
#'  Function and increase the number of intervals n until the answer it provides using
#'  the specified approximation is within tolerance of the correct answer
#'
#' @param fun function. A function to be evaluated
#' @param tolerance numeric. The tolerance we want to set
#' @param rule character. Trapezoidal or Simpsons rule
#' @param start numeric. The initial n
#' @param limits numeric. The limits of the integral
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' fun = function(x) sin(x)
#' tolerance = 0.001
#' rule = "Trapezoid"
#' start = 10
#' limits = c(1,10)
#' tolTest(fun=fun, tolerance=tolerance, rule=rule, start=start, limits=limits)
#'
setGeneric(name="tolTest", #Set Generic
           def=function(fun, tolerance, rule, start, limits,...)
           {standardGeneric("tolTest")}
)

#' @export
setMethod( #Set Method
  f="tolTest",
  definition=function(fun, tolerance, rule, start, limits, ...){

    error <- tolerance +1 #Force to enter to the while loop
    n <- start
    while(error > tolerance){ #If the error is greater than tolerance, the while loop will run
      step <- (limits[2] - limits[1]) / n
      x <- seq(limits[1], limits[2], by = step)
      f <- fun(x)
      integral_object <- integrateIt(x, f, limits, rule) #Use integrateIt
      integral_estimation <- integral_object@result #Access to the rule was set: trapezoid or simpsons
      correct <- integrate(fun, limits[1], limits[2])$value #This is the exact calculation of the integrate
      error <- abs(correct - integral_estimation) #Calculate the error
      n <- n + 1
    }

    return_list = list(
      inputs = list(fun=fun, tolerance=tolerance, rule=rule, start=start, limits=limits),
      n = n,
      absolute_error = error
    )

    return(return_list)
  }
)
