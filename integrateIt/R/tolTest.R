
#' Tolarence test
#'
#' @param fun
#' @param tolerance
#' @param rule
#' @param start
#' @param limits
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
setGeneric(name="tolTest",
           def=function(fun, tolerance, rule, start, limits,...)
           {standardGeneric("tolTest")}
)

#' @export
setMethod(
  f="tolTest",
  definition=function(fun, tolerance, rule, start, limits, ...){

    error = tolerance +1 # Force to enter first while loop
    n = start
    while(error > tolerance){
      step = (limits[2] - limits[1]) / n
      x = seq(limits[1], limits[2], by = step)
      f = fun(x)
      integral_object = integrateIt(x, f, limits, rule)
      integral_estimation = integral_object@result
      correct = integrate(fun, limits[1], limits[2])$value
      error = abs(correct - integral_estimation)
      n = n + 1
    }

    return_list = list(
      inputs = list(fun=fun, tolerance=tolerance, rule=rule, start=start, limits=limits),
      n = n,
      absolute_error = error
    )

    return(return_list)
  }
)
