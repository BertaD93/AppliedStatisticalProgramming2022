
#' Trapezoidal Rule
#'
#' Technique for approximating the definite integral,
#'
#' @slot x numeric. A vector of values
#' @slot f numeric. A vector of evaluated values
#' @slot result numeric. Result of the integrated approximation
#'
#' @return Trapezoidal class
#' @export
#'
#' @examples
#' new("Trapezoid", x=1:10, f=1:10, result = 49.5)
setClass(
  Class = "Trapezoid",
  representation = representation(
    x = "numeric",
    f = "numeric",
    result = "numeric"
  ),
  prototype = prototype(
    x = c(),
    f = c(),
    result = c()
  )
)

#' @export
setValidity(
  "Trapezoid",
  function(object){
    # Check lenght vectors
    if(length(object@x) != length(object@f)) stop("Both lengths of x & f must be equal")
    # Check x sort
    if(!all(sort(object@x) == object@x)) stop("x vector must be ordered!")
    # Check result
    if(length(object@result) != 1) stop("Result must be an integer not a vector")
  }
)

#' @export
setMethod(
  "initialize",
  "Trapezoid",
  function(.Object, ...) {
    value = callNextMethod()
    validObject(value)
    return(value)
  }
)

#' @export
setMethod(
  f = "show",
  signature = "Trapezoid",
  definition = function(object){
    print(object@result)
  }
)
