
#' Simpson's Rule
#'
#' Technique for approximating a definite integrals numerically
#'
#' @slot x numeric. A vector of values
#' @slot f numeric. A vector of evaluated values
#' @slot result numeric. Result of the integrated approximation
#'
#' @return Simpson class
#' @export
#'
#' @examples
#' new("Simpson", x=1:10, f=1:10, result = 49.5)
setClass(
  Class = "Simpson",
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
  "Simpson",
  function(object){
    # Check length vectors
    if(length(object@x) != length(object@f)) stop("Both lengths of x & f must be equal")
    # Check x sort
    if(!all(sort(object@x) == object@x)) stop("x vector must be ordered!")
    # Check result
    if(length(object@result) != 1) stop("Result must be an integer not a vector")
  }
)

#' @export
setMethod(
  "initialize", #nueva clase, corre ese comando
  "Simpson",
  function(.Object, ...) {
    value = callNextMethod() #llamar algun metodo que se llame simpson
    validObject(value)
    return(value)
  }
)

#' @export
setMethod(
  f = "show", #Print method
  signature = "Simpson",
  definition = function(object){
    print(object@result)
  }
)

