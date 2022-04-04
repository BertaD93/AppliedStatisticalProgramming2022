
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
setClass( #We need to setClass to create the structure
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
setValidity( #Create a validator, we can add more validation as we want
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
setMethod( #Initialize Method
  "initialize", #It is a new class
  "Simpson",
  function(.Object, ...) {
    value = callNextMethod() #Call the method Simpson
    validObject(value)
    return(value)
  }
)

#' @export
setMethod(
  f = "show", ##Use show method to print only the approximated integrate
  signature = "Simpson",
  definition = function(object){
    print(object@result)
  }
)

