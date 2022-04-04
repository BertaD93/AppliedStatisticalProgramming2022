
#' Integration approximation
#'
#' Calculate integral by Simpson or Trapezoidal rule
#'
#' @param x numeric. A vector of values
#' @param f numeric. A vector of evaluated values
#' @param limits numeric. Limits of integrals: a and b values
#' @param rule character. Simpson or Trapezoidal rule
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' x = 1:10
#' f = 1:10
#' limits = c(1,10)
#' integrateIt(x, f, limits, "Simpson")
#' integrateIt(x, f, limits, "Trapezoid")
setGeneric(
  name = "integrateIt",
  def = function(x, f, limits, rule, ...)
  {standardGeneric("integrateIt")}
)


#' @export
setMethod(
  f = "integrateIt",
  definition = function(x, f, limits, rule, ...){
    # Validation of inout
    #
    n <-length(x)-1
    h <- (limits[2] - limits[1])/n

    if(rule == "Trapezoid"){

      result = (h / 2) * (f %*% c(1, rep(2, n-1), 1))

      result = result[1,1] # %*% operator returns matrix type

      return(new("Trapezoid", x = x, f = f, result=result))

    } else if(rule == "Simpson"){

      result <- (h / 3) * f %*% c(1, 2 ^ (1:(n-1) %% 2 + 1), 1)

      result = result[1,1] # %*% operator returns matrix type

      return(new("Simpson", x = x, f = f, result=result))

    } else {
      stop("Rule not sopported!")
    }

  }
)
