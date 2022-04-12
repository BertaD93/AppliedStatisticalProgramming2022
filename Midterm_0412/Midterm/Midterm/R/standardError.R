#' Title
#'
#' @param y
#' @param SEtype
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
setGeneric( #Create a generic
  name = "standardError",
  def = function(y, SEtype, ...)
  {standardGeneric("standardError")}
)


#' @export
setMethod( #Create the functions
  f = "standardError",
  definition = function(y, SEtype, ...){

    n <-length(y)

    if(SEtype == "basic"){ #Create a function

    se_basic <- sqrt(mle(y)/n)

      return(se_basic)

    } else if(SEtype == "bootstrap"){
      B <- 50
        for(i in 1:B) {
          samples[[i]] <-  sample(y, n, replace= TRUE)
        }
      msample <- matrix(unlist(samples), n, B)
      mean_mle <- apply(msample, 2, mle)
      sdev <- sd(mean_mle)
      return(sdev)

    } else {
      stop("Function not sopported!")
    }

  }
)
