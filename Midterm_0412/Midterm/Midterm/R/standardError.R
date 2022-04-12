#' Standard Error
#'
#' Calculate the Standard Error. This function can take two values: basic or bootstrap
#'
#' @param y numeric. The observed data
#' @param SEtype character. Two values: basic or bootstrap
#'
#' @author Berta Diaz
#'
#' @return
#' @export
#'
#' @examples
#' y=c(1,2,3,4)
#' standardError(y, "basic")
setGeneric( #Create a generic
  name = "standardError",
  def = function(y, SEtype)
  {standardGeneric("standardError")}
)


#' @export
setMethod( #Create the functions
  f = "standardError",
  definition = function(y, SEtype){

    n <-length(y)

    if(SEtype == "basic"){ #Create a function

    se_basic <- sqrt(mle(y)/n)

      return(se_basic)

    } else if(SEtype == "bootstrap"){
      B <- 50 #The B is going to be equal to 50
        for(i in 1:B) {
          samples[[i]] <-  sample(y, n, replace= TRUE)#Set a sample with size n, replacement
        }
      msample <- matrix(unlist(samples), n, B)#Matrix form
      mean_mle <- apply(msample, 2, mle)#Calculate the mle throughout all the matrix with apply
      sdev <- sd(mean_mle)#Calculate the standard deviation
      return(sdev)#Return the standard deviation

    } else {
      stop("SEtype not sopported!")#In case the rule is not supported, then stopped
    }

  }
)
