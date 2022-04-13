#' PoisMLE
#'
#' Create an object of class PoisMLE and a plot tgat show the MLE and standard errors (1.96)
#'
#' @slot y numeric.The original data
#' @slot mle numeric. The maximum likelihood estimator for this dataset
#' @slot ll numeric. The log likelihood calculated from the observed data
#' @slot se numeric. Standard error for the MLE
#' @slot SEtype character. The method used to calculate the standard error: basic or bootstrap
#'
#' @author Berta Diaz
#' @return
#' @export
#'

setClass(#We need to setClass to create the structure
  Class = "PoisMLE",
  representation = representation(
    y = "numeric",
    mle = "numeric",
    ll = "numeric",
    se= "numeric",
    SEtype="character"
  ),
  prototype = prototype(
    y=c(),
    mle = c(),
    ll = c(),
    se= c(),
    SEtype=c()

  )
)

#' @export
setValidity( #Create a validator, we can add more validation as we want
  "PoisMLE",
  function(object){
    if(!is.numeric(object@y)) stop("y is not  numeric") #If y it is not numeric, then stop
    if(!all(object@y >= 0)) stop("y must be positive") #If the vector y has negative number, then stop

  }
)

#' @export
setMethod( #Initialize Method
  "initialize", #It is a new class
  "PoisMLE",
  function(.Object, ...) {
    value = callNextMethod() #Call the method PoisMLE
    validObject(value)
    return(value)
  }
)

#' @export
setMethod("plot", "PoisMLE", function(x) { #Set Method to create a plot
  x_ = seq(max(min(x@y)-1,0), max(x@y)+1, 1) #The x axis is the numbers contain in the y vector
  y = dpois(x_, x@mle) #The Y axis is the distribution function of the vector with parameter lambda
  y_upper = y + y * x@se * 1.96 #Upper standard errors
  y_lower = y - y * x@se * 1.96 #Lower standard errors
  plot.new() #Create a plot
  hist( #It is a histogram
    x@y,
    freq=F,
    right = F,
    breaks=seq(min(x@y)-0.5, max(x@y)+0.5, by=1), #Set de breaks in the axis
    main = "MLE", #Title of the plot
    xlab = "", #Name of x axis
    ylab= "", #Name of y axis
    ylim=c(0,max(y_upper)),
    xlim=c(0, max(x_))
  )
  error_color = rgb(0.53, 0.79, 0.88, alpha = 0.5) #Set colors for standard errors
  polygon(
    x=c(x_, rev(x_)),
    y=c(y_lower, rev(y_upper)),
    col= error_color,
    border=NA
  )
  lines(x_, y, col="red") #Set color of the solid line

  colors <- c("grey", "red", error_color) #Set colors of histogram
  labels <- c("Data Hist", "Theorical MLE", "Error 1.96xSE") #Set the labels
  legend("topright", inset=.05, title="Distributions",
         labels, lwd=2, lty=c(1, 1, 1), col=colors) #The place where the legends will be
})
