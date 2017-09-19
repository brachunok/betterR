# binary search

#' Binary search
#'
#' @param funct the function to be optimized. Must be first differentiable and in the R function format
#' @param a minimum search parameter
#' @param b maximum search paramtetr
#' @param tolerance solution gap to indicate we are done
#' @param maxIterations Time out number of iterations
#'
#' @return Gives us the optimal x as well as a dataframe of the iterations
#' @export
#'
#' @examples
bs <- function(funct,a,b,tolerance,maxIterations=1000){
  library("stats")
  library("Deriv")
  # define a function to check the tolerance
  tolCheck <- function(a, b, tol) {
    check <- FALSE

    if (abs(a - b) <= tol) {
      check <- TRUE
    }

    return(check)
  }
  ##

  n <-0

  opt <-NULL
  d<-Deriv(funct,x="x")
  for(n in 1:maxIterations){

    opt <-(b+a)/2


    if(d(opt)<=0){
      a <-opt
    }else{

      b<-opt
    }

    if(tolCheck(a,b,tolerance)){

      opt <- (b+a)/2
      return(c(n,opt))
    }

  }
  return(NaN)

}




