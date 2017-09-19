

#' Golden Section Search
#'
#' @param func The function to be searched through. In the form of an R function
#' @param a The initial lower limit
#' @param b The initial upper limit
#' @param tolerance The iteration gap below which we stop
#'
#' @return First value of x when we meet the tolerance
#' @export
#'
#' @examples
gs <-function(func,a,b,tolerance,maxIterations=100000,r=1 / 2 * (sqrt(5) - 1)){
  ##


  # define a function to check the tolerance
  tolCheck <- function(a, b, tol) {
    check <- FALSE

    if (abs(a - b) <= tol) {
      check <- TRUE
    }

    return(check)
  }
  ##

  left <- (b - a) * (1 - r) + a
  right <- (b - a) * (r) + a
  opt = NULL
  for (n in 1:maxIterations) {

    if (func(left) <= func(right)) {


      b <- right
      if (tolCheck(a, b, tolerance)) {
        opt <- (b + a) / 2
        return(c(n,opt))
      }else{
        right <- left
        left <- (b - a) * (1 - r) + a
        opt <- (b + a) / 2
      }

    }else{

      a <- left

      if (tolCheck(a, b, tolerance)) {
        opt <- (b + a) / 2
        return(c(n,opt))

      } else{
        left <- right
        right <- (b - a) * r + a
        opt <- (b + a) / 2
      }
    }
  }
  return(NaN)
}
