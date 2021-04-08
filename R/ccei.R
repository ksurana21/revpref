#' Computes the critical cost efficiency index
#'
#' @param p T X N matrix of prices
#' @param q T X N matrix of quantities
#'
#' @return the critical cost efficiency index
#' @export
#'
#' @note T = number of observations and N = number of goods
#'
#' @examples
ccei <- function(p,q){

  if(garp(p,q)[1] == 1){
    e <- 1
  }
  else{
    eupper <- 1
    elower <- 0

    while ((eupper-elower)/elower >= 0.0000000000001){
      eevaluate = 0.5*(eupper+elower)
      passevaluate = garp(p,q,eevaluate)[1]
      if(passevaluate == 1){
        elower = eevaluate
      }
      else{
        eupper = eevaluate
      }
    }
    e = eevaluate
  }
  e
}
