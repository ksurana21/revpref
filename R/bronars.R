#' bronars computes the Bronars power index corresponding to a given data set of prices and quantities
#'
#' @param p T X N matrix of prices
#' @param q T X N matrix of quantities
#' @param m number of iterations
#'
#' @return Bronars power index
#' @export
#'
#' @examples
bronars <- function(p,q,m){

  if(missing(m)){
    m = 1000
  }

  #  number of goods
  t <- dim (p)[1]

  #  number of observations
  n <- dim (p)[2]

  passvector <- rep(0,m)

  for (i in 1:m){
    qrand = randomq(p,q)
    passvector[i] = garp(p,qrand)[1]
  }

  power = 1 - mean(passvector)

  return(power)
}
