#' Tests consistency with the Weak Axiom of Revealed Preference (WARP)
#'
#' @param p T X N matrix of prices
#' @param q T X N matrix of quantities
#' @param e efficiency level
#'
#' @return \code{(passwarp, nviol)} where \code{passwarp} = 1 if the data is consistent with WARP and = 0 otherwise.
#' \code{nviol} is the number of WARP violations.
#' @export
#'
#' @note T = number of observations and N = number of goods
#'
#' @examples
warp <- function(p,q,e=1)
{

  passwarp <- 1
  t <- dim(p)[1]
  DRP <- diag(t)
  for(i in 1:t)
  {
    for(j in 1:t)
    {
      if(sum(p[i,]*q[j,]) <= e*sum(p[i,]*q[i,])) {DRP[i,j] = 1}
    }
  }


  nviol = 0
  for(i in 1:t)
  {
    for(j in 1:t)
    {
      if(i != j)
      {if(DRP[i,j] == 1 & DRP[j,i] == 1) # i is revealed preferred to j & j is directly revealed preferred to i
      {
        if(any(q[i,]!= q[j,])){
          passwarp = 0
          nviol = nviol + 1}
      }
      }
    }
  }
  return(c(passwarp,nviol))
}
