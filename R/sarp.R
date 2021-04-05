#' Checks consistency with Strong Axiom of Revealed Preferences (SARP).
#'
#' @param p T X N matrix of prices.
#' @param q T X N matrix pf quantities.
#'
#' @note T = number of observations and N = number of goods
#'
#' @return \code{(passsarp,nviol)} where \code{passsarp} = 1 if data is consistent with SARP and = 0 otherwise.
#' \code{nviol} is the number of SARP violations.
#' @export
#'
#' @examples
sarp <- function(p,q)
{

  passsarp <- 1
  t <- dim(p)[1]
  DRP <- diag(t)
  for(i in 1:t)
  {
    for(j in 1:t)
    {
      if(sum(p[i,]*q[j,]) <= sum(p[i,]*q[i,])) {DRP[i,j] = 1}
    }
  }

  RP <- warshall(DRP)
  nviol <- 0

  for(i in 1:t)
  {
    for(j in 1:t)
    {
      if(i != j)
      {if(RP[i,j] == 1 & DRP[j,i] == 1) # i is revealed preferred to j & j is directly revealed preferred to i
      {
        if(any(q[i,]!= q[j,])){
          passsarp = 0
          nviol = nviol + 1}
      }
      }
    }
  }
  return(c(passsarp,nviol))
}
