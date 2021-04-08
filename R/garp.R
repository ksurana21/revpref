#' Tests consistency with Generalized Axiom of Revealed Preferences (GARP) at efficiency level e
#'
#' @param p T X N matrix of prices
#' @param q T X N matrix of quantities
#' @param e efficiency level. Defaults to 1.
#'
#' @return \code{(passgarp, nviol)} where \code{passgarp} = 1 if the data is consistent with GARP at efficiency level e and = 0 otherwise.
#' \code{nviol} is the number of GARP violations.
#' @export
#'
#' @note T = number of observations and N = number of goods
#'
#' @examples
garp <- function(p,q,e=1)
{

  passgarp <- 1
  t <- dim(p)[1]
  DRP <- diag(t)
  PO <- matrix(0, nrow=t,ncol=t)

  for(i in 1:t)
  {
    for(j in 1:t)
    {
      if(sum(p[i,]*q[j,]) <= e*sum(p[i,]*q[i,])) {DRP[i,j] = 1}
      if(sum(p[i,]*q[j,]) < e*sum(p[i,]*q[i,]))  {PO[i,j] = 1}
    }
  }

  RP <- warshall(DRP)
  nviol <- 0
  for(i in 1:t)
  {
    for(j in 1:t)
    {
      if(i != j)
      {if(RP[i,j] == 1 & PO[j,i] == 1)
      {passgarp = 0
      nviol = nviol + 1
      }
      }
    }
  }
  return(c(passgarp,nviol))
}
