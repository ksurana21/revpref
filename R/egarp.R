#' Tests consistency with the Generalized Axiom of Revealed Preference at efficiency level \code{e}
#'
#' @param p T X N matrix of prices
#' @param q T X N matrix of quantities
#' @param e efficiency level
#'
#' @return 1 if the data is consistent with GARP at efficiency level e, returns 0 otherwise.
#' @export
#'
#'@note T = number of observations and N = number of goods
#'
#' @examples
egarp <- function(p,q,e)
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

  for(i in 1:t)
  {
    for(j in 1:t)
    {
      if(i != j)
      {if(RP[i,j] == 1 & PO[j,i] == 1)
      {passgarp = 0}
      }
    }
  }
  passgarp
}
