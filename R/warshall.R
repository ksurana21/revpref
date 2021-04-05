#' Algorithm to compute indirect revealed preference relations (R)
#'
#' @param RO T X T matrix of direct revealed preference relations
#'
#' @return T X T matrix of indirect revealed preference relations which is a transitive closure of \code{RO}.
#' @export
#'
#' @note T = number of observations
#'
#' @examples
warshall <- function(RO)
{
  t <- dim(RO)[1]
  R <- RO
  for(k in 1:t)
  {
    for(i in 1:t)
    {
      for(j in 1:t)
      {
        if (R[i,j] != 1)
        {if(R[i,k] == 1 & R[k,j] == 1){R[i,j] = 1}}
      }
    }
  }
  R
}
