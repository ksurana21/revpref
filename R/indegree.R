#' A function to compute the number of incoming edges to each node of a given graph.
#'
#' @param g A graph list
#'
#' @return A 1 X T matrix with in-degree of each node where T is the number of observations.
#' @export
#'
#' @examples
indegree <- function(g)
{
  incoming <- matrix(0,nrow=1, ncol=length(g))       ## "incoming" stores number of incoming edges for each node

  for (k in 1:length(g))                             ## for each node k in graph g
  {
    if(length(g[[k]]) > 0)
    {
      for(j in 1:length(g[[k]]))                     ## for each node having incoming edge from k
      {
        i <- g[[k]][[j]][1]                          ## for each node i having incoming edge from k
        incoming[1,i] = incoming[1,i] + 1            ## increase the count of indegree for node i
      }
    }
  }
  incoming
}
