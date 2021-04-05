#' A function which makes a zero residue graph of given \code{Graph} and distances \code{d}.
#' This is needed to identify a zero length cycle in \code{Graph}.
#' If there exist a cycle in zero residual graph than there exist a zero-length cycle in original \code{Graph}.
#'
#' @param Graph A graph list
#' @param d 2 X N matrix of shortest distance and predecessors (from node 1; computed from \code{\link{bellmanford}})
#'
#' @return Returns an updates graph
#' @export
#'
#' @examples
zero_residual_graph <- function(Graph,d)
{
  g <- list()                                            ## Make a new empty graph g
  n <- length(Graph)
  for(i in 1:n)                                          ## make as many nodes in g and Graph
  {
    g[[i]] <- list()                                     ## For node i, make a list to store out going edges from i
    if(length(Graph[[i]]) > 0)                           ## If there is any edge from i in original "Graph"
    {
      for(k in 1:length(Graph[[i]]))                     ## For each edge from i in "Graph"
      {
        j <- Graph[[i]][[k]][1]
        if(Graph[[i]][[k]][2] + d[1,i] - d[1,j] == 0)    ## check if reduced arc length is of zero length
        {
          vec <- c(j,0)                                  ## if yes, add a edge (i,j) with arc length 0 in new graph
          if(length(g[[i]]) == 0){ g[[i]] <- list(vec)}
          else { g[[i]][[length(g[[i]])+1]] <-vec}

        }
      }
    }
  }
  g
}
