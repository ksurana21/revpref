#' An auxiliary function used within \code{\link{minimum_cost_time}}.
#' It updates the weight of each edge of a given graph.
#'
#' @param Graph A graph list where each edge in the graph has a cost and time weights attached to it.
#' @param u A scalar to update the edges of the \code{Graph}
#'
#' @return A new graph with \eqn{new cost = old cost - u*time}
#' @export
#'
#' @examples
redefined_graph <- function(Graph,u)
{
  g <- Graph
  for(i in 1:length(g))                                      ## for each node i
  {
    if(length(g[[i]]) > 0){                                  ## if the node has any outgoing edge
      for(k in 1:length(g[[i]]))                             ## for each outgoing edge
      {
        g[[i]][[k]][2] <- g[[i]][[k]][2] - u*g[[i]][[k]][3]  ## update the cost of edge
      }
    }
  }
  g                                                          ## return updated graph
}
