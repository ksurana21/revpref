#' Computes the minimum cost-to-time ratio of a graph
#'
#' @param Graph A graph list
#' @param low A lower bound for search value
#' @param high An upper bound for search value
#'
#' @return If the graph is acyclic, it returns 0. Otherwise, if the graph contains a cycle, it return the optimal ratio of minimum cost time.
#' @export
#'
#' @examples
minimum_cost_time <- function(Graph,low,high)
{
  if(cycle_detection_topo(Graph)==0){
    return(0)}                                               ## check if Graph is acyclic
  else{
    if(abs(high-low) < 10^-10){return((high+low)/2)}         ## if convergence criterion is satisfied, return
    u <- (low + high)/2                                      ## else guess u
    g <- redefined_graph(Graph,u)                            ## update the graph
    d <- bellmanford(g,1)
    if(is.matrix(d)){                                        ## if there is no negative cycle
      e <- zero_residual_graph(g,d)                          ## check if there is cycle with zero length
      cycle <- cycle_detection_topo(e)
      if(cycle == 1){return(u)}                              ## if yes, return u
      else{minimum_cost_time(Graph,u,high)                   ## else, guess a higher value of u
      }
    }
    else{minimum_cost_time(Graph,low,u)                      ## else, there is a negative cycle, guess lower u
    }
  }
}
