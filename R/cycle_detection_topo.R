#' Checks for a cycle in a directed graph based on topological ordering
#'
#' @param Graph A graph list
#'
#' @return 0 if graph is acyclic and 1 if graph is cyclic
#' @export
#'
#' @examples
cycle_detection_topo <- function(Graph)
{
  cycle = 0

  g <- Graph                                             ## make copy 'g' of original graph
  incoming <- indegree(g)                                ## incoming stores indegrees of each node in g

  lst = list()
  for(i in 1:dim(incoming)[2])
  {
    if (incoming[1,i] == 0)                              ## if node i has no incoming edge
    {lst[length(lst)+1] = i}                             ## add i to the list
  }

  count = 0
  while(length(lst) > 0){
    node = lst[[length(lst)]][1]                         ## remove last element of the list
    lst[[length(lst)]] = NULL
    count = count + 1                                    ## increase the counter


    if (length(g[[node]]) > 0)                           ## if "node" has any outgoing edge
    {
      for(j in 1:length(g[[node]]))                      ## for each edge emanating from "node"
      {
        i <- g[[node]][[j]][1]                           ## if there is an edge from "node" to i
        incoming[1,i] = incoming[1,i] - 1                ## reduce indegree of i by 1

        if(incoming[1,i] == 0){                          ## if node i has no incoming edge
          lst[length(lst)+1] = i                         ## add i to the list
        }
      }
    }
  }

  if(count < length(Graph)){return(1)}                   ## if not all nodes were present in list, graph is cyclic
  else{return(0)}                                        ## otherwise topological sorting exists, graph is acyclic

}
