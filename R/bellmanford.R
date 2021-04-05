#' Computes shortest distance from a given node to all other nodes in a given Graph.
#'
#' @param g A graph list
#' @param s An integer indicating node
#'
#' @return If the cycle contains a negative cycle, it returns -1. Otherwise, it returns a 2 x N matrix.
#' The first row of the output matrix stores the shortest distance of the node \code{i} to node \code{s}.
#' The second row of the output matrix stores the index of predecessor node in the shortest path.
#' @export
#'
#' @examples
bellmanford <- function(g,s)
{
  d <- matrix(10^6, nrow = 1, ncol = length(g))   ## stores for each node shortest distance from node s
  pred <- matrix(0, nrow = 1, ncol = length(g))   ## stores the predecessors on the shortest path
  d[1,s] <- 0                                     ## initialization
  pred[1,s] <- 0

  for(t in 1:(length(g)-1))                       ## relax/update edges repeatedly for n-1 times
  {
    for(i in 1:length(g))                         ## for each node i in the graph
    {
      if(length(g[[i]]) > 0)                      ## if the node has any outgoing edge
      {
        for(k in 1:length(g[[i]]))                ## for all outgoing edges from node i
        {
          j <- g[[i]][[k]][1]                     ## for edge(i,j)
          if(d[1,i] + g[[i]][[k]][2] < d[1,j])    ## check if distance of node j can be updated
          {
            d[1,j] = d[1,i] + g[[i]][[k]][2]      ## if yes, update it
            pred[1,j] = i
          }
        }}}}

  for(i in 1:length(g))                           ## detecting negative cycle
  {
    if(length(g[[i]]) >0)
    {
      for(k in 1:length(g[[i]]))
      {
        j <- g[[i]][[k]][1]
        if(d[1,i] + g[[i]][[k]][2] < d[1,j])      ## check if distance of node j can still be updated
        {return(-1)}                              ## if yes, it indicates a negative cycle
      }}}
  result <- rbind(d,pred)                         ## if no negative cycle, return d and pred
  result
}
