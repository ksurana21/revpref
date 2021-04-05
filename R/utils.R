#' Given dataset (\code{p},\code{q}) creates appropriate graph list
#'
#' @param p T x N matrix of prices
#' @param q T X N matrix of quantities
#' @param bound 1 if upper bound is to be computed, -1 if lower bound is to be computed
#'
#' @note T = number of observations and N = number of goods.
#' For each observation \code{i} in \code{{1,2,..,T}}, there is a list.
#' The list of \code{i}-th observation consists of list of 3-element vectors.
#' The first element stores the observation \code{(k)} such that \code{q_i R_0 q_k}.
#' The second element stores the weight of the edge (\code{p_iq_k-p_iq_i}) for lower bound and (\code{p_iq_i - p_iq_k}) for uppper bound.
#' The third element stores the budget of the edge (\code{p_iq_i}).
#'
#' @return A graph list which is a list of \code{T} lists
#' @export
#'
#' @examples
graph_list <- function(p,q,bound)
{
  t <- dim(p)[1]                                            # number of observations

  z <- matrix(0, nrow = t, ncol = t)                        # z stores expenditure level with pj and qi
  for(j in 1:t)
  {
    for(i in 1:t)
    {
      z[j,i] <- sum(p[j,]*q[i,])
    }
  }

  G <- matrix(0, nrow = t , ncol = t)                      # G stores the direct revealed preference relation
  for(i in 1:t)
  {
    for(j in 1:t)
    {
      if(z[i,j] <= z[i,i]) {G[i,j] = 1}                    # piqj <= piqi => qi R_0 qj
    }
  }

  g <- list()
  if(bound == -1){                                         # graph for computing lower bound
    for(i in 1:t)
    {
      g[[i]] <- list()                                     # create an empty list for each observation
      for(k in 1:length(G[i,]))
      {
        if(k != i & G[i,k] == 1)                           # if qi R_0 qk
        {
          w <- z[i,i] - z[i,k]                             # weight of the edge = piqi - piqk
          b <- z[i,i]                                      # budget of the edge = piqi
          vec <- c(k,w,b)
          if(length(g[[i]]) == 0){ g[[i]] <- list(vec)}    # add the vector to the list of i-th node
          else { g[[i]][[length(g[[i]])+1]] <-vec}
        }
      }

    }
  }


  if(bound == 1){                                          # graph for computing upper bound
    for(i in 1:t)
    {
      g[[i]] <- list()
      for(k in 1:length(G[i,]))
      {
        if(k != i & G[i,k] == 1)
        {
          w <- z[i,k] - z[i,i]
          b <- z[i,i]
          vec <- c(k,w,b)
          if(length(g[[i]]) == 0){ g[[i]] <- list(vec)}
          else { g[[i]][[length(g[[i]])+1]] <-vec}
        }
      }

    }
  }
  g
}



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
