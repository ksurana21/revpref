#' Given data set (\code{p},\code{q}), creates appropriate graph list
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
