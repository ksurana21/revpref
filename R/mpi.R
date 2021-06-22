#' Computes bounds for the Money Pump Index
#'
#'  following Smeulders et al. (2013).
#' This function is meant for the analysis of prices and quantities data (e.g., of bundles of goods and corresponding prices).
#'
#' @param p T X N matrix of prices
#' @param q T X N matrix of quantities
#'
#' @return 1 X 2 vector of minimum and maximum MPI of dataset (\code{p},\code{q}).
#' @export
#'
#' @note T = number of observations and N = number of goods
#'
#' @references Smeulders, Bart, Laurens Cherchye, Frits CR Spieksma, and Bram De Rock. "The money pump as a measure of revealed preference violations: A comment." Journal of Political Economy 121, no. 6 (2013): 1248-1258.
#'
#' @examples
mpi <- function(p,q)
{

  if((dim(p)[1] != dim(q)[1]) || (dim(p)[2] != dim(q)[2])){
    print("Dimensions of price and quantity matrix do not match")
  }
  else{
    p_new = c()
    q_new = c()
    for(k in 1:dim(p)[1]){
      if(sum(q[k,]) !=0){
        p_new = rbind(p_new,p[k,])
        q_new = rbind(q_new,q[k,])
      }
    }

    d <- graph_list(p_new,q_new,-1)                        ## d stores the graph corresponding to consumption data x
    if(cycle_detection_topo(d)== 0){return(c(0,0))}          ## if graph is acyclic, then there is no GARP violation


    minimum_MPI <- minimum_cost_time(d,-1000000,1000000)
    d <- graph_list(p_new,q_new,1)
    maximum_MPI <- -1* minimum_cost_time(d,-1000000,1000000)
    return(round(c(minimum_MPI,maximum_MPI),5))
  }
}
