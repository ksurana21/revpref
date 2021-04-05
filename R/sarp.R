SARP <- function(p,q)
{

  passsarp <- 1

  t = dim(p)[1]
  z <- matrix(0, nrow = t, ncol = t)
  for(j in 1:t)
  {
    for(i in 1:t)
    {
      z[j,i] <- sum(p[j,]*q[i,])
    }
  }

  DRP <- diag(t)
  for(i in 1:t)
  {
    for(j in 1:t)
    {
      if(sum(p[i,]*q[j,]) <= sum(p[i,]*q[i,])) {DRP[i,j] = 1}
    }
  }

  RP <- warshall(DRP)

  for(i in 1:t)
  {
    for(j in 1:t)
    {
      if(i != j)
      {if(R[i,j] == 1 & G[j,i] == 1) # i is revealed preferred to j & j is directly revealed preferred to i
      {
        if (all(q[i,]==q[j,]) == FALSE){
          passsarp = 1}
      }
      }
    }
  }
  passsarp
}
