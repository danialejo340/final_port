
## ---------------------------------
## Programa evaluacion de desempeno
## Teoria de portafolios - 2021-2
## ---------------------------------


performance <- function(ret ,r.indice){
  t <- nrow(ret)
  rport <- matrix(0,nrow=t,ncol=8) 
  colnames(rport) <- c("PMVG","Sharpe","Treynor", "Sortino", "Omega","Cvar", "BL", "Benchmark") 
  vport <- matrix(0,nrow=t,ncol=8)
  colnames(vport) <- c("PMVG","Sharpe","Treynor", "Sortino", "Omega","Cvar", "BL","Benchmark") 
  
  # Retornos
  
  # PMVG
  rpmv = ret%*%wpmv
  rport[,1] = rpmv
  
  # Sharpe
  rpsharpe = ret%*%wpt
  rport[,2] = rpsharpe 
  
  # Treynor
  rptreynor = ret%*%wpot
  rport[,3] = rptreynor
  
  # Sortino
  rpsortino = ret%*%wpts
  rport[,4] = rpsortino
  
  # Omega
  rpomega = ret%*%wpomega
  rport[,5] = rpomega
  
  #CVaR
  rpcvar = ret%*%wpcvar
  rport[,6] = rpcvar
  
  #BL
  rpbl = ret%*%wptBL
  rport[,7] = rpbl
  
  # Benchmark
  r.benchmark <- r.indice
  rport[,8] <- r.benchmark
  
  # Valor del portafolio
  # PMV
  
  port.mv <- matrix(0, nrow=t)
  port.mv[1] <- valor
  for(i in 2:t){
    port.mv[i] <- port.mv[i-1]*exp(rpmv[i-1])
  }
  vport[,1] <- port.mv
  
  
  # Sharpe
  
  port.sharpe <- matrix(0, nrow=t)
  port.sharpe[1] <- valor
  for(i in 2:t){
    port.sharpe[i] <- port.sharpe[i-1]*exp(rpsharpe[i-1])
  }
  vport[,2] <- port.sharpe
  
  # Treynor
  
  port.treynor <- matrix(0, nrow=t)
  port.treynor[1] <- valor
  for(i in 2:t){
    port.treynor[i] <- port.treynor[i-1]*exp(rptreynor[i-1])
  }
  vport[,3] <- port.treynor
  
  # Sortino
  
  port.sortino <- matrix(0, nrow=t)
  port.sortino[1] <- valor
  for(i in 2:t){
    port.sortino[i] <- port.sortino[i-1]*exp(rpsortino[i-1])
  }
  vport[,4] <- port.sortino
  
  # Omega 
  
  port.omega <- matrix(0, nrow=t)
  port.omega[1] <- valor
  for(i in 2:t){
    port.omega[i] <- port.omega[i-1]*exp(rpomega[i-1])
  }
  vport[,5] <- port.omega
  
  #CVaR
  v.cvar = matrix(0, nrow = t)
  v.cvar[1] = valor
  
  for(i in 2:t){
    v.cvar[i] = v.cvar[i-1]*exp(rpcvar[i-1])
  }
  vport[,6] <- v.cvar
  
  #BL
  vBL = matrix(0, nrow = t)
  vBL[1] = valor
  
  for(i in 2:t){
    vBL[i] = vBL[i-1]*exp(rpbl[i-1])
  }
  vport[,7] <- vBL
  
  
  # Benchmark
  
  v.benchmark <- matrix(0, nrow = t)
  v.benchmark[1] <- valor
  
  for(i in 2:t){
    v.benchmark[i] <- v.benchmark[i-1]*exp(r.benchmark[i-1])
  }
  vport[,8] <- v.benchmark
  
  
  
  DH <- list()
  DH[[1]] <- vport
  DH[[2]] <- rport
  
  VBA = list()
  VBA[[1]] = rpmv
  VBA[[2]] = rpsharpe
  VBA[[3]] = rptreynor
  VBA[[4]] = rpsortino
  VBA[[5]] = rpomega
  VBA[[6]] = rpcvar
  VBA[[7]] = rpbl
  VBA[[8]] = r.benchmark
 
  return(DH, VBA)
}
