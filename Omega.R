
#  -------------------------------------------
# Funcion optimización omega

f.omega = function(retornos, h){

  library("ROI")
  library("ROML")
  library("ROML.portfolio")
  
  short = short 
  if(short == 1){lb = 1}
  else{lb = 0}
  
  m <- model()
  m$variable(portfolio, lb = 0) # the portfolio choice vector; 
  m$maximize( omega(portfolio) )
  opt <- optimize(m, solver="glpk", 
                  data=list(returns = coredata(retornos))) 
  wpomega = round(opt$solution[grep("portfolio", names(opt$solution))]/opt$solution[grep("z", names(opt$solution))],4)
  
  names(wpomega) = activos
  
  
  rpomega = mu %*% wpomega
  sigmaomega = sqrt(t(wpomega) %*% cov %*% wpomega)
  
  PO = list()
  PO[[1]] = cbind(wpomega)
  PO[[2]] = rpomega
  PO[[3]] = sigmaomega
  # PO[[4]] = exportar la medida omega
  return(PO)
}








