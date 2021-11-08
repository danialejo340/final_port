
m.treynor = function(retornos, r.indice){
  ## Modelo Treynor
  # Recomendación: Al menos 60 datos de historia (Para calculo betas)
  
  
  # Insumos: Matriz de retornos de los activos y del indice
  sigmam = sd(r.indice)
  
  # Calculos del modelo: Beta y varianza del error (paso 2)
  
  n = length(mu)
  betas = matrix(0,ncol = n)
  varerror = matrix(0,ncol = n)
  #colnames(beta) = activos
  #colnames(varerror) = activos
  
  
  for (i in 1:n) {
    
    modelo = lm(retornos[,i] ~ r.indice) # para obtener los betas
    betas[i] = coefficients(modelo)[2]
    varerror[i] = var(residuals(modelo))
    
  }
  
  # Calculo coeficiente Treynor (paso 1)
  
  
  treynori = (mu-rf) / betas
  matriz = t(rbind(treynori, betas, varerror, mu, sigma))
  matriz.ord = matriz[order(-matriz[,1]),]
  colnames(matriz.ord) = c("Treynor", "Beta", "VarError", "Mu", "Sigma")
  
  # Calculos de Ratios 1 y 2 (paso 3)
  
  
  ratio1 = ((matriz.ord[,4]-rf)*matriz.ord[,2])/matriz.ord[,3]
  ratio2 = matriz.ord[,2]^2/matriz.ord[,3]
  suma1 = cumsum(ratio1)   # sumas acumuladas de los ratios
  suma2 = cumsum(ratio2)
  
  # Calculo Tasa C
  
  tasaC = (sigmam^2*suma1)/(1+sigmam^2*suma2)
  
  
  # plot(tasaC, type = "l", ylim = c(min(treynori), max(treynori)))
  # lines(matriz.ord[,1], col = "red")

  
  diff = matriz.ord[,1]-tasaC
  cond.diff = diff[!is.na(diff) & diff > 0]  # Todo lo que sea negativo que lo omita  
  n.optimo = length(cond.diff)
  Cmax = tasaC[n.optimo]
  
  # Calculos Zi y pesos
  
  zi = (matriz.ord[,2]/matriz.ord[,3])*(matriz.ord[,1]-Cmax)
  zi = pmax(zi, 0)
  
  wpot = zi/sum(zi)
  rpot = t(wpot)%*%mu
  sigmapot = sqrt(t(wpot)%*%cov%*%wpot)
  
  # Creación ista para retornar valores clave del modelo
  
  MT = list()
  MT[[1]] = wpot 
  MT[[2]] = rpot
  MT[[3]] = sigmapot
  MT[[4]] = n.optimo
  return(MT)
}

