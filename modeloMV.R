
##-------------------------------------------
## Programa optimizacion Media-Varianza (MV)
## Teoria de portafolios - 2021-2
## Con cortos (permite pesos negativos): 1
## Sin cortos (no pesos negativos): 0
##-------------------------------------------

modeloMV <- function(ret){
  # Inputs
  rf <- rf
  mu <- colMeans(ret)
  cov <- cov(ret)
  activos <- names(ret)
  short = 0
  # Optimizacion sin restricciones en cortos
  if(short == 1){
    ones <- rep(1,n)
    x <- t(mu)%*%solve(cov)%*%mu
    y <- t(mu)%*%solve(cov)%*%ones
    z <- t(ones)%*%solve(cov)%*%ones
    d <- x*z - y*y
    g <- (solve(cov,ones)%*%x-solve(cov,mu)%*%y)%*%solve(d)
    h <- (solve(cov,mu)%*%z-solve(cov,ones)%*%y)%*%solve(d)
    rpmin <- min(mu)
    rpmax <- max(mu)*1.5
    nport <- 1000
    j <- seq(rpmin,rpmax, length=nport) 
    wpo <- matrix(c(0), ncol=n, nrow=nport) 
    rpo <- matrix(c(0), nrow=nport)
    sigmapo <- matrix(c(0), nrow=nport)
    wj <- 0
    cont <- 1
    for(i in 1:nport){
      wj <- g + h*j[i] 
      wpo[cont,] <- t(wj)
      rpo[cont,] <- t(wj)%*%mu
      sigmapo[cont,] <- sqrt(t(wj)%*%cov%*%wj)
      cont <- cont+1
    }
    # PMVG
    cov_inv_1 <- solve(cov, ones) 
    wpmvg <- (1/as.numeric(ones %*% cov_inv_1)) * cov_inv_1
    rpmvg <- mu%*%wpmvg
    sigmapmvg <- sqrt(t(wpmvg)%*%cov%*%wpmvg)
    # Sharpe
    Er <- mu-rf 
    Z <- solve(cov,Er)  
    sumZ <- sum(Z) 
    wpt <- Z/sumZ 
    rpt <- t(wpt)%*%mu
    sigmapt <- sqrt(t(wpt)%*%cov%*%wpt)
    wpmvg <- t(wpmvg)
    wpt <- t(wpt)
    
    MV <- list()
    MV[[1]] <- wpo
    MV[[2]] <- rpo
    MV[[3]] <- sigmapo
    MV[[4]] <- t(wpmvg)
    MV[[5]] <- rpmvg
    MV[[6]] <- sigmapmvg
    MV[[7]] <- t(wpt)
    MV[[8]] <- rpt 
    MV[[9]] <- sigmapt
    return(MV)
  }
  # Con restricciones en corto
  else {
    # FE    
    library(quadprog)
    if(min(mu) > 0){rpmin = min(mu)*1.001}
    else{rpmin = 0.00}
    rpmax <- max(mu)*0.999
    n <- length(mu)
    nport <- 1000
    j <- seq(rpmin,rpmax,length=nport)
    sigmapo <- matrix(0,nrow=nport)
    wpo <- matrix(0,nrow=nport, ncol=n)
    Amat <- t(rbind(rep(1,n),mu,diag(1,nrow=n)))
    dvec <- rep(0,n) 
    Dmat <- 2*cov
    for(i in 1:nport){
      bvec <- c(1,j[i],rep(0,n))
      result <- solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=2)
      wpo[i,] <- result$solution
      sigmapo[i,] <- sqrt(result$value)
    }
    rpo <- j
    colnames(wpo) <- c(activos)
    # PMVG
    pmvg <- cbind(sigmapo,wpo)
    pmvg.sort <- pmvg[order(pmvg[,1]),]
    pmvg.sel <- cbind(pmvg.sort[1,])
    wpmvg <- cbind(round(pmvg.sel[2:length(pmvg.sel)],6))
    rownames(wpmvg) <- c(activos)
    rpmvg <- mu%*%wpmvg
    sigmapmvg <- sqrt(t(wpmvg)%*%cov%*%wpmvg)
    # Sharpe    
    sharpe_port <- (rpo-rf)/sigmapo
    sharpe <- cbind(sharpe_port,wpo)
    sharpe.sort <- sharpe[order(-sharpe[,1]),]
    sharpe.sel <- cbind(sharpe.sort[1,])
    wpt <- round(cbind(sharpe.sel[2:length(sharpe.sel)]),6)
    rownames(wpt) <- c(activos)
    rpt <- mu%*%wpt
    sigmapt <- sqrt(t(wpt)%*%cov%*%wpt)
    
    MV <- list()
    MV[[1]] <- wpo
    MV[[2]] <- rpo
    MV[[3]] <- sigmapo
    MV[[4]] <- wpmvg
    MV[[5]] <- rpmvg
    MV[[6]] <- sigmapmvg
    MV[[7]] <- wpt
    MV[[8]] <- rpt 
    MV[[9]] <- sigmapt
    return(MV)
  }
}






