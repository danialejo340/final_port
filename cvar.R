
f.cvar = function(retornos, alpha){

    library("ROI")
    library("ROML")
    library("ROML.portfolio")
    alpha = alpha
    short = short
    if(short == 1){lb = -1}
    else{lb=0}
    
    m <- model()
    m$variable(portfolio, lb = lb) # 1: con cortos
    m$minimize( cvar(portfolio, 0.95) )
    m$subject_to( budget_norm(portfolio) )
    opt <- optimize(m, solver="glpk", 
                    data=list(returns = coredata(retornos))) 
    
    wpcvar = round(opt$solution[grep("portfolio", names(opt$solution))], 6)
    names(wpcvar) = activos
    
    rpcvar = mu%*% wpcvar
    sigmapcvar = sqrt(t(wpcvar)%*% cov %*% wpcvar)
    
    PCV = list()
    PCV[[1]] = cbind(wpcvar)
    PCV[[2]] = rpcvar
    PCV[[3]] = sigmapcvar
    return(PCV)

}






