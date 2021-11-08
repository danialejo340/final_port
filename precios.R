
## ----------------------------------
## Programa para importar los precios
## Teoria de portafolios - 2021-2
## ----------------------------------

f.precios <- function(activos,fechai,fechaf,periodicidad){
    precios <- xts()
    for(i in 1:length(activos)){
        aux <- Ad(getSymbols(activos[i],from=fechai,to=fechaf,
                             periodicity=periodicidad,auto.assign=FALSE))
        aux <- na.approx(aux,na.rm=FALSE) # Interpolaci?n de datos con NA
        precios <- cbind(precios,aux)
    }
    colnames(precios) <- activos
    tclass(precios) <- "Date"
    return(precios)
}

