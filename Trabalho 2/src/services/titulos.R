# Funções que geram titulos para os gráficos

#-----------------------------------------------------------------------------------------------

titulo_1 <- function(variavel) {
  titulo <- ""
  
  if (variavel == "confirmed") {
    aux <- paste("Casos confirmados")
  } else {
    if (variavel == "deaths") {
      aux <- paste("Número de mortos")
    } else {
      aux <- paste("Doses administradas/10000")
    }
  }
  
  if (is.null(est) || est == '') {
    titulo <- paste(aux, "no Brasil")
  } else {
    if (!(is.null(cid) || cid == '')) {
      titulo <- paste(aux, "em", est, "-", cid)
    } else {
      titulo <- paste(aux, "em", est)
    }
  }
  
  return(titulo)
}