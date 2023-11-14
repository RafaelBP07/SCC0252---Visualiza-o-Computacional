# Funçao geral para o grafico de series

#-----------------------------------------------------------------------------------------------

# Funçao que gera o grafico de series
grafico_1 <- function(variavel, titulo_grafico, eixo_x, eixo_y) {
  p <- as.data.frame(cbind(serie=series)) %>%
    slice(-1) %>%
    ggplot(aes(x = , y = variavel)) +
    geom_line(color = "#2596be") +
    labs(title = titulo_grafico, x = eixo_x, y = eixo_y) +
    theme_minimal() +
  
  fig <- ggplotly(p)
  
  return(fig)
}

#-----------------------------------------------------------------------------------------------

# Funçao que renderiza o grafico de series para a variavel especificada
render_grafico_1 <- function(input) {
  variavel <- input$variavel
  
  # Verifica se a variável 'variavel' é uma coluna válida nos dados
  if (!(variavel %in% names(df))) {
    stop("A variável 'variavel' não é uma coluna válida nos dados.")
  }
  
  titulo <- titulo_1(variavel)
  
  
  p <- grafico_1(df[[variavel]], titulo, "X", "Y")
  
  return(p)
}