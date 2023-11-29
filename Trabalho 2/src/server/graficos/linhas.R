# Função que gera o gráfico de linhas
grafico_2 <- function(data, x_var, y_var, titulo_grafico, eixo_x, eixo_y) {
  p <- ggplot(data, aes(x = !!x_var, y = !!y_var, group = 1)) +
    geom_line(color = "#2596be") +
    labs(title = titulo_grafico, x = eixo_x, y = eixo_y) +
    theme_minimal()
  
  fig <- ggplotly(p)
  return(fig)
}

# Função que renderiza o gráfico de linhas para as variáveis especificadas
render_grafico_2 <- function(input) {
  variavel_x <- as.name("Released_Year")
  variavel_y <- as.name(input$variavel)
  
  # Verifica se as variáveis são colunas válidas nos dados
  if (!(as.character(variavel_x) %in% names(df) && as.character(variavel_y) %in% names(df))) {
    stop("Pelo menos uma das variáveis não é uma coluna válida nos dados.")
  }
  
  # Carrega os dados
  data <- carregar_dados("sum")
  
  # Filtra os dados com base no valor do slider
  data_filtrado <- data[data$Released_Year >= input$date_slider[1] & data$Released_Year <= input$date_slider[2], ]
  
  # Utiliza a função grafico_2 para gerar o gráfico de linhas
  p <- grafico_2(data = data_filtrado, x_var = variavel_x, y_var = variavel_y,
                 titulo_grafico = 'titulo', eixo_x = 'X', eixo_y = 'Y')
  
  return(p)
}
