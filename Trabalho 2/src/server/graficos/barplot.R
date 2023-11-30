# Funçao geral para o grafico de series

#-----------------------------------------------------------------------------------------------

# Função que gera o gráfico de barras
grafico_1 <- function(data, x_var, y_var, titulo_grafico, eixo_x, eixo_y) {
  p <- ggplot(data, aes(x = !!x_var, y = !!y_var)) +
    geom_bar(stat = "identity", fill = "#2596be") +
    labs(title = titulo_grafico, x = eixo_x, y = eixo_y) +
    theme_minimal()
  
  fig <- ggplotly(p)
  return(fig)
}

# Exemplo de uso da função
# grafico_1(data = df, x_var = Released_Year, y_var = Gross, 
          # titulo_grafico = "Título do Gráfico", eixo_x = "Eixo X", eixo_y = "Eixo Y")

#-----------------------------------------------------------------------------------------------

# Função que renderiza o gráfico de barras para as variáveis especificadas
render_grafico_1 <- function(input) {
  variavel_x <- as.name("Released_Year")
  variavel_y <- as.name(input$variavel)
  periodo <- input$date_slider
  
  # Verifica se as variáveis são colunas válidas nos dados
  if (!(as.character(variavel_x) %in% names(df) && as.character(variavel_y) %in% names(df))) {
    stop("Pelo menos uma das variáveis não é uma coluna válida nos dados.")
  }
  
  # Carrega os dados
  data <- carregar_dados("sum", periodo)
  
  # Utiliza a função grafico_1 para gerar o gráfico
  p <- grafico_1(data = data, x_var = variavel_x, y_var = variavel_y, 
                 titulo_grafico = "titulo", eixo_x = "X", eixo_y = "Y")
  
  return(p)
}