# Funçao geral para o grafico de linhas

#-----------------------------------------------------------------------------------------------

# Função que gera o gráfico de linhas
line_plot <- function(data, x_var, y_var, titulo_grafico, eixo_x, eixo_y) {
  p <- ggplot(data, aes(x = !!x_var, y = !!y_var, group = 1)) +
    geom_line(color = "#2596be") +
    geom_point(color = "#2596be") +
    geom_smooth(method = "lm", se = FALSE, color = "#ef2f00") +
    labs(title = titulo_grafico, x = eixo_x, y = eixo_y) +
    theme_minimal()
  
  fig <- ggplotly(p)
  return(fig)
}

#-----------------------------------------------------------------------------------------------

# Função que renderiza o gráfico de linhas para as variáveis especificadas
render_lineplot <- function(input) {
  variavel_x <- as.name("Released_Year")
  variavel_y <- as.name(input$variavel)
  periodo <- input$date_slider
  
  # Verifica se as variáveis são colunas válidas nos dados
  if (!(as.character(variavel_x) %in% names(df) && as.character(variavel_y) %in% names(df))) {
    stop("Pelo menos uma das variáveis não é uma coluna válida nos dados.")
  }
  
  # Carrega os dados
  data <- carregar_dados("mean", periodo)
  
  # Utiliza a função linhas para gerar o gráfico de linhas
  p <- line_plot(data = data, x_var = variavel_x, y_var = variavel_y, 
                titulo_grafico = 'titulo', eixo_x = 'X', eixo_y = 'Y')
  
  return(p)
}