# Funçao geral para o grafico pair plot

#-----------------------------------------------------------------------------------------------

# Função que gera o pair plot
pair_plot <- function(data) {
  p <- ggpairs(data) +
    theme_minimal()
  
  fig <- ggplotly(p)
  return(fig)

}

# Função que renderiza o pair plot para as variáveis especificadas
render_pair_plot <- function(input) {
  selected_variables <- input$variaveis_picker
  
  # Verifica se as variáveis são colunas válidas nos dados
  if (!(all(selected_variables %in% names(df)))) {
    stop("Pelo menos uma das variáveis não é uma coluna válida nos dados.")
  }
  
  # Carrega os dados
  data <- carregar_dados("sum")
  
  data_filtrado <- data[, selected_variables]
  
  # Utiliza a função pair_plot para gerar o pair plot
  p <- pair_plot(data = data_filtrado)
  
  return(p)
}