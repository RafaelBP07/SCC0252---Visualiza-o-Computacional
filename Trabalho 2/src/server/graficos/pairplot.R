# Funçao geral para o grafico pair plot

#-----------------------------------------------------------------------------------------------

# Função lowerFn
lowerFn <- function(data, mapping, method = "lm", ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(colour = "#2596be") +
    geom_smooth(method = method, fill = "#2596be", color = "red", ...)
}

# Função que gera o pair plot
pair_plot <- function(data) {
  num_vars <- ncol(data)
  size_param <- ifelse(num_vars < 4, 10, 6)
  
  p <- ggpairs(data, 
               lower = list(continuous = wrap(lowerFn, method = "lm")),
               diag = list(continuous = wrap("barDiag", fill = "#2596be", colour = "#2596be")),
               upper = list(continuous = wrap("cor", size = size_param, label_params = list(show.signif = FALSE)))) +
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