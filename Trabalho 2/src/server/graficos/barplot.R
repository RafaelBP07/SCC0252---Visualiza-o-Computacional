# Função geral para o gráfico de barras

#-----------------------------------------------------------------------------------------------

# Função para criar um barplot
barplot <- function(data, variavel, top_n, variavel_x, medida_resumo, ascending) {
  
  top_values <- calculate_top_values(df, variavel, top_n, variavel_x, medida_resumo, ascending)

  variavel <- tradutor(variavel, traducao)
  
  variavel_x <- tradutor(variavel_x, traducao)
  
  p <- ggplot(top_values, aes(x = Variavel, y = if (ascending) reorder(Class, Variavel) else reorder(Class, -Variavel))) +
    geom_bar(stat = "identity", fill = "#2596be") +
    labs(title = paste("Top", min(top_n, length(top_values$Class)), variavel, "de", ifelse(ascending, "Maior", "Menor"), variavel_x, ifelse(medida_resumo == "sum", "(Total)", "(Média)")), x = variavel_x, y = variavel) +  # Substitua Class pelo nome da sua variável
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  interativo <- ggplotly(p)
  return(interativo)
  
}

#-----------------------------------------------------------------------------------------------

# Função que renderiza o gráfico de contagem
render_barplot <- function(input) {
  classe <- input$variavel_y
  top_n <- input$top_n
  variavel_x <- input$variavel_x
  medida_resumo_1 <- input$medida_resumo_1
  ascending <- input$ascending
  
  data <- carregar_dados("")
  
  # Utiliza a função pair_plot para gerar o pair plot
  p <- barplot(data, classe, top_n, variavel_x, medida_resumo_1, ascending)
  
  return(p)
}
