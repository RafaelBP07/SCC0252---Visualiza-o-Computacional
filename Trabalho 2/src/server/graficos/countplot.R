# Função geral para o gráfico de contagem

#-----------------------------------------------------------------------------------------------

# Função para criar um countplot
countplot <- function(data, variavel, top_n, ascending) {
  
  top_values <- calculate_top_values(df, variavel, top_n, descending_order = ascending)
  
  variavel <- tradutor(variavel, traducao)
    
  p <- ggplot(top_values, aes(x = Variavel, y = if (ascending) reorder(Class, Variavel) else reorder(Class, -Variavel))) +
    geom_bar(stat = "identity", fill = "#2596be") +
    labs(title = paste("Top", min(top_n, length(top_values$Class)), variavel, "com", ifelse(ascending, "Mais", "Menos"), "Filmes"), x = "Quantidade", y = variavel) +  # Substitua Class pelo nome da sua variável
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  interativo <- ggplotly(p)
  return(interativo)

}

#-----------------------------------------------------------------------------------------------

# Função que renderiza o gráfico de contagem
render_countplot <- function(input) {
  classe <- input$variavel_y
  top_n <- input$top_n
  ascending <- input$ascending
  
  data <- carregar_dados("")
  
  # Utiliza a função pair_plot para gerar o pair plot
  p <- countplot(data, classe, top_n, ascending)
  
  return(p)
}
