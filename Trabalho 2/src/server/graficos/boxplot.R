# Funçao geral para o grafico boxplot

#-----------------------------------------------------------------------------------------------

# Função que gera o pair plot
box_plot <- function(data) {
  # Transformar dados para o formato longo
  df_long <- data %>%
    pivot_longer(everything(), names_to = "Variavel", values_to = "Valor")
  
  # Criar boxplot para cada variável numérica com escala própria
  boxplot_plot <- ggplot(df_long, aes(x = "", y = Valor, fill = Variavel)) +
    geom_boxplot() +
    facet_wrap(~Variavel, scales = "free_y", ncol = length(unique(df_long$Variavel))) +
    labs(title = NULL,
         x = NULL,
         y = "Valor") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          legend.position = "none")
  
  # Converter para um gráfico interativo com plotly
  boxplot_plot_interativo <- ggplotly(boxplot_plot)
  
  return(boxplot_plot_interativo)
}

# Exemplo de uso da função
# box_plot(df[, c("Gross", "IMDB_Rating")])

#-----------------------------------------------------------------------------------------------

# Função que renderiza o pair plot para as variáveis especificadas
render_boxplot <- function(input) {
  selected_variables <- input$variaveis_picker
  
  # Verifica se as variáveis são colunas válidas nos dados
  if (!(all(selected_variables %in% names(df)))) {
    stop("Pelo menos uma das variáveis não é uma coluna válida nos dados.")
  }
  
  # Carrega os dados
  data <- carregar_dados("")
  
  data_filtrado <- data[, selected_variables]
  
  # Utiliza a função box_plot para gerar o boxplot
  p <- box_plot(data = data_filtrado)
  
  return(p)
}