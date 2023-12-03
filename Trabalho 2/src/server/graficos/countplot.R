# Função geral para o gráfico de contagem

#-----------------------------------------------------------------------------------------------

# Função filtra os dados
calculate_top_values <- function(data, Class, top_n = 10, 
                                 Variavel = NULL, 
                                 stat_function = "sum", 
                                 descending_order = TRUE) {
  
  # Criar um dataframe dividido pela coluna de interesse
  split_data <- data.frame(Class = unlist(strsplit(data[[Class]], ', ')))
  
  # Calcular a soma ou média total para cada valor na coluna de interesse
  Class_totals <- table(split_data$Class)
  Class_totals <- data.frame(Class = names(Class_totals), Freq = as.numeric(Class_totals))
  
  # Filtrar para incluir apenas os top_n valores
  top_values <- Class_totals %>%
    arrange(desc(Freq) ^ ifelse(descending_order, 1, -1)) %>%
    head(top_n)
  
  # Se uma coluna adicional for especificada, calcular a soma ou média para cada valor na coluna adicional
  if (!is.null(Variavel)) {
    additional_data <- data.frame(Class = unlist(strsplit(data[[Class]], ', ')),
                                  Var2 = rep(data[[Variavel]], sapply(strsplit(data[[Class]], ', '), length)))
    
    # Calcular a soma ou média da coluna adicional
    additional_summary <- additional_data %>%
      group_by(Class) %>%
      summarize(Value = switch(
        stat_function,
        "sum" = sum(as.numeric(Var2), na.rm = TRUE),
        "mean" = mean(as.numeric(Var2), na.rm = TRUE)
      ))
    
    # Combinar os resultados das duas colunas
    top_values <- merge(top_values, additional_summary, by = "Class", all.x = TRUE)
  }
  
  # Ordenar o dataframe resultante
  top_values <- top_values[order(top_values$Freq, decreasing = TRUE), ]
  
  # Verificar se a coluna 'Value' existe no dataframe
  if ("Value" %in% colnames(top_values)) {
    # Renomear 'Value' para 'variavel'
    top_values <- top_values %>%
      rename(Variavel = Value)
  } else if ("Freq" %in% colnames(top_values)) {
    # Renomear 'Freq' para 'variavel' se 'Value' não existir
    top_values <- top_values %>%
      rename(Variavel = Freq)
  }
  
  # Manter apenas as colunas 'Class' e 'variavel'
  top_values <- top_values %>%
    dplyr::select(Class, Variavel)
  
  return(top_values)
}

# # Exemplo de uso da função
# calculate_top_values(df, "Director", 25, "Gross", "mean", FALSE)
# calculate_top_values(df, "Director", 25, "Gross", "mean")

#-----------------------------------------------------------------------------------------------

# Função para criar um countplot
countplot <- function(data, variavel, top_n, ascending) {
  
  top_values <- calculate_top_values(df, variavel, top_n, descending_order = ascending)
  
  variavel <- tradutor(variavel, traducao)
    
  p <- ggplot(top_values, aes(x = Variavel, y = if (ascending) reorder(Class, Variavel) else reorder(Class, -Variavel))) +
    geom_bar(stat = "identity", fill = "#2596be") +
    labs(title = paste(min(top_n, length(top_values$Class)), variavel, "de", ifelse(ascending, "Maior", "Menor"), "Filmes"), x = "Quantidade", y = variavel) +  # Substitua Class pelo nome da sua variável
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
