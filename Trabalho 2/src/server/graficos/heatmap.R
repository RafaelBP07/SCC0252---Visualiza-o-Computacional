# Função geral para o gráfico de calor

#-----------------------------------------------------------------------------------------------

# Função para criar um gráfico de calor
heatmap_plot <- function(data, measure = "count", variable = NULL, Certificado = FALSE) {
  grouping_var <- ifelse(Certificado == "Classificação", "Certificate", "Genre")
  
  if (is.null(variable)) {
    # Contar o número total de filmes por variável e ano
    df <- suppressMessages(
      data %>%
        separate_rows(!!sym(grouping_var), sep = ", ") %>%
        group_by(Released_Year, !!sym(grouping_var)) %>%
        summarize(count = n()))
  } else {
    # Usar a medida resumo (soma ou média) para a variável específica
    df <- suppressMessages(
      data %>%
        separate_rows(!!sym(grouping_var), sep = ", ") %>%
        group_by(Released_Year, !!sym(grouping_var)) %>%
        summarize(count = switch(
          measure,
          "sum" = sum(!!sym(variable)),
          "mean" = mean(!!sym(variable))
        )))
  }
  
  # Ordenar as variáveis pelo número total de filmes
  order_var <- df %>%
    group_by(!!sym(grouping_var)) %>%
    summarize(total_count = sum(count)) %>%
    arrange(desc(total_count)) %>%
    pull(!!sym(grouping_var))
  
  df[[grouping_var]] <- factor(df[[grouping_var]], levels = rev(order_var))  # Inverter a ordem dos níveis
  
  # Traduzir título
  variable <- tradutor(as.character(variable), traducao)
  class <- tradutor(as.character(grouping_var), traducao)
  
  # Criar o gráfico de mapa de calor
  plot_title <- switch(
    measure,
    "count" = paste("Mapa de Calor da Contagem de Filmes por", class, "ao Longo dos Anos"),
    "sum" = paste("Mapa de Calor da Soma de", variable, "por", class, "ao Longo dos Anos"),
    "mean" = paste("Mapa de Calor da Média de", variable, "por", class, "ao Longo dos Anos")
  )
  
  heatmap_plot <- ggplot(df, aes(x = Released_Year, y = !!sym(grouping_var), fill = count)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "#2596be") +
    labs(title = plot_title,
         x = "Ano de Lançamento",
         y = grouping_var,
         fill = switch(
           measure,
           "count" = "Contagem",
           "sum" = "Soma",
           "mean" = "Média"
         )) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Converter para plotly
  heatmap_plotly <- ggplotly(heatmap_plot)
  
  return(heatmap_plotly)
}


# # Exemplo de uso da função
# # Contagem total de filmes por gênero e ano
# heatmap_plot(df)
# # Contagem total de filmes por classificação e ano
# heatmap_plot(df, Certificado = TRUE)
# 
# # Soma do faturamento por gênero e ano
# heatmap_plot(df, measure = "sum", variable = "Gross")
# 
# # Média da avaliação IMDb por gênero e ano
# heatmap_plot(df, measure = "mean", variable = "IMDB_Rating")

#-----------------------------------------------------------------------------------------------

# Função que renderiza o gráfico de calor para o período especificado
render_heatmap <- function(input, gerar_padrao = FALSE) {
  variavel <- as.name(input$variavel)
  periodo <- input$date_slider
  medida <- input$medida_resumo
  certificado <- input$certificado
  
  # Carrega os dados
  data <- carregar_dados("", periodo)
  
  # Gera o heatmap padrão se 'gerar_padrao' estiver definido como TRUE
  if (gerar_padrao) {
    p <- heatmap_plot(data, Certificado = certificado)
  } else {
    # Utiliza a função heatmap_plot para gerar o gráfico com os parâmetros fornecidos
    p <- heatmap_plot(data, medida, variavel, certificado)
  }
  
  return(p)
}
