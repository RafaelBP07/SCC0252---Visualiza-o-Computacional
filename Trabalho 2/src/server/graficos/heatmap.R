# Função geral para o gráfico de calor

#-----------------------------------------------------------------------------------------------

# Função para criar um gráfico de calor
heatmap_plot <- function(data, measure = "count", variable = NULL, Certificado = FALSE) {
  if (Certificado == "Classificação"){
    if (is.null(variable)) {
      # Contar o número total de filmes por certificado e ano
      df_certificate <- data %>%
        group_by(Released_Year, Certificate) %>%
        summarize(count = n())
    } else {
      # Usar a medida resumo (soma ou média) para a variável específica
      df_certificate <- data %>%
        group_by(Released_Year, Certificate) %>%
        summarize(count = switch(
          measure,
          "sum" = sum(!!sym(variable)),
          "mean" = mean(!!sym(variable))
        ))
    }
    
    # Ordenar os certificados pelo número total de filmes
    certificate_order <- df_certificate %>%
      group_by(Certificate) %>%
      summarize(total_count = sum(count)) %>%
      arrange(desc(total_count)) %>%
      pull(Certificate)
    
    df_certificate$Certificate <- factor(df_certificate$Certificate, levels = rev(certificate_order))  # Inverter a ordem dos níveis
    
    # Traduzir titulo
    variable <- tradutor(as.character(variable), traducao)
    
    # Criar o gráfico de mapa de calor
    certificate_heatmap_plot <- ggplot(df_certificate, aes(x = Released_Year, y = Certificate, fill = count)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "#2596be") +
      labs(title = switch(
        measure,
        "count" = "Mapa de Calor da Contagem de Filmes por Classificação ao Longo dos Anos",
        "sum" = paste("Mapa de Calor da Soma de", variable, "por Classificação ao Longo dos Anos"),
        "mean" = paste("Mapa de Calor da Média de", variable, "por Classificação ao Longo dos Anos")
      ),
      x = "Ano de Lançamento",
      y = "Classificação",
      fill = switch(
        measure,
        "count" = "Contagem",
        "sum" = "Soma",
        "mean" = "Média"
      )) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Converter para plotly
    certificate_heatmap_plotly <- ggplotly(certificate_heatmap_plot)
    
    return(certificate_heatmap_plotly)
  }
  
  if (is.null(variable)) {
    # Contar o número total de filmes por gênero e ano
    df_genre <- data %>%
      separate_rows(Genre, sep = ", ") %>%
      group_by(Released_Year, Genre) %>%
      summarize(count = n())
  } else {
    # Usar a medida resumo (soma ou média) para a variável específica
    df_genre <- data %>%
      separate_rows(Genre, sep = ", ") %>%
      group_by(Released_Year, Genre) %>%
      summarize(count = switch(
        measure,
        "sum" = sum(!!sym(variable)),
        "mean" = mean(!!sym(variable))
      ))
  }
  
  # Ordenar os gêneros pelo número total de filmes
  genre_order <- df_genre %>%
    group_by(Genre) %>%
    summarize(total_count = sum(count)) %>%
    arrange(desc(total_count)) %>%
    pull(Genre)
  
  df_genre$Genre <- factor(df_genre$Genre, levels = rev(genre_order))  # Inverter a ordem dos níveis
  
  # Traduzir titulo
  variable <- tradutor(as.character(variable), traducao)
  
  # Criar o gráfico de mapa de calor
  heatmap_plot <- ggplot(df_genre, aes(x = Released_Year, y = Genre, fill = count)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "#2596be") +
    labs(title = switch(
      measure,
      "count" = "Mapa de Calor da Contagem de Filmes por Gênero ao Longo dos Anos",
      "sum" = paste("Mapa de Calor da Soma de", variable, "por Gênero ao Longo dos Anos"),
      "mean" = paste("Mapa de Calor da Média de", variable, "por Gênero ao Longo dos Anos")
    ),
    x = "Ano de Lançamento",
    y = "Gênero",
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
