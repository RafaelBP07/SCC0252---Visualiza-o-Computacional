calculate_top_values <- function(data, column, top_n = 10) {
  split_data <- data.frame(Var1 = unlist(strsplit(data[[column]], ', ')))
  
  # Calcular a soma total para cada valor na coluna
  column_totals <- table(split_data$Var1)
  column_totals <- data.frame(Var1 = names(column_totals), Freq = as.numeric(column_totals))
  
  # Filtrar para incluir apenas os top_n valores
  top_values <- column_totals %>%
    arrange(desc(Freq)) %>%
    head(top_n)
  
  return(top_values)
}

# Função para criar um countplot dos 10 principais valores de uma variável
countplot <- function(data, variavel, medida) {
  if (medida == 'Quantidade de Filmes') {
    # Verificar se a variável é 'Genre' antes de ordenar e selecionar os 10 principais valores
    if (variavel == "Genre") {
      top_values <- calculate_top_values(df, 'Genre', '10')
      variavel = "Gêneros"
    }
    
    else if (variavel == "Star") {
      top_values <- calculate_top_values(df, 'Star', '10')
      variavel = "Estrelas"
    }
    
    else if (variavel == "Certificate") {
      # Contagem de frequência da variável
      top_values <- as.data.frame(table(data[[variavel]]))
      top_values <- top_values[order(-top_values$Freq), ]
      top_values <- top_values[1:10, ]
      variavel = "Classificações"
    }
    
    else {
      # Contagem de frequência da variável
      top_values <- as.data.frame(table(data[[variavel]]))
      top_values <- top_values[order(-top_values$Freq), ]
      top_values <- top_values[1:10, ]
      variavel = "Diretores"
    }
    
    p <- ggplot(top_values, aes(x = Freq, y = reorder(Var1, Freq))) +
      geom_bar(stat = "identity", fill = "#2596be") +
      labs(title = paste("Top 10", variavel, "com Mais Filmes"), x = "Quantidade", y = variavel) +  # Substitua Var1 pelo nome da sua variável
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    interativo <- ggplotly(p)
    return(interativo)
  }

  else if (medida == "Faturamento"){
    if (variavel == "Genre"){
      df_genre <- data %>%
        separate_rows(Genre, sep = ", ") %>%
        group_by(Genre) %>%
        summarize(count = switch(
          medida,
          "sum" = sum(!!sym(variavel)),
          "mean" = mean(!!sym(variavel))
        ), total_gross = sum(Gross, na.rm = TRUE))
      
      # Selecionar os 10 maiores gêneros
      top_genres <- df_genre %>%
        arrange(desc(total_gross)) %>%
        slice_head(n = 10) %>%
        pull(Genre)
      
      # Filtrar apenas os 10 maiores gêneros
      df_genre <- df_genre %>%
        filter(Genre %in% top_genres)
      
      # Ordenar os gêneros pelo número total de filmes
      df_genre$Genre <- factor(df_genre$Genre, levels = rev(top_genres))
      
      # Criar o countplot usando ggplot2
      p <- ggplot(df_genre, aes(x = total_gross / 10^9, y = Genre, fill = Genre)) +
        geom_bar(stat = "identity", fill="#2596be") +
        labs(title = "Faturamento Total por Gênero (Bilhões $)", x = "Faturamento", y = "Gênero") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      interativo <- ggplotly(p)
      return(interativo)
    }
    
    else if (variavel == "Star"){
      df_star <- data %>%
        separate_rows(Star, sep = ", ") %>%
        group_by(Star) %>%
        summarize(count = switch(
          medida,
          "sum" = sum(!!sym(variavel)),
          "mean" = mean(!!sym(variavel))
        ), total_gross = sum(Gross, na.rm = TRUE))
      
      # Selecionar os 10 maiores gêneros
      top_star <- df_star %>%
        arrange(desc(total_gross)) %>%
        slice_head(n = 10) %>%
        pull(Star)
      
      # Filtrar apenas os 10 maiores gêneros
      df_star <- df_star %>%
        filter(Star %in% top_star)
      
      # Ordenar os gêneros pelo número total de filmes
      df_star$Star <- factor(df_star$Star, levels = rev(top_star))
      
      # Criar o countplot usando ggplot2
      p <- ggplot(df_star, aes(x = total_gross / 10^9, y = Star, fill = Star)) +
        geom_bar(stat = "identity", fill="#2596be") +
        labs(title = "Faturamento Total por Estrela (Bilhões $)", x = "Faturamento", y = "Estrela") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      interativo <- ggplotly(p)
      return(interativo)
    }
    
    else if (variavel == "Director"){
      df_diretor <- data %>%
        group_by(Director) %>%
        summarize(count = switch(
          medida,
          "sum" = sum(!!sym(variavel)),
          "mean" = mean(!!sym(variavel))
        ), total_gross = sum(Gross, na.rm = TRUE))
      
      # Selecionar os 10 maiores gêneros
      top_diretor <- df_diretor %>%
        arrange(desc(total_gross)) %>%
        slice_head(n = 10) %>%
        pull(Director)
      
      # Filtrar apenas os 10 maiores gêneros
      df_diretor <- df_diretor %>%
        filter(Director %in% top_diretor)
      
      # Ordenar os gêneros pelo número total de filmes
      df_diretor$Director <- factor(df_diretor$Director, levels = rev(top_diretor))
      
      # Criar o countplot usando ggplot2
      p <- ggplot(df_diretor, aes(x = total_gross / 10^9, y = Director, fill = Director)) +
        geom_bar(stat = "identity", fill="#2596be") +
        labs(title = "Faturamento Total por Diretor (Bilhões $)", x = "Faturamento", y = "Diretor") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      interativo <- ggplotly(p)
      return(interativo)
    }
    else {
      df_classif <- data %>%
        group_by(Certificate) %>%
        summarize(count = switch(
          medida,
          "sum" = sum(!!sym(variavel)),
          "mean" = mean(!!sym(variavel))
        ), total_gross = sum(Gross, na.rm = TRUE))
      
      # Selecionar os 10 maiores gêneros
      top_classif <- df_classif %>%
        arrange(desc(total_gross)) %>%
        slice_head(n = 10) %>%
        pull(Certificate)
      
      # Filtrar apenas os 10 maiores gêneros
      df_classif <- df_classif %>%
        filter(Certificate %in% top_classif)
      
      # Ordenar os gêneros pelo número total de filmes
      df_classif$Certificate <- factor(df_classif$Certificate, levels = rev(top_classif))
      
      # Criar o countplot usando ggplot2
      p <- ggplot(df_classif, aes(x = total_gross / 10^9, y = Certificate, fill = Certificate)) +
        geom_bar(stat = "identity", fill="#2596be") +
        labs(title = "Faturamento Total por Classificação (Bilhões $)", x = "Faturamento", y = "Classificação") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      interativo <- ggplotly(p)
      return(interativo)
    }
  }
  else {
    if (variavel == "Genre"){
      df_genre <- data %>%
        separate_rows(Genre, sep = ", ") %>%
        group_by(Genre) %>%
        summarize(count = switch(
          medida,
          "sum" = sum(!!sym(variavel)),
          "mean" = mean(!!sym(variavel))
        ), mean_gross = mean(Gross, na.rm = TRUE))
      
      # Selecionar os 10 maiores gêneros
      top_genres <- df_genre %>%
        arrange(desc(mean_gross)) %>%
        slice_head(n = 10) %>%
        pull(Genre)
      
      # Filtrar apenas os 10 maiores gêneros
      df_genre <- df_genre %>%
        filter(Genre %in% top_genres)
      
      # Ordenar os gêneros pelo número total de filmes
      df_genre$Genre <- factor(df_genre$Genre, levels = rev(top_genres))
      
      # Criar o countplot usando ggplot2
      p <- ggplot(df_genre, aes(x = mean_gross / 10^6, y = Genre, fill = Genre)) +
        geom_bar(stat = "identity", fill="#2596be") +
        labs(title = "Faturamento Médio por Gênero (Milhões $)", x = "Faturamento", y = "Gênero") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      interativo <- ggplotly(p)
      return(interativo)
    }
    
    else if (variavel == "Star"){
      df_star <- data %>%
        separate_rows(Star, sep = ", ") %>%
        group_by(Star) %>%
        summarize(count = switch(
          medida,
          "sum" = sum(!!sym(variavel)),
          "mean" = mean(!!sym(variavel))
        ), mean_gross = mean(Gross, na.rm = TRUE))
      
      # Selecionar os 10 maiores gêneros
      top_star <- df_star %>%
        arrange(desc(mean_gross)) %>%
        slice_head(n = 10) %>%
        pull(Star)
      
      # Filtrar apenas os 10 maiores gêneros
      df_star <- df_star %>%
        filter(Star %in% top_star)
      
      # Ordenar os gêneros pelo número total de filmes
      df_star$Star <- factor(df_star$Star, levels = rev(top_star))
      
      # Criar o countplot usando ggplot2
      p <- ggplot(df_star, aes(x = mean_gross / 10^6, y = Star, fill = Star)) +
        geom_bar(stat = "identity", fill="#2596be") +
        labs(title = "Faturamento Médio por Estrela (Milhões $)", x = "Faturamento", y = "Estrela") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      interativo <- ggplotly(p)
      return(interativo)
    }
    
    else if (variavel == "Director"){
      df_diretor <- data %>%
        group_by(Director) %>%
        summarize(count = switch(
          medida,
          "sum" = sum(!!sym(variavel)),
          "mean" = mean(!!sym(variavel))
        ), mean_gross = mean(Gross, na.rm = TRUE))
      
      # Selecionar os 10 maiores gêneros
      top_diretor <- df_diretor %>%
        arrange(desc(mean_gross)) %>%
        slice_head(n = 10) %>%
        pull(Director)
      
      # Filtrar apenas os 10 maiores gêneros
      df_diretor <- df_diretor %>%
        filter(Director %in% top_diretor)
      
      # Ordenar os gêneros pelo número total de filmes
      df_diretor$Director <- factor(df_diretor$Director, levels = rev(top_diretor))
      
      # Criar o countplot usando ggplot2
      p <- ggplot(df_diretor, aes(x = mean_gross / 10^6, y = Director, fill = Director)) +
        geom_bar(stat = "identity", fill="#2596be") +
        labs(title = "Faturamento Médio por Diretor (Milhões $)", x = "Faturamento", y = "Diretor") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      interativo <- ggplotly(p)
      return(interativo)
    }
    else {
      df_classif <- data %>%
        group_by(Certificate) %>%
        summarize(count = switch(
          medida,
          "sum" = sum(!!sym(variavel)),
          "mean" = mean(!!sym(variavel))
        ), mean_gross = mean(Gross, na.rm = TRUE))
      
      # Selecionar os 10 maiores gêneros
      top_classif <- df_classif %>%
        arrange(desc(mean_gross)) %>%
        slice_head(n = 10) %>%
        pull(Certificate)
      
      # Filtrar apenas os 10 maiores gêneros
      df_classif <- df_classif %>%
        filter(Certificate %in% top_classif)
      
      # Ordenar os gêneros pelo número total de filmes
      df_classif$Certificate <- factor(df_classif$Certificate, levels = rev(top_classif))
      
      # Criar o countplot usando ggplot2
      p <- ggplot(df_classif, aes(x = mean_gross / 10^6, y = Certificate, fill = Certificate)) +
        geom_bar(stat = "identity", fill="#2596be") +
        labs(title = "Faturamento Médio por Classificação (Milhões $)", x = "Faturamento", y = "Classificação") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      interativo <- ggplotly(p)
      return(interativo)
    }
  }
    
  }

render_countplot <- function(input) {
  variavel <- input$variavel_x
  medida <- input$variavel_y
  
  data <- carregar_dados("")
  
  # Utiliza a função pair_plot para gerar o pair plot
  p <- countplot(data, variavel, medida)
  
  return(p)
}
