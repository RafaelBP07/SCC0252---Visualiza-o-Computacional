# Função geral para o gráfico de linhas

#-----------------------------------------------------------------------------------------------

# Função que gera o gráfico de linhas
line_plot <- function(data, x_var, y_var, medida_resumo) {
  
  # Verifica se a variável de medida está presente nos dados
  if (!(as.character(y_var) %in% names(data))) {
    stop("A variável não é uma coluna válida nos dados.")
  }
  
  # Adiciona um prefixo ao título indicando se é a soma ou média
  prefixo_titulo <- switch(
    medida_resumo,
    "sum" = "Soma de ",
    "mean" = "Média de ",
    stop("A medida de resumo fornecida não é válida. Use 'sum' ou 'mean'.")
  )
  
  # Monta o título completo
  titulo_completo <- paste(prefixo_titulo, tradutor(as.character(y_var), traducao),
                           " ao Longo do Tempo com Modelo Linear", sep = "")
  
  p <- ggplot(data, aes(x = !!x_var, y = !!y_var, group = 1)) +
    geom_smooth(formula = y ~ x, method = "lm", se = FALSE, color = "#ef2f00", linetype = "dashed") +
    geom_line(color = "#2596be") +
    geom_point(color = "#2596be") +
    labs(title = titulo_completo, x = "Ano de Lançamento", y = tradutor(as.character(y_var), traducao)) +
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
  medida <- input$medida_resumo
  
  # Verifica se as variáveis são colunas válidas nos dados
  if (!(as.character(variavel_x) %in% names(df) && as.character(variavel_y) %in% names(df))) {
    stop("Pelo menos uma das variáveis não é uma coluna válida nos dados.")
  }
  
  # Carrega os dados
  data <- carregar_dados(medida, periodo)
  
  # Utiliza a função linhas para gerar o gráfico de linhas
  p <- line_plot(data, variavel_x, variavel_y, medida)
  
  return(p)
}