# Carregar Pacotes
pacman::p_load(shiny, 
               shinydashboard)

#------------------------------------------------------------

# Carregar Funções
source("./src/services/index.R")
source("./src/server/graficos/index.R")

#------------------------------------------------------------

# Definir o servidor
server <- function(input, output, session) {
  
  print('recarregando')
  #================================================================== GRÁFICOS
    
  output$pairplot <- renderPlotly({
    render_pairplot(input)
  })
  
  output$boxplot <- renderPlotly({
    render_boxplot(input)
  })
  
  output$barplot <- renderPlotly({
    render_barplot(input)
  })
  
  output$lineplot <- renderPlotly({
    render_lineplot(input)
  })
  
  output$heatmap1 <- renderPlotly({
    render_heatmap(input)
  })
  
  output$heatmap2 <- renderPlotly({
    render_heatmap(input, TRUE)
  })
  
  output$countplot <- renderPlotly({
    render_countplot(input)
  })
  
  #================================================================== END: GRÁFICOS

  #================================================================== TEXTOS
  output$info_text_pairplot1 <- renderUI({
    tagList(
      tags$h4(tags$b("Contexto e Justificativa")),
      tags$p("XXX"),
      tags$h4(tags$b("Interpretação do Gráfico")),
      tags$p("XXX"),
      tags$h4(tags$b("Insights e Observações")),
      tags$p("XXX"),
      tags$h4(tags$b("Exemplos e Aplicações Práticas")),
      tags$p("XXX"),
      tags$h4(tags$b("Limitações e Considerações Adicionais")),
      tags$p("XXX")
    )
  })
  
  output$info_text_lineplot1 <- renderUI({
    tagList(
      tags$h4(tags$b("Contexto e Justificativa")),
      tags$p("XXX"),
      tags$h4(tags$b("Interpretação do Gráfico")),
      tags$p("XXX"),
      tags$h4(tags$b("Insights e Observações")),
      tags$p("XXX"),
      tags$h4(tags$b("Exemplos e Aplicações Práticas")),
      tags$p("XXX"),
      tags$h4(tags$b("Limitações e Considerações Adicionais")),
      tags$p("XXX")
    )
  })
  
  output$info_text_heatmap1 <- renderUI({
    tagList(
      tags$h4(tags$b("Contexto e Justificativa")),
      tags$p("XXX"),
      tags$h4(tags$b("Interpretação do Gráfico")),
      tags$p("XXX"),
      tags$h4(tags$b("Insights e Observações")),
      tags$p("XXX"),
      tags$h4(tags$b("Exemplos e Aplicações Práticas")),
      tags$p("XXX"),
      tags$h4(tags$b("Limitações e Considerações Adicionais")),
      tags$p("XXX")
    )
  })
  
  output$info_text_countplot <- renderUI({
    tagList(
      tags$h4(tags$b("Contexto e Justificativa")),
      tags$p("XXX"),
      tags$h4(tags$b("Interpretação do Gráfico")),
      tags$p("XXX"),
      tags$h4(tags$b("Insights e Observações")),
      tags$p("XXX"),
      tags$h4(tags$b("Exemplos e Aplicações Práticas")),
      tags$p("XXX"),
      tags$h4(tags$b("Limitações e Considerações Adicionais")),
      tags$p("XXX")
    )
  })
  
  #================================================================== END: TEXTOS
  
  output$texto_sobre <- renderUI({
    tagList(
      tags$p(tags$b("Origem dos Dados:")),
      tags$p("Os dados utilizados são provenientes do",
             tags$a(href = 'https://www.kaggle.com/datasets/harshitshankhdhar/imdb-dataset-of-top-1000-movies-and-tv-shows', target = "_blank", "Kaggle"), 
             "e passaram por um preprocessamento relacionado ao",
             tags$a(href = 'https://github.com/RafaelBP07/SCC0252-Visualizacao-Computacional/tree/main/Trabalho%201', target = "_blank", "Trabalho 1"), 
             "desta mesma disciplina."),
      
      tags$p(tags$b("Navegação no Dashboard:")),
      tags$p("Explore o dashboard para obter insights detalhados sobre os top 1000 filmes no IMDb. Cada aba oferece funcionalidades específicas para análise:"),
      tags$p(tags$b("Aba 1: Visão Geral")),
      tags$ul(
        tags$li("Explore a correlação e dispersão das variáveis-chave."),
        tags$li("Analise padrões e relações entre as variáveis com um Pairplot dinâmico.")
      ),
      tags$p(tags$b("Aba 2: Análise Temporal")),
      tags$ul(
        tags$li("Observe a evolução do número de filmes ao longo do tempo, destacando tendências significativas."),
        tags$li("Analise como medidas resumo (soma e média) para variáveis como Faturamento, Avaliação IMDb, Pontuação Meta, Número de Votos e Duração evoluíram ao longo dos anos.")
      ),
      tags$p(tags$b("Aba 3: Comparação de Medidas")),
      tags$ul(
        tags$li("Compare diferentes medidas, como certificações, atores e diretores."),
        tags$li("Identifique insights valiosos ao comparar as medidas entre diferentes grupos.")
      ),
      tags$p("Clique nas abas para explorar e analisar os dados interativamente."),
      
      
      tags$p(tags$b("Código no GitHub:")),
      tags$p("O código-fonte deste projeto está disponível no",
             tags$a(href = 'https://github.com/RafaelBP07/SCC0252-Visualizacao-Computacional/tree/main', target = "_blank", "GitHub")),
      
      
      tags$p(tags$b("Significado das variáveis:")),
      tags$ol(
        tags$li("Poster_Link: Link do pôster usado pelo IMDb."),
        tags$li("Series_Title: Nome do filme."),
        tags$li("Released_Year: Ano de lançamento do filme."),
        tags$li("Certificate: Certificado/Classificação obtido pelo filme."),
        tags$li("Runtime: Duração total do filme."),
        tags$li("Genre: Gênero do filme."),
        tags$li("IMDB_Rating: Avaliação do filme no site IMDb."),
        tags$li("Overview: Resumo da história."),
        tags$li("Meta_score: Pontuação obtida pelo filme."),
        tags$li("Director: Nome do diretor."),
        tags$li("Star1, Star2, Star3, Star4: Nomes das estrelas do filme."),
        tags$li("No_of_votes: Número total de votos."),
        tags$li("Gross: Dinheiro arrecadado pelo filme.")
      ),
      
      tags$p(tags$b("Classes de Certificate:")),
      tags$ol(
        tags$li("A: \"Adulto\" - Geralmente indicando que o conteúdo é destinado apenas para adultos."),
        tags$li("UA: \"Adulto Sem Restrições\" - Adequado para todos os adultos, menos restritivo que \"A\"."),
        tags$li("U: \"Universal\" - Adequado para todas as audiências, incluindo crianças."),
        tags$li("PG-13: \"Orientação dos Pais 13\" - Material inadequado para menores de 13 anos."),
        tags$li("R: \"Restrito\" - Destinado a adultos ou espectadores com mais de uma certa idade."),
        tags$li("Unrated: Não oficialmente classificado por uma organização de classificação."),
        tags$li("PG: \"Orientação dos Pais\" - Material pode não ser adequado para crianças."),
        tags$li("G: \"Geral\" - Adequado para todas as audiências, incluindo crianças."),
        tags$li("Passed: Filme aprovado para todas as audiências."),
        tags$li("TV-14: Adequado para maiores de 14 anos."),
        tags$li("16: Indica que o conteúdo é adequado para espectadores com 16 anos ou mais."),
        tags$li("TV-MA: Adequado para adultos, pode conter linguagem forte ou violência gráfica."),
        tags$li("GP: \"Patronato Geral\" - Indicava que o filme era adequado para todas as audiências, com orientação dos pais."),
        tags$li("Approved: Filme aprovado para todas as audiências."),
        tags$li("TV-PG: Adequado para crianças mais velhas com orientação dos pais."),
        tags$li("U/A: \"Universal/Adulto\" - Adequado para todas as audiências com orientação dos pais.")
      )
    )
  })
  
  #------------------------------------------------------------
  
  # CSS para atualizar o layout
  output$style <- renderUI({
    tags$style(HTML(
      "
      /* Estilo para o slider*/
      .irs--flat .irs-bar {
      top: 25px;
      height: 12px;
      background-color: #00C0EF;
    }
    .irs--flat .irs-from, .irs--flat .irs-to, .irs--flat .irs-single {
      color: white;
      font-size: 10px;
      line-height: 1.333;
      text-shadow: none;
      padding: 1px 5px;
      background-color: #00C0EF;
      border-radius: 4px;
    }
    .irs--flat .irs-handle>i:first-child {
      position: absolute;
      display: block;
      top: 0;
      left: 50%;
      width: 2px;
      height: 100%;
      margin-left: -1px;
      background-color: #00C0EF;
    }
    .irs--flat .irs-from:before, .irs--flat .irs-to:before, .irs--flat .irs-single:before {
      position: absolute;
      display: block;
      content: '';
      bottom: -6px;
      left: 50%;
      width: 0;
      height: 0;
      margin-left: -3px;
      overflow: hidden;
      border: 3px solid transparent;
      border-top-color: #00C0EF; 
    }
    
    /* Estilo para a aba ativa no tabBox */
    .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #00C0EF;
    }
      "
    ))
  })
  
}