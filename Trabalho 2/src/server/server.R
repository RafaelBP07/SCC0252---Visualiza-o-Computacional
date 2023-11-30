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
  
  output$info_text_barplot1 <- renderUI({
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
  
  #================================================================== END: TEXTOS
  
  output$texto_sobre <- renderUI({
    tagList(
      tags$p(tags$b("Origem dos Dados:")),
      tags$p("Os dados utilizados são provenientes do",
             tags$a(href = 'https://www.kaggle.com/datasets/harshitshankhdhar/imdb-dataset-of-top-1000-movies-and-tv-shows', target = "_blank", "kaggle"), "."),
      
      tags$p(tags$b("Navegação no Dashboard:")),
      tags$p("Clique nas abas abaixo para explorar diferentes aspectos dos dados:"),
      tags$p("1.", tags$a("Pairplot", onclick = "openTab('pairplot')", href = "#"), 
             "- Nesta seção, você pode visualizar um pairplot das variáveis selecionadas para entender as relações entre elas."),
      tags$p("2.", tags$a("Análise Temporal", onclick = "openTab('analise_temporal')", href = "#"), 
             "- Explore a evolução temporal das variáveis relacionadas aos filmes, observando tendências ao longo do tempo."),
      tags$p("3.", tags$a("Comparação", onclick = "openTab('comparacao')", href = "#"), 
             "- Compare diferentes variáveis para obter insights sobre como elas se relacionam."),

      tags$p(tags$b("Código no GitHub:")),
      tags$p("O código-fonte deste projeto está disponível no GitHub. Você pode acessá-lo em:",
             tags$a(href = 'https://github.com/RafaelBP07/SCC0252-Visualizacao-Computacional/tree/main', target = "_blank", "GitHub Repository")),
      
      tags$p("Para mais informações sobre cada aba, consulte a seção correspondente.")
      
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