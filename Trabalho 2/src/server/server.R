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
  
  output$grafico_1 <- renderPlotly({
    render_grafico_1(input)
  })
  
  #================================================================== END: GRÁFICOS

  #================================================================== TEXTOS
  output$texto_sobre <- renderUI({
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