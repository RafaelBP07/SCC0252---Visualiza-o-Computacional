# Carregar Pacotes
pacman::p_load(shiny, 
               shinydashboard, 
               shinyWidgets, 
               plotly)

#------------------------------------------------------------------------------------------------------------------------

# Carregar dados
df <- read.csv('https://raw.githubusercontent.com/RafaelBP07/SCC0252-Visualizacao-Computacional/main/Trabalho%201/pre_processed_data.csv')

# # Opção agrupando colunas com texto
# # Soma por ano
# df_sum <- aggregate(. ~ Released_Year, data = df, FUN = function(x) if(is.numeric(x)) sum(x, na.rm = TRUE) else x)
# 
# # Média por ano
# df_mean <- aggregate(. ~ Released_Year, data = df, FUN = function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else x)

# Opção removendo colunas com texto
# Encontrar colunas numéricas
numeric_columns <- sapply(df, is.numeric)

# Soma por ano
df_sum <- aggregate(. ~ Released_Year, data = df[, numeric_columns], FUN = sum, na.rm = TRUE)

# Média por ano
df_mean <- aggregate(. ~ Released_Year, data = df[, numeric_columns], FUN = mean, na.rm = TRUE)

#------------------------------------------------------------------------------------------------------------------------

# Input filtros

# Aba pairplot
Vars_pair <- names(df_sum)
Vars_pair <- sort(Vars_pair)
Vars_pair_nomes <- c("Faturamento", "Avaliação IMDB", "Pontuação Meta", "Número de Votos", "Ano de Lançamento", "Duração")

# Aba Análise temporal
Vars_temp <- names(df_sum)
Vars_temp <- sort(setdiff(Vars_temp, "Released_Year"))
Vars_temp_nomes <- c("Faturamento", "Avaliação IMDB", "Pontuação Meta", "Número de Votos", "Duração")

Medidas_resumo <- c("sum", "mean")
Medidas_resumo_nomes <- c("Soma por ano", "Media por ano")


# Aba Comparação
Vars_comp <- names(df)
Vars_comp <- sort(setdiff(Vars_comp, c(Vars_temp, "Released_Year", "Series_Title")))
Vars_comp_nomes <- c("Certificado", "Diretor", "Gênero", "Estrela1", "Estrela2", "Estrela3", "Estrela4")


#------------------------------------------------------------------------------------------------------------------------

# Definir o UI
ui <- dashboardPage(
  dashboardHeader(title = span(icon("film"), "Top 1000 Filmes")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pairplot", tabName = "pag1", icon = icon("chart-line")),
      menuItem("Análise Temporal", tabName = "pag2", icon = icon("timeline")),
      menuItem("Comparação", tabName = "pag3",icon = icon("arrow-down-up-across-line")),
      menuItem("Sobre", tabName = "pag4",icon = icon("circle-info"))
    )
  ),
  
  #------------------------------------------------------------------------------------------------------------------------
  
  dashboardBody(
    uiOutput("style"),
    chooseSliderSkin("Flat"),
    tabItems(
      tabItem("pag1",
              fluidRow(
                column(width = 12,
                       box(title = span(icon("x"), " Selecione as variáveis de sua preferência"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           pickerInput(
                             inputId = "variaveis_picker",
                             label = "Selecione Variáveis:",
                             choices = setNames(Vars_pair, Vars_pair_nomes),
                             options = pickerOptions(
                               actionsBox = TRUE, 
                               size = 10,
                               selectedTextFormat = "count > 3"
                             ), 
                             multiple = TRUE,
                             selected = c("IMDB_Rating", "Gross")
                           ),
                           collapsible = TRUE
                       )
                )
              ),
                
              fluidRow(
                column(width = 12,
                       box(
                         title = span(icon("chart-line"), "Pair Plot"),
                         width = NULL, status = "info", solidHeader = TRUE,
                         plotlyOutput("pairplot", height = 800),
                         plotlyOutput("boxplot", height = 400),
                         collapsible = TRUE, collapsed = FALSE,
                         absolutePanel(
                           dropdown(
                             uiOutput("info_text_pairplot1"),
                             style = "unite", icon = icon("circle-info"),
                             status = "primary", width = "300px",
                             tooltip = tooltipOptions(title = "Clique para ver mais informações sobre esse gráfico!")),
                           top = "4%", left = "1%", width = 300, zIndex = 1000 # left = 1% ou 95%
                         )
                       )
                )
              )
              
      ),
      
      #------------------------------------------------------------------------------------------------------------------------
      
      tabItem(tabName = "pag2",
              fluidRow(
                column(width = 4,
                       box(title = span(icon("x"), " Selecione a variável de sua preferência"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           selectInput("variavel", "Variável Selecionada", setNames(Vars_temp, Vars_temp_nomes), selected = "Gross", selectize = TRUE),
                           collapsible = TRUE
                       )
                ),
                
                column(
                  width = 4,
                  box(title = span(icon("calendar"), " Selecione o período de sua preferência"),
                      width = NULL, status = "info", solidHeader = TRUE,
                      sliderInput("date_slider", "Período", min = min(df$Released_Year), max = max(df$Released_Year),
                                  value = c(min(df$Released_Year), max(df$Released_Year))),
                      collapsible = TRUE
                  )
                ),
                
                column(
                  width = 4,
                  box(title = span(icon("database"), " Selecione a medida resumo por ano de sua preferência"),
                      width = NULL, status = "info", solidHeader = TRUE,
                      selectInput("medida_resumo", "Medida Selecionada", setNames(Medidas_resumo, Medidas_resumo_nomes), selected = "sum", selectize = TRUE),
                      collapsible = TRUE
                  )
                )
                
              ),
              
              
              # fluidRow(
              #   column(width = 12,
              #          box(
              #            title = span(icon("chart-line"), " Gráfico de Barras"),
              #            width = NULL, status = "info", solidHeader = TRUE,
              #            plotlyOutput("barplot", height = 500),
              #            collapsible = TRUE, collapsed = FALSE,
              #            absolutePanel(
              #              dropdown(
              #                uiOutput("info_text_barplot1"),
              #                style = "unite", icon = icon("circle-info"),
              #                status = "primary", width = "300px",
              #                tooltip = tooltipOptions(title = "Clique para ver mais informações sobre esse gráfico!")),
              #              top = "8%", left = "1%", width = 300, zIndex = 1000 # left = 1% ou 95%
              #            )
              #          )
              #   )
              # ),
              
              
              
              fluidRow(
                column(width = 12,
                       box(
                         title = span(icon("chart-line"), " Gráfico de Linhas"),
                         width = NULL, status = "info", solidHeader = TRUE,
                         plotlyOutput("lineplot", height = 500),
                         plotlyOutput("heatmap1", height = 600),
                         collapsible = TRUE, collapsed = FALSE,
                         absolutePanel(
                           dropdown(
                             uiOutput("info_text_lineplot1"),
                             style = "unite", icon = icon("circle-info"),
                             status = "primary", width = "300px",
                             tooltip = tooltipOptions(title = "Clique para ver mais informações sobre esse gráfico!")),
                           top = "4%", left = "1%", width = 300, zIndex = 1000 # left = 1% ou 95%
                         )
                       )
                )
              ),
              
              fluidRow(
                column(width = 12,
                       box(
                         title = span(icon("chart-line"), " Gráfico de Calor"),
                         width = NULL, status = "info", solidHeader = TRUE,
                         plotlyOutput("heatmap2", height = 600),
                         collapsible = TRUE, collapsed = FALSE,
                         absolutePanel(
                           dropdown(
                             uiOutput("info_text_heatmap1"),
                             style = "unite", icon = icon("circle-info"),
                             status = "primary", width = "300px",
                             tooltip = tooltipOptions(title = "Clique para ver mais informações sobre esse gráfico!")),
                           top = "8%", left = "1%", width = 300, zIndex = 1000 # left = 1% ou 95%
                         )
                       )
                )
              ),
      
      ),
      
      #------------------------------------------------------------------------------------------------------------------------
      
      tabItem("pag3",
              fluidRow(
                column(width = 4,
                       box(title = span(icon("x"), " Selecione a variável de sua preferência"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           selectInput("variavel_x", "Variável Eixo X", setNames(Vars_comp, Vars_comp_nomes), selected = "X", selectize = TRUE),
                           selectInput("variavel_y", "Variável Eixo Y", setNames(Vars_comp, Vars_comp_nomes), selected = "X", selectize = TRUE),
                           collapsible = TRUE
                       )
                ),

                
              ),
              
              fluidRow(
                column(width = 12,
                       box(title = span(icon("chart-line"), " XXXXX"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           # fluidRow(column(width = 6,
                           #                 plotlyOutput("grafico_2", height = 500)),
                           #          column(width = 6,
                           #                 plotlyOutput("grafico_3", height = 500))),
                           # fluidRow(column(width = 6,
                           #                 plotlyOutput("grafico_4", height = 500)),
                           #          column(width = 6,
                           #                 plotlyOutput("grafico_5", height = 500))),
                           collapsible = TRUE, collapsed = FALSE
                       )
                )
              )
              
      ),
      
      #------------------------------------------------------------------------------------------------------------------------
      
      tabItem("pag4",
              fluidRow(
                column(width = 12, 
                       box(title = span(icon("circle-info"), " Informaçoes sobre o dashboard"), 
                           width = NULL, status = "info", solidHeader = TRUE,
                           htmlOutput("texto_sobre"),
                           collapsible = TRUE
                       )
                )
              )

      )
      
    )
  )
)