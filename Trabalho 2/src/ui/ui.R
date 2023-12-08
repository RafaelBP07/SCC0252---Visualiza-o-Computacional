# Carregar Pacotes
pacman::p_load(shiny, 
               shinydashboard, 
               shinyWidgets, 
               plotly)
#------------------------------------------------------------

# Ocultar warnings
options(warn=-1)

#------------------------------------------------------------------------------------------------------------------------

# Carregar dados
df <- read.csv('https://raw.githubusercontent.com/RafaelBP07/SCC0252-Visualizacao-Computacional/main/Trabalho%201/pre_processed_data.csv')

df <- unite(df, Star, Star1, Star2, Star3, Star4, sep = ", ", remove = FALSE)

df <- df[, !(names(df) %in% c("Star1", "Star2", "Star3", "Star4"))]

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
Vars_comp_nomes <- c("Classificação", "Diretor", "Gênero", "Estrela")
Vars_comp_medidas <- c("Quantidade de Filmes", "Faturamento", "Faturamento Médio")

#------------------------------------------------------------------------------------------------------------------------

# Definir o UI
ui <- dashboardPage(
  dashboardHeader(title = span(icon("film"), "Top Filmes IMDb")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão Geral", tabName = "pag1", icon = icon("chart-line")),
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
                         title = span(icon("chart-column"), "Análise de correlação, distribuição e dispersão"),
                         width = NULL, status = "info", solidHeader = TRUE,
                         plotlyOutput("pairplot", height = 800),
                         plotlyOutput("boxplot", height = 400),
                         collapsible = TRUE, collapsed = FALSE,
                         absolutePanel(
                           dropdown(
                             uiOutput("info_text_pairplot1"),
                             style = "unite", icon = icon("circle-info"),
                             status = "primary", width = "900px",
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
                  box(title = span(icon("database"), " Selecione a medida resumo de sua preferência"),
                      width = NULL, status = "info", solidHeader = TRUE,
                      selectInput("medida_resumo", "Medida Selecionada", setNames(Medidas_resumo, Medidas_resumo_nomes), selected = "sum", selectize = TRUE),
                      collapsible = TRUE
                  )
                )
                
              ),
              
              
              fluidRow(
                column(width = 12,
                       box(
                         title = span(icon("chart-line"), " Análise temporal para as principais variáveis"),
                         width = NULL, status = "info", solidHeader = TRUE,
                         plotlyOutput("lineplot", height = 500),
                         plotlyOutput("heatmap1", height = 600),
                         collapsible = TRUE, collapsed = FALSE,
                         absolutePanel(
                           dropdown(
                             uiOutput("info_text_lineplot1"),
                             style = "unite", icon = icon("circle-info"),
                             status = "primary", width = "900px",
                             tooltip = tooltipOptions(title = "Clique para ver mais informações sobre esse gráfico!")),
                           top = "4%", left = "0.5%", width = 300, zIndex = 1000 # left = 1% ou 95%
                         )
                       )
                )
              ),
              
              
              fluidRow(
                column(width = 4,
                       box(title = span(icon("square-check"), " Selecione a classe de sua preferência"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           radioGroupButtons(
                             inputId = "certificado",
                             label = "Label",
                             choices = c("Gênero", 
                                         "Classificação"),
                             checkIcon = list(
                               yes = icon("ok",
                                          lib = "glyphicon")),
                             justified = TRUE
                           ),
                           collapsible = TRUE
                       )
                ),
                
              ),
              
              
              fluidRow(
                column(width = 12,
                       box(
                         title = span(icon("chart-line"), " Análise temporal da contagem de filmes"),
                         width = NULL, status = "info", solidHeader = TRUE,
                         plotlyOutput("heatmap2", height = 600),
                         collapsible = TRUE, collapsed = FALSE,
                         absolutePanel(
                           dropdown(
                             uiOutput("info_text_heatmap1"),
                             style = "unite", icon = icon("circle-info"),
                             status = "primary", width = "900px",
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
                       box(title = span(icon("magnifying-glass"), " Selecione a classe de sua preferência"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           selectInput("variavel_y", "Classe", setNames(Vars_comp, Vars_comp_nomes), selected = "Certificate", selectize = TRUE),
                           numericInput("top_n", "Quantidade", value = 10, min = 5, max = 30, step = 5),
                           collapsible = TRUE
                       )
                ),
                column(width = 4,
                       box(title = span(icon("x"), " Selecione a variável e medida resumo de sua preferência"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           selectInput("variavel_x", "Variável Selecionada", setNames(Vars_temp, Vars_temp_nomes), selected = "Gross", selectize = TRUE),
                           selectInput("medida_resumo_1", "Medida Selecionada", setNames(Medidas_resumo, Medidas_resumo_nomes), selected = "sum", selectize = TRUE),
                           collapsible = TRUE
                       )
                ),
                column(width = 4,
                       box(title = span(icon("arrow-down-wide-short"), " Selecione a variável de sua preferência"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           radioGroupButtons(
                             inputId = "ascending",
                             choices = c("Maiores" = TRUE, 
                                         "Menores" = FALSE),                             checkIcon = list(
                               yes = icon("ok",
                                          lib = "glyphicon")),
                             justified = TRUE
                           ),
                           collapsible = TRUE
                       )
                ),

                
              ),
              
              fluidRow(
                column(width = 12,
                       box(
                         title = span(icon("chart-bar"), " Análise em função de variáveis categóricas"),
                         width = NULL, status = "info", solidHeader = TRUE,
                         fluidRow(
                           column(width = 6,
                                  plotlyOutput("countplot", height = 800)
                           ),
                           column(width = 6,
                                  plotlyOutput("barplot", height = 800)
                           )
                         ),
                         collapsible = TRUE, collapsed = FALSE,
                         absolutePanel(
                           dropdown(
                             uiOutput("info_text_countplot"),
                             style = "unite", icon = icon("circle-info"),
                             status = "primary", width = "900px",
                             tooltip = tooltipOptions(title = "Clique para ver mais informações sobre esse gráfico!")
                           ),
                           top = "6%", left = "1%", width = 300, zIndex = 1000
                         )
                       )
                )
              ),
              
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