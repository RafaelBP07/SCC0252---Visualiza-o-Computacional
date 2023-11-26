# Carregar Pacotes
pacman::p_load(shiny, 
               shinydashboard, 
               shinyWidgets, 
               plotly)

#------------------------------------------------------------

# Carregar dados
df <- read.csv('https://raw.githubusercontent.com/RafaelBP07/SCC0252-Visualizacao-Computacional/main/Trabalho%201/pre_processed_data.csv')

# # Opção não removendo colunas com texto
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

#------------------------------------------------------------

# Input filtros

# Aba Análise temporal
Vars_temp <- names(df_sum)
Vars_temp <- setdiff(Vars_temp, "Released_Year")
Vars_temp_nomes <- Vars_temp

# Aba Comparação
Vars_comp <- names(df)
Vars_comp <- setdiff(Vars_comp, c(Vars_temp, "Released_Year", "Series_Title"))
Vars_comp_nomes <- Vars_comp


#------------------------------------------------------------

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
  dashboardBody(
    uiOutput("style"),
    chooseSliderSkin("Flat"),
    tabItems(
      tabItem("pag1",
              fluidRow(
                column(width = 12,
                       box(title = span(icon("chart-line"), " Pairplot"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           fluidRow(column(width = 6,
                                           # plotlyOutput("pairplot", height = 500)
                                           )),
                           collapsible = TRUE, collapsed = FALSE
                       )
                )
              )
              
      ),
      
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
                )
                
              ),
              
              
              fluidRow(
                column(width = 12,
                       box(
                         title = span(icon("chart-line"), " Gráfico Exemplo"),
                         width = NULL, status = "info", solidHeader = TRUE,
                         plotlyOutput("grafico_1", height = 500),
                         collapsible = TRUE, collapsed = FALSE,
                         absolutePanel(
                           dropdown(
                             uiOutput("texto_info1"),
                             style = "unite", icon = icon("circle-info"),
                             status = "primary", width = "300px",
                             tooltip = tooltipOptions(title = "Clique para ver mais informações sobre esse gráfico!")),
                           top = "8%", left = "1%", width = 300, zIndex = 1000 # left = 1% ou 95%
                         )
                       )
                )
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
                           collapsible = TRUE, collapsed = TRUE
                       )
                )
              )
      ),
      
      
      
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