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
Vars <- names(df_sum)
Vars_nomes <- names(df_sum)

#------------------------------------------------------------

# Definir o UI
ui <- dashboardPage(
  dashboardHeader(title = span(icon("film"), "Top 1000 Filmes")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Página 1", tabName = "pag1", icon = icon("chart-line")),
      menuItem("Página 2", tabName = "pag2", icon = icon("chart-line")),
      menuItem("Página 3", tabName = "pag3",icon = icon("chart-line")),
      menuItem("Página 4", tabName = "pag4",icon = icon("chart-line")),
      menuItem("Página 5", tabName = "pag5",icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    uiOutput("style"),
    chooseSliderSkin("Flat"),
    tabItems(
      tabItem(tabName = "pag1",
              fluidRow(
                column(width = 4,
                       box(title = span(icon("x"), " Selecione a variável de sua preferência"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           selectInput("variavel_x", "Variável Eixo X", setNames(Vars, Vars_nomes), selected = "Released_Year", selectize = TRUE),
                           selectInput("variavel_y", "Variável Eixo Y", setNames(Vars, Vars_nomes), selected = "Gross", selectize = TRUE),
                           collapsible = TRUE
                       )
                ),
                
                column(
                  width = 4,
                  box(title = span(icon("calendar"), " Selecione o período de sua preferência"),
                      width = NULL, status = "info", solidHeader = TRUE,
                      # sliderInput(),
                      collapsible = TRUE
                  )
                )
                
              ),
              
              
              fluidRow(
                column(width = 12,
                       box(title = span(icon("chart-line"), " Gráfico Exemplo"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           plotlyOutput("grafico_1", height = 500),
                           collapsible = TRUE, collapsed = FALSE
                       )
                )
              ),
              
              
              fluidRow(
                column(width = 12,
                       box(title = span(icon("chart-line"), " XXXXX"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           fluidRow(column(width = 6,
                                           plotlyOutput("grafico_2", height = 500)),
                                    column(width = 6,
                                           plotlyOutput("grafico_3", height = 500))),
                           fluidRow(column(width = 6,
                                           plotlyOutput("grafico_4", height = 500)),
                                    column(width = 6,
                                           plotlyOutput("grafico_5", height = 500))),
                           collapsible = TRUE, collapsed = TRUE
                       )
                )
              )
      ),
      
      
      
      tabItem("pag2",
              h2("Conteúdo da Página 2"),
          
      ),
      
      tabItem("pag3",
              h2("Conteúdo da Página 3"),
              
      ),
      
      tabItem("pag4",
              h2("Conteúdo da Página 4"),

      ),
      
      tabItem("pag5",
              h2("Conteúdo da Página 5"),

      )
    )
  )
)