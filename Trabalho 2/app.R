# Carregar Pacotes
if(!require(pacman)) install.packages("pacman")
pacman::p_load(shiny,
               GGally,
               shinydashboard, 
               shinyWidgets, 
               shinythemes, 
               fpp3, 
               plotly, 
               forecast, 
               stringr, 
               drc,
               ggpubr,
               padr)

#------------------------------------------------------------

# Definir o UI
source("./src/ui/ui.R")

#------------------------------------------------------------

# Definir o servidor
source("./src/server/server.R")

#------------------------------------------------------------

# Criar o aplicativo Shiny
shinyApp(ui, server)