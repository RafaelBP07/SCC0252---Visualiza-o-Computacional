# Funções que traduz as variaveis dos titulos para dos gráficos

#-----------------------------------------------------------------------------------------------

traducao <- c("Genre" = "Gêneros",
              "Star" = "Estrelas",
              "Certificate" = "Classificações",
              "Director" = "Diretores",
              "Gross" = "Faturamento",
              "IMDB_Rating" = "Avaliação IMDB",
              "Meta_score" = "Pontuação Meta",
              "No_of_Votes" = "Número de Votos",
              "Runtime" = "Duração")

# Função para substituir valores com base no mapeamento
tradutor <- function(variavel, traducao) {
  variavel_pt <- traducao[variavel]
  
  return(variavel_pt)
}