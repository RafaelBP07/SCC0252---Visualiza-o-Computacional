# Funções que transformam e filtram/selecionam os dados

#-----------------------------------------------------------------------------------------------

# Carregar dados especificados
carregar_dados <- function(tipo = "") {
  switch(tipo,
         "sum" = df_sum,
         "mean" = df_mean,
         df)
}