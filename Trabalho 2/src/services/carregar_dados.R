# Funções que transformam e filtram/selecionam os dados

#-----------------------------------------------------------------------------------------------

# Carregar dados especificados
carregar_dados <- function(tipo = "", date_slider = NULL) {
  df_filtered <- switch(tipo,
                        "sum" = df_sum,
                        "mean" = df_mean,
                        df)
  
  if (!is.null(date_slider)) {
    df_filtered <- df_filtered[df_filtered$Released_Year >= date_slider[1] & df_filtered$Released_Year <= date_slider[2], ]
  }
  
  return(df_filtered)
}