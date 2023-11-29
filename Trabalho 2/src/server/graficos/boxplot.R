# Filtrar apenas colunas numéricas
numeric_columns <- sapply(df, is.numeric)
df_numeric <- df[, numeric_columns]

# Transformar dados para o formato longo
df_long <- df_numeric %>%
  pivot_longer(everything(), names_to = "Variavel", values_to = "Valor")

# Criar boxplot para cada variável numérica com escala própria
boxplot_plot <- ggplot(df_long, aes(x = "", y = Valor, fill = Variavel)) +
  geom_boxplot() +
  facet_wrap(~Variavel, scales = "free_y", ncol = length(unique(df_long$Variavel))) +
  labs(title = "Boxplot de Variáveis Numéricas",
       x = NULL,
       y = "Valor") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none")

# Converter para um gráfico interativo com plotly
boxplot_plot_interativo <- ggplotly(boxplot_plot)

# Exibir o gráfico interativo
print(boxplot_plot_interativo)