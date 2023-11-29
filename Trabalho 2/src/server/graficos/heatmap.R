# Transformar os dados para ter uma linha por gênero e por ano
df_genre <- df %>%
  separate_rows(Genre, sep = ", ") %>%
  group_by(Released_Year, Genre) %>%
  summarize(count = n())

# Ordenar os gêneros pelo número total de filmes
genre_order <- df_genre %>%
  group_by(Genre) %>%
  summarize(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  pull(Genre)

df_genre$Genre <- factor(df_genre$Genre, levels = rev(genre_order))  # Inverter a ordem dos níveis

# Criar o gráfico de mapa de calor
heatmap_plot <- ggplot(df_genre, aes(x = Released_Year, y = Genre, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#2596be") +
  labs(title = "Mapa de Calor da Contagem de Filmes por Gênero ao Longo dos Anos",
       x = "Ano de Lançamento",
       y = "Gênero",
       fill = "Contagem") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Converter para plotly
heatmap_plotly <- ggplotly(heatmap_plot)

# Exibir o gráfico interativo
print(heatmap_plotly)