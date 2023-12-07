# Carregar Pacotes
pacman::p_load(shiny, 
               shinydashboard)

#------------------------------------------------------------

# Carregar Funções
source("./src/services/index.R")
source("./src/server/graficos/index.R")

#------------------------------------------------------------

# Definir o servidor
server <- function(input, output, session) {
  
  print('INICIANDO')
  #================================================================== GRÁFICOS
    
  output$pairplot <- renderPlotly({
    render_pairplot(input)
  })
  
  output$boxplot <- renderPlotly({
    render_boxplot(input)
  })
  
  output$barplot <- renderPlotly({
    render_barplot(input)
  })
  
  output$lineplot <- renderPlotly({
    render_lineplot(input)
  })
  
  output$heatmap1 <- renderPlotly({
    render_heatmap(input)
  })
  
  output$heatmap2 <- renderPlotly({
    render_heatmap(input, TRUE)
  })
  
  output$countplot <- renderPlotly({
    render_countplot(input)
  })
  
  #================================================================== END: GRÁFICOS

  #================================================================== TEXTOS
  output$info_text_pairplot1 <- renderUI({
    tagList(
      tags$h4(tags$b("Contexto e Justificativa")),
      tags$p("Ao imergir na vasta base de dados do IMDB, repleta de informações sobre filmes, buscamos compreender não apenas características individuais, mas também explorar as complexas relações entre diferentes variáveis. Nosso objetivo é obter insights abrangentes sobre como fatores específicos interagem e influenciam o desempenho e a recepção de filmes. Optamos por utilizar o gráfico Pairplot em conjunto com Boxplots, uma escolha que nos permitirá examinar simultaneamente seis variáveis específicas - orçamento, bilheteria e classificação - selecionadas com base na sua relevância para a indústria cinematográfica. Essa abordagem oferece uma visão detalhada das interações entre essas variáveis, destacando padrões visuais, identificando outliers e proporcionando uma compreensão mais profunda das relações complexas que moldam o sucesso de um filme."),
      tags$h4(tags$b("Interpretação do Gráfico")),
      tags$p("No âmbito desse gráfico, destacam-se correlações significativas entre o faturamento e tanto o número de votos quanto a avaliação no IMDB. No que diz respeito às distribuições, com exceção do ano, observamos uma tendência de dispersões pronunciadas e presença marcante de outliers em todas as variáveis. Esses padrões visuais apontam para relações substanciais entre o desempenho financeiro dos filmes, a participação do público e as avaliações, embora as distribuições mostrem uma notável variabilidade, indicando a diversidade de casos e possíveis influências externas ao longo do tempo."),
      tags$h4(tags$b("Insights e Observações")),
      tags$p("Ao examinar o gráfico que ilustra correlações entre faturamento, número de votos e avaliação do IMDB, destacam-se insights significativos. A correlação positiva entre faturamento e número de votos sugere que filmes populares tendem a acumular mais avaliações. Além disso, a relação positiva entre faturamento e rating do IMDB indica que filmes financeiramente bem-sucedidos frequentemente recebem melhores avaliações. Contudo, a presença de dispersões pronunciadas e outliers em todas as variáveis aponta para uma notável variabilidade na indústria cinematográfica, destacando a complexidade do cenário. A influência do ano como variável discrepante sugere uma evolução ao longo do tempo, apontando para a necessidade de análises mais profundas para compreender os fatores impulsionadores do sucesso cinematográfico."),
      tags$h4(tags$b("Limitações e Considerações Adicionais")),
      tags$p("No entanto, é crucial reconhecer as limitações e considerações adicionais ao interpretar os resultados desses gráficos. A correlação identificada não implica causalidade, e fatores externos não representados na análise podem influenciar os resultados. Além disso, a presença de outliers pode distorcer as tendências gerais, exigindo uma abordagem mais detalhada na identificação e compreensão desses casos excepcionais. A variabilidade temporal pode ser mais complexa do que sugere a análise, considerando a evolução das tendências ao longo dos anos. Portanto, ao utilizar essas ferramentas, é fundamental adotar uma abordagem cautelosa, incorporando uma compreensão mais holística das variáveis e contextos específicos da indústria cinematográfica.")
    )
  })
  
  output$info_text_lineplot1 <- renderUI({
    tagList(
      tags$h4(tags$b("Contexto e Justificativa")),
      tags$p("Ao empregar gráficos de linha e heatmap em função do tempo na análise de dados do IMDB, buscamos desvendar padrões temporais que possam influenciar o desempenho dos filmes. A escolha dessas visualizações se justifica pela capacidade de capturar tendências ao longo do tempo e identificar correlações entre variáveis-chave, como faturamento, avaliações e participação do público."),
      tags$h4(tags$b("Interpretação do Gráfico")),
      tags$p("O gráfico de linha temporal revela um padrão consistente de aumento em todas as variáveis ao longo do tempo, indicando um crescimento geral em fatores como faturamento, avaliações e participação do público. Contudo, destaca-se uma queda considerável em 2020, o que pode ser diretamente atribuído à pandemia de Covid-19. Essa anomalia temporal sugere uma interrupção significativa nos padrões habituais da indústria cinematográfica durante esse período, evidenciando o impacto direto da pandemia nos resultados."),
      tags$h4(tags$b("Insights e Observações")),
      tags$p("Ao examinar essa queda em 2020, podemos inferir insights importantes sobre as ramificações da pandemia na indústria cinematográfica. A diminuição acentuada em todas as variáveis sugere não apenas uma redução nas receitas, mas também uma possível mudança no comportamento do público e nas estratégias de lançamento de filmes. Essa observação ressalta a necessidade de considerar eventos externos ao interpretar padrões temporais, oferecendo uma perspectiva valiosa para ajustes estratégicos e adaptações futuras na indústria do entretenimento."),
      tags$h4(tags$b("Limitações e Considerações Adicionais")),
      tags$p("Entretanto, é importante reconhecer as limitações. Eventos externos não refletidos nos dados podem influenciar drasticamente os resultados. Além disso, a interpretação de correlações não implica causalidade, exigindo uma análise cuidadosa. A granularidade temporal escolhida pode impactar as conclusões, sendo essencial considerar intervalos que capturem eventos relevantes. Portanto, ao explorar esses gráficos, é crucial adotar uma abordagem balanceada, considerando a complexidade do contexto temporal na indústria cinematográfica.")
    )
  })
  
  output$info_text_heatmap1 <- renderUI({
    tagList(
      tags$h4(tags$b("Contexto e Justificativa")),
      tags$p("A aplicação de um heatmap ao longo dos anos, com a contagem de gêneros de filmes, visa proporcionar uma análise abrangente das tendências de produção cinematográfica ao longo do tempo. A escolha dessa visualização é motivada pela busca por padrões nas preferências do público em relação aos gêneros cinematográficos. O heatmap, ao mapear visualmente a distribuição das contagens de gêneros ao longo dos anos, oferece insights cruciais sobre a evolução dos gostos do público e orienta decisões estratégicas na indústria cinematográfica."),
      tags$h4(tags$b("Interpretação do Gráfico")),
      tags$p("A análise mais aprofundada do heatmap revela que o gênero de filme que se destaca predominantemente é o drama, com uma frequência que aumenta consistentemente ao longo dos anos. Este padrão sugere uma preferência duradoura do público por filmes desse gênero específico. A concentração contínua de contagens de dramas destaca a relevância persistente desse tipo de produção ao longo do tempo na indústria cinematográfica."),
      tags$h4(tags$b("Insights e Observações")),
      tags$p("Ao identificar o drama como o gênero de destaque, podemos extrair insights valiosos sobre as preferências de longo prazo do público. O aumento contínuo na frequência de dramas pode orientar estratégias de produção, marketing e distribuição, indicando uma demanda constante por esse tipo de conteúdo. Além disso, ao observar que os tipos de filmes mais frequentemente feitos são voltados para todos os públicos e adultos em geral, podemos inferir tendências mais amplas da indústria cinematográfica, alinhando a produção com as expectativas do mercado."),
      tags$h4(tags$b("Limitações e Considerações Adicionais")),
      tags$p("Entretanto, é fundamental reconhecer que a contagem de gêneros por si só não reflete a qualidade ou aceitação crítica dos filmes. Além disso, eventos externos e mudanças culturais podem influenciar significativamente as preferências de gênero, exigindo uma análise contextual. A escolha da granularidade temporal é vital, pois certos eventos podem impactar a popularidade de determinados gêneros em curtos períodos. Dessa forma, ao interpretar o heatmap, é essencial adotar uma abordagem equilibrada, considerando a complexidade das preferências do público em relação aos gêneros cinematográficos ao longo do tempo.")
    )
  })
  
  output$info_text_countplot <- renderUI({
    tagList(
      tags$h4(tags$b("Contexto e Justificativa")),
      tags$p("XXX"),
      tags$h4(tags$b("Interpretação do Gráfico")),
      tags$p("XXX"),
      tags$h4(tags$b("Insights e Observações")),
      tags$p("XXX"),
      tags$h4(tags$b("Limitações e Considerações Adicionais")),
      tags$p("XXX")
    )
  })
  
  #================================================================== END: TEXTOS
  
  #================================================================== TEXTO SOBRE
  output$texto_sobre <- renderUI({
    tagList(
      tags$p(tags$b("Origem dos Dados:")),
      tags$p("Os dados utilizados são provenientes do",
             tags$a(href = 'https://www.kaggle.com/datasets/harshitshankhdhar/imdb-dataset-of-top-1000-movies-and-tv-shows', target = "_blank", "Kaggle"), 
             "e passaram por um preprocessamento relacionado ao",
             tags$a(href = 'https://github.com/RafaelBP07/SCC0252-Visualizacao-Computacional/tree/main/Trabalho%201', target = "_blank", "Trabalho 1"), 
             "desta mesma disciplina."),
      
      tags$p(tags$b("Navegação no Dashboard:")),
      tags$p("Explore o dashboard para obter insights detalhados sobre os top 1000 filmes no IMDb. Cada aba oferece funcionalidades específicas para análise:"),
      tags$p(tags$b("Aba 1: Visão Geral")),
      tags$ul(
        tags$li("Explore a correlação e dispersão das variáveis-chave."),
        tags$li("Analise padrões e relações entre as variáveis com um Pairplot dinâmico.")
      ),
      tags$p(tags$b("Aba 2: Análise Temporal")),
      tags$ul(
        tags$li("Observe a evolução do número de filmes ao longo do tempo, destacando tendências significativas."),
        tags$li("Analise como medidas resumo (soma e média) para variáveis como Faturamento, Avaliação IMDb, Pontuação Meta, Número de Votos e Duração evoluíram ao longo dos anos.")
      ),
      tags$p(tags$b("Aba 3: Comparação de Medidas")),
      tags$ul(
        tags$li("Compare diferentes medidas, como certificações, atores e diretores."),
        tags$li("Identifique insights valiosos ao comparar as medidas entre diferentes grupos.")
      ),
      tags$p("Clique nas abas para explorar e analisar os dados interativamente."),
      
      
      tags$p(tags$b("Código no GitHub:")),
      tags$p("O código-fonte deste projeto está disponível no",
             tags$a(href = 'https://github.com/RafaelBP07/SCC0252-Visualizacao-Computacional/tree/main', target = "_blank", "GitHub")),
      
      
      tags$p(tags$b("Significado das variáveis:")),
      tags$ol(
        tags$li("Poster_Link: Link do pôster usado pelo IMDb."),
        tags$li("Series_Title: Nome do filme."),
        tags$li("Released_Year: Ano de lançamento do filme."),
        tags$li("Certificate: Certificado/Classificação obtido pelo filme."),
        tags$li("Runtime: Duração total do filme."),
        tags$li("Genre: Gênero do filme."),
        tags$li("IMDB_Rating: Avaliação do filme no site IMDb."),
        tags$li("Overview: Resumo da história."),
        tags$li("Meta_score: Pontuação obtida pelo filme."),
        tags$li("Director: Nome do diretor."),
        tags$li("Star1, Star2, Star3, Star4: Nomes das estrelas do filme."),
        tags$li("No_of_votes: Número total de votos."),
        tags$li("Gross: Dinheiro arrecadado pelo filme.")
      ),
      
      tags$p(tags$b("Classes de Certificate:")),
      tags$ol(
        tags$li("A: \"Adulto\" - Geralmente indicando que o conteúdo é destinado apenas para adultos."),
        tags$li("UA: \"Adulto Sem Restrições\" - Adequado para todos os adultos, menos restritivo que \"A\"."),
        tags$li("U: \"Universal\" - Adequado para todas as audiências, incluindo crianças."),
        tags$li("PG-13: \"Orientação dos Pais 13\" - Material inadequado para menores de 13 anos."),
        tags$li("R: \"Restrito\" - Destinado a adultos ou espectadores com mais de uma certa idade."),
        tags$li("Unrated: Não oficialmente classificado por uma organização de classificação."),
        tags$li("PG: \"Orientação dos Pais\" - Material pode não ser adequado para crianças."),
        tags$li("G: \"Geral\" - Adequado para todas as audiências, incluindo crianças."),
        tags$li("Passed: Filme aprovado para todas as audiências."),
        tags$li("TV-14: Adequado para maiores de 14 anos."),
        tags$li("16: Indica que o conteúdo é adequado para espectadores com 16 anos ou mais."),
        tags$li("TV-MA: Adequado para adultos, pode conter linguagem forte ou violência gráfica."),
        tags$li("GP: \"Patronato Geral\" - Indicava que o filme era adequado para todas as audiências, com orientação dos pais."),
        tags$li("Approved: Filme aprovado para todas as audiências."),
        tags$li("TV-PG: Adequado para crianças mais velhas com orientação dos pais."),
        tags$li("U/A: \"Universal/Adulto\" - Adequado para todas as audiências com orientação dos pais.")
      )
    )
  })
  
  #================================================================== END: TEXTO SOBRE
  
  #================================================================== CSS
  # CSS para atualizar o layout
  output$style <- renderUI({
    tags$style(HTML(
      "
      /* Estilo para o slider*/
      .irs--flat .irs-bar {
      top: 25px;
      height: 12px;
      background-color: #00C0EF;
    }
    .irs--flat .irs-from, .irs--flat .irs-to, .irs--flat .irs-single {
      color: white;
      font-size: 10px;
      line-height: 1.333;
      text-shadow: none;
      padding: 1px 5px;
      background-color: #00C0EF;
      border-radius: 4px;
    }
    .irs--flat .irs-handle>i:first-child {
      position: absolute;
      display: block;
      top: 0;
      left: 50%;
      width: 2px;
      height: 100%;
      margin-left: -1px;
      background-color: #00C0EF;
    }
    .irs--flat .irs-from:before, .irs--flat .irs-to:before, .irs--flat .irs-single:before {
      position: absolute;
      display: block;
      content: '';
      bottom: -6px;
      left: 50%;
      width: 0;
      height: 0;
      margin-left: -3px;
      overflow: hidden;
      border: 3px solid transparent;
      border-top-color: #00C0EF; 
    }
    
    /* Estilo para a aba ativa no tabBox */
    .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #00C0EF;
    }
      "
    ))
  })
  #================================================================== END: CSS
  
}