#' ---
#' title: Aula de laboratório - Estimação Pontual e Intervalar
#' author: Rafael Bressan
#' ---
#' 
#' # Introdução
#' 
#' Nesta aula vamos utilizar dados disponíveis do livro _Data Analysis for 
#' Business, Economics, and Policy_ (Békés e Kézdi, 2021). Os estudos de caso e
#' dados disponibilizados pelos autores encontram-se no site [https://gabors-data-analysis.com/](https://gabors-data-analysis.com/).
#' 
#' Como pano de fundo para nossa aula de laboratório utilizaremos a estudo de 
#' caso _"Comparing hotel prices in Europe: Vienna vs London"_.
#' 
#' # Estudo de Caso
#'
#' Como podemos comparar os mercados hoteleiros na Europa e conhecer as características 
#' dos preços dos hotéis? Podemos visualizar duas distribuições em um gráfico? 
#' Que estatísticas descritivas melhor descreveriam cada distribuição e suas 
#' diferenças? Podemos visualizar estatísticas descritivas?
#' 
#' Este estudo de caso usa o conjunto de dados da `hotels-europe` e seleciona 
#' hotéis de 3 a 4 estrelas em Viena e Londres para comparar a distribuição de 
#' preços para um dia da semana em novembro de 2017. Ele ilustra a comparação de 
#' distribuições e o uso de histogramas e gráficos de densidade. Ele ilustra o 
#' uso de algumas das estatísticas descritivas mais importantes para variáveis 
#' quantitativas e suas visualizações, box plots e violin plots. 
#' 
#' Além das comparações gráficas, utilizaremos testes formais de hipóteses para
#' comparar o preço médio de hospedagem em Viena e Londres.
#' 
#' ## Apresentando o conjunto de dados hotels-europe
#' 
#' Os dados da `hotels-europe` incluem informação sobre preços e características 
#' de hotéis em 46 cidades europeias e para 10 datas diferentes. N=148.021. 
#' Variáveis chave: preço do hotel, distância do hotel ao centro da cidade. 
#' Extraído de um site de comparação de preços. Foi anonimizado e ligeiramente 
#' alterado para garantir a confidencialidade. Ele contém muitas informações 
#' sobre o local e a classificação, mas não o nome ou o endereço.
#' 
#' # Script R
#' 
#' Novamente, começamos carregando as bibliotecas necessárias para nossa análise.
#' 
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(moments)
#' 
#' Vamos baixar os dados e salvá-los em um Data Frame. Este conjunto de dados é
#' na verdade **composto por duas tabelas** que possuem uma coluna em comum, 
#' também conhecida como chave primária na terminologia de banco de dados. É a partir
#' desta coluna (`hotel_id`) que iremos unir os dois data frames em um único. O 
#' comando `head()` nos fornecerá uma visualização rápida dos dados.
#' 
precos <- read.csv(url("https://osf.io/p6tyr/download/"))
head(precos)

hoteis <- read.csv(url("https://osf.io/utwjs/download/"))
head(hoteis)
#'
#' Precisamos fazer a união destes dados (merge ou join). Antes vamos filtar
#' as datas e as cidades de interesse, para somente depois fazer a união dos
#' dados e obter apenas um data frame para trabalhar.
#' 
precos_filtrado <- precos |> 
  filter(year == 2017, month == 11)

hoteis_filtrado <- hoteis |> 
  filter(city == "Vienna" | city == "London") |> 
  filter(stars >= 3 & stars <= 4)

# Inner Join: apenas os hoteis que estao em ambos data frames
df <- inner_join(precos_filtrado, hoteis_filtrado, by = "hotel_id")
head(df[, c("hotel_id", "city", "year", "month", "stars")])
# Apenas uma conferencia para ver se as filtragens foram corretas
sapply(df[, c("city", "year", "month", "stars")], unique)
sapply(df[, c("city", "year", "month", "stars")], 
       function(x) all(!is.na(x)))
#' 
#' Temos agora um data frame completo (`df`) com todos os dados necessários para
#' nossa análise. Começaremos com alguns típicos gráficos para analisar distribuição
#' de variáveis quantitativas.
#' 
#' ## Histogramas
#' 
#' 
histpreco <- ggplot(data = df, aes(x = price, group = city)) +
  geom_histogram(aes(y = stat(density)), binwidth = 20, 
                 fill = "darkred", color = "white") +
  labs(x = "Preco (US dolares)", y = "Densidade") +
  scale_x_continuous(limits = c(0, 500),
                     breaks = seq(0, 500, by = 50)) +
  facet_wrap(~city, ncol = 1) +
  theme_clean() 
histpreco
#' 
#' Suponha agora que nestes histogramas desejamos acrescentar uma linha vertical
#' indicando o preço médio das estadias. Esta é uma forma de ajudar na visualização
#' da diferença de um importante parâmetro. Precisamos primeiro calcular estas
#' médias por cidade e guardar em um data frame separado
#' 
precos_medios <- df |> 
  group_by(city) |> 
  summarise(media = mean(price))
precos_medios
#' 
#' E agora utilizamos este novo data frame para incluir as linhas verticais.
#' 
histpreco_media <- histpreco +
  geom_vline(aes(xintercept = media), data = precos_medios,
             linetype = 2)
histpreco_media
#' 
#' ## Boxplot
#' 
#' Outra forma de investigar graficamente as diferenças nas distribuições
#' de preços é através de boxplots, um para cada cidade.
#' 
boxpreco <- ggplot(data = df, aes(x = city, y = price)) +
  geom_boxplot(aes(color = city), show.legend = FALSE) +
  labs(y = "Preco (US dolares)", x = "Cidade") +
  theme_clean() 
boxpreco
#' 
#' ## Violino
#' 
#' Podemos aliar as informações de um boxplot ao formato de um gráfico de densidade
#' e obter o chamado gráfico de violino.
#' 
violinopreco <- ggplot(data = df, aes(x = city, y = price)) +
  geom_violin(aes(color = city), show.legend = FALSE,
              draw_quantiles = c(0.25, 0.5, 0.75)) +
  labs(y = "Preco (US dolares)", x = "Cidade") +
  coord_flip() +
  theme_clean() 
violinopreco
#' 
#' ## Estatísticas Descritivas
#' 
#' Percebemos claramente através de todos os gráficos apresentados que as 
#' distribuições de preços são diferentes entre as cidades. Valores mínimos, máximos,
#' média e de observações podem ser calculados para cada uma das cidades e tabulados.
#' Além destes, vamos computar também alguns outros momentos da distribuição:
#' variância, assimetria e curtose.
#' 
descritivas <- df |> 
  group_by(city) |> 
  summarise(media = mean(price), 
            min = min(price),
            max = max(price), 
            n = n(),
            var = var(price),
            assimetria = skewness(price),
            curtose = kurtosis(price))
descritivas
#' 
#' ## Teste de Hipótese Paramétrico
#' 
#' Por ora vamos testar os preços médios de estadia nas cidades de forma individual
#' contra valores fixos. Iremos fazer um **teste bilateral** em Viena, verificando
#' se o preço médio é diferente de US$ 150 enquanto que para Londres faremos um
#' teste unilateral, $H_0: \mu < 200$.
#' 
#' Temos uma amostra de dados onde podemos estimar a média, recaímos no Caso I da
#' aula. Este é o famoso caso do **teste t** para apenas uma amostra. No `R` 
#' podemos fazer o teste t através da função `t.test()`.
#' 
#' Poderíamos criar dois vetores de preços, um para cada cidade, e então aplicar
#' o teste adequado a cada um. O que vamos ver abaixo é uma aplicação de uma 
#' **função definida pelo usuário** para, nesta mesma função realizar os dois
#' testes a depender da cidade.
#' 
meus_testes <- function(price, city) {
  htest <- switch(city,
    "Vienna" = t.test(price, mu = 150),
    "London" = t.test(price, alternative = "less", mu = 200)
    )
  
  return(list(htest))
}
# Cria uma lista nomeada com a cidade
split_lista <- split(df$price, df$city)
# Aplica a função meus_testes para cada elemento da lista e do vetor com os 
# nomes das cidades
hteste_lista <- mapply(meus_testes, split_lista, names(split_lista))
# Apresenta os resultados em forma de tabela
lapply(hteste_lista, broom::tidy)
#' 
#' 