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
#' caso _"Finding a Good Deal among Hotels"_.
#' 
#' # Estudo de Caso
#' 
#' ## Apresentando o conjunto de dados hotels-vienna
#' 
#' 
#' O objetivo final do nosso primeiro estudo de caso é usar dados de todos os 
#' hotéis de uma cidade para encontrar bons negócios: hotéis com preços baixos 
#' em relação à sua localização e qualidade. 
#' 
#' O conjunto de dados `hotels-vienna` contém informações sobre hotéis, albergues 
#' e outros tipos de acomodação em uma cidade, Viena, e uma noite durante a semana, 
#' em novembro de 2017. Para cada acomodação, os dados incluem informações sobre 
#' nome e endereço, preço da noite em foco, em dólares americanos (USD), 
#' classificação média do cliente de duas fontes mais o número correspondente 
#' dessas classificações, estrelas, distância até o centro da cidade e 
#' distância até a principal estação ferroviária. Os dados incluem $N = 428$ 
#' acomodações em Viena. Cada linha refere-se a uma acomodação separada. Todos os 
#' preços referem-se à noite do mesmo dia da semana em novembro de 2017.
#' 
#' # Script R
#' 
#' Começamos carregando as bibliotecas que serão necessárias para nossa análise.
#' Como esta aula destina-se a fundamentar os métodos da análise estatística,
#' vamos nos limitar o máximo possível a não utilizar pacotes com funções prontas
#' para fazer análise. Nosso intuito é demonstrar, com o auxílio do R, os cálculos
#' envolvidos na análise teórica mostrada em aula.
#' 
library(ggplot2)

#' Vamos baixar os dados e salvá-los em um Data Frame. O comando `head()` nos
#' fornecerá uma visualização rápida dos dados.
#' 
df <- read.csv(url("https://osf.io/y6jvb/download"))
head(df)
#'
#' Vimos em sala de aula que podemos fazer **estimativas pontuais e intervalares**,
#' começaremos com a primeira.
#' 
#' Desejamos estimar o valor médio do preço de uma acomodação em Vienna, uma noite
#' de dia de semana em novembro de 2017. Sabemos que o estimador da média populacional
#' $\mu$ é a média aritimética da amostra. Vamos calcular este valor.
#' 
#' ## Estimação Pontual
#' 
precos <- df$price # Retorna a coluna de preços na forma de vetor
x_barra <- sum(precos)/length(precos) # Media aritmetica
x_barra2 <- mean(precos) # Funcao built-in para media

x_barra
x_barra2
#' 
#' Supomos que queiramos estimar a média de outras variáveis também, `ratingta` e
#' `rating`. Não precisamos fazer uma a uma criando novos nomes no ambiente do R.
#' Podemos selecionar estas colunas do data frame e aplicar a função `mean` a 
#' todas elas.
#' 
medias_lst <- lapply(df[, c("price", "ratingta", "rating")], mean)
medias_lst
#' 
#' Ops. Existem dados faltantes para as variáveis de `rating`. A função `mean` 
#' possui como argumento `na.rm` que remove os dados faltantes para computar a 
#' média. (digite ?mean no console para acessar a ajuda da função)
#' 
medias_lst <- lapply(df[, c("price", "ratingta", "rating")], mean, na.rm = TRUE)
medias_lst
#' 
#' O comando `lapply` retorna uma lista. Como este nosso exemplo é simples, 
#' os três valores podem facilmente serem armazenados como vetor. Para tanto,
#' experimente o commando `sapply`. Você consegue perceber a diferença?
#' 
#' ## Estimação Intervalar
#' 
#' Vamos agora criar os intervalos de confiança. Primeiramente definimos o **nível
#' de significância** $\alpha=5\%$. Nossa amostra possui N = `r nrow(df)` observações, e
#' portanto, recai no Caso I de estimação. Utilizaremos valores críticos oriundos
#' da **tabela normal**. 
#' 
#' O próximo passo é ter uma estimativa do desvio padrão dos preços. Podemos 
#' fazer os cálculos manualmente através da fórmula $s^2=\frac{1}{n-1}\sum_{i=1}^n(x_i-\bar{x})^2$,
#' ou utilizar a função `sd`.
#' 
sd_precos <- sd(precos) # Manualmente seria sqrt(sum((precos - x_barra)^2)/427)
sd_precos
#' 
#' O valor crítico a ser retirado da tabela normal é obtido através da função
#' `qnorm` com o argumento 0.975, representando que desejamos o quantil 97,5%.
#' 
zc <- qnorm(0.975)
zc
#'
#' Agora temos todos os termos necessários para calcular o intervalo de confiança
#' da média dos preços de hospedagem. A fórmula para o intervalo de confiança é:
#' 
#' $$\bar{x}\pm z_{\alpha/2}\frac{s}{\sqrt{n}}$$
#' 
ic <- c(lim_inf = x_barra - zc*sd_precos/sqrt(length(precos)),
        lim_sup = x_barra + zc*sd_precos/sqrt(length(precos)))
ic
#' 
#' O `R` (o pacote `stats` que é carregado por default) disponibiliza uma função 
#' para fazer **teste t-Student** em uma ou duas amostras. O resultado do teste, 
#' dado um nível de confiança exigido, retorna a média e o intervalo. Como a 
#' distribuição t-Student se aproxima da Normal para grandes amostras, podemos
#' utilizar esta função para obter os valores todos de uma só vez, basta ter o 
#' vetor de preços.
#' 
t.test(precos, conf.level = 0.95)
#' 
#' # Referências
#' 
#' BÉKÉS, Gábor; KÉZDI, Gábor. Data analysis for business, economics, and 
#' policy. Cambridge University Press, 2021.