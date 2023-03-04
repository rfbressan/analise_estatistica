#' ---
#' title: Aula de laboratório - Estruturas básicas no R
#' author: Rafael Bressan
#' ---
#' 
#' 
#' Aula baseada no livro: [Introdução ao R](http://www.lampada.uerj.br/introducaoaor)
#' capítulo 3. É sugerida a leitura do livro para um maior aprofundamento na 
#' linguagem de programação.
#' 
#' ## Objetos no R
#' 
#' Tudo no R é um objeto. Para atribuirmos resultados de operações a algum 
#' objeto que pode posteriormente ser manipulado, usamos a expressão de 
#' atribuição “<-”:
y <- 4
y

x <- "ola"
x
#' 
#' Observações:
#'  
#' 1. Recomenda-se o uso de nomes intuitivos, descritivos.
#' 
#' 2. Nomes de objetos não podem começar com números ou underline
#' 
# 2sls <- 45
# _1st_stage <- 1234
#'
#' 3. Letras maiúsculas e minúsculas são diferentes para o interpretador do R
#' 
#' O R possui cinco classes básicas ou “atômicas” de objetos:
#' 
#' - character;
#' 
#' - numeric (números reais);
#' 
#' - integer (números inteiros);
#' 
#' - complex (números complexos);
#' 
#' - logical (Falso/Verdadeiro)
#' 
#' A função class(), aplicada a um objeto, retorna a sua classe.
class(x)

x <- 'ola'
class(x)

class(y)

y <- 2L
class(y)

z <- TRUE
class(z)

z <- z == y
class(z)

c <- 1 + 1i
class(c)
#'
#' ## Valores especiais
#' 
#' `NA`, `Inf`, `-Inf`, e `NaN`
#' 
1/0
-1/0
-Inf
0/0
NA
#' 
#' Podemos testar se uma expressão resulta em um valor especial
#' 
is.na(NA_real_)
is.infinite(1/0)
is.nan(0/0)
is.nan(NA_character_)
is.na(0/0)
#'
#' ## Vetores
#' 
#' São um conjunto unidimensional de valores de mesma classe
v <- c("arroz", "feijao")
v
class(v)

v <- c(v, 23) # append em um vetor
v
#'
#' Note que v transformou o numero 23 na string "23" e fez o append. Sem erro!
#' 
#' Vetores possuem tamanho (length)
length(v)
#'
#' Podemos usar também o operador `:` para criar sequências numéricas (que são
#' armazenadas como vetores)
#' 
z <- 10:30
z
class(z)
#'
#' Acessamos os elementos de um vetor através de seu índice
#' 
z[2]
#' O último elemento de um vetor possui índice igual ao tamanho do vetor
#' 
z[length(z)]
#' 
#' E podemos acessar um subconjunto do vetor através de uma sequência de índices
#' 
z[10:length(z)]
#' 
#' Índices negativos significam exclusão daquele elemento
#' 
z[-1] # retorna z com o primeiro elemento excluído
z[-c(1:5)] # retorna z sem os primeiros 5 elementos
#' 
#' Para selecionarmos elementos de um vetor, podemos utilizar também uma 
#' expressão lógica, com os operadores == (igual), > (maior), < (menor), 
#' >= (maior ou igual), <= (menor ou igual), != (diferente), ou combinações de 
#' expressões lógicas por meio dos operadores booleanos “OU” (|), “E” (&), 
#' e “Não” (!).
#' 
z[z == 15]
z[z < 15]
z[z %in% c(12, 15, 31)]
z[!(z %in% 15:25)]
#' 
#' 
#' ## Matrizes
#' 
#' Matrizes são vetores com um atributo de dimensão (dimension), que é ele 
#' próprio um vetor inteiro de tamanho 2: número de linhas e número de colunas.
#' Uma matriz é criada por meio da função matrix(). Essa função possui os 
#' seguintes argumentos:
#' 
#' - data: vetor que contém os valores da matriz;
#' 
#' - nrow: número de linhas da matriz;
#' 
#' - ncol: número de colunas da matriz;
#' 
#' - byrow: indica como a matriz é montada. Se for TRUE, ela é montada linha a 
#' linha. Se for FALSE, ela é montada coluna a coluna;
#' 
#' - dimnames: nomes das linhas e colunas, respectivamente.
#' 
m <- matrix(data = 1:6, 
            nrow = 2, 
            ncol = 3, 
            byrow = TRUE, 
            dimnames = list(c("trat1", "trat2"), c("out1", "out2", "out3")))
m
#' 
#' Para obtermos a dimensão de uma matriz, usamos a função `dim`.
dim(m)
#' 
#' Outra maneira de se criar matrizes é pela adição de linhas ou colunas por 
#' meio das funções rbind() e cbind(), respectivamente. rbind() monta uma 
#' matriz, empilhando os argumentos linha a linha na ordem em que aparecem na 
#' lista. Já cbind() monta a matriz colocando os argumentos lado a lado na 
#' ordem em que aparecem na lista.
#' 
#' 
x <- 1:4
y <- 10:13
rbind(x, y)
cbind(x, y)
#' 
#' Os elementos (células) de uma matriz podem ser acessados por um par de 
#' índices que indicam, respectivamente, a linha e a coluna do elemento desejado.
#' 
m[2, 3]
#' 
#' Para extrairmos todos os elementos de uma linha, basta não especificarmos o 
#' índice correspondente à coluna.
#' 
m[2, ]
m[, 2]
#' 
#' Note que a seleção de uma linha ou coluna retorna um vetor!
#' 
#' Índices negativos excluem os elementos indicados.
m[, -2] # matriz sem a segunda coluna
#' 
#' Podemos utilizar vetores de índices para selecionarmos uma submatriz.
#' 
m[, c(1, 3)]
#' 
#' Também podemos selecionar elementos de uma matriz através dos nomes das 
#' dimensões
#'  
m["trat1", "out3"]
#' 
#' ## Listas
#' 
#' Listas são coleções de elementos que não precisam ser da mesma classe. 
#' As listas são os objetos mais versáteis no R. Os elementos de uma lista 
#' podem ser de qualquer tipo, inclusive outras listas. O tipo
#' `list` é uma importante classe de objetos no R. Listas podem ser criadas 
#' explicitamente usando a função `list()`.
#' 
x <- list(5, "kiwi", c(FALSE, TRUE, TRUE), 3 + 1i, 2L, m)
x
class(x)
length(x)
#'
#' Podemos extrair uma sub-lista usando colchetes simples e índices. Colchetes
#' duplos são utilizados para extrair um elemento da lista. Neste caso o formato
#' de retorno será o do elemento.
#' 
x[6] # retorna uma lista com um elemento (a matriz)
x[3:6] # retorno uma lista com os elementos de 3 a 6
x[[2]] # retorna o character "kiwi"
#' 
#' Uma lista pode ser composta de elementos que são eles próprios uma lista ou 
#' vetores. Elementos de uma lista que pertence a outra lista são chamados 
#' elementos aninhados. Há duas maneiras de acessar um elemento aninhado de uma 
#' lista.
#'
x[[c(3, 1)]] # extrai o 1º elemento do vetor que é o 3º elemento da lista
x[[3]][1] # 3º elemento da lista, e então 1º elemento do vetor
#' 
#' Elementos de uma lista podem ser nomeados (assim como em vetores)
#' 
x <- list(
    num = 5,
    char = "kiwi",
    vec = c(FALSE, TRUE, TRUE),
    comp = 3 + 1i,
    int = 2L,
    mat = m
)
x
#' E agora podemos extrair estes elementos pelo nome
x$vec
x[["vec"]]
names(x)
#' 
#' ## Data Frames
#' 
#' Data frames são utilizados para armazenar dados tabulados no R. Eles são 
#' usualmente criados quando lemos um arquivo de dados para ser analisado. 
#' Internamente no R, os data frames são representados como uma lista especial 
#' onde cada elemento da lista possui o mesmo tamanho. Cada elemento da lista 
#' pode ser interpretado como uma coluna e o tamanho de cada elemento da lista 
#' é o número de linhas ou observações. Como se trata de uma lista, as colunas 
#' de um data frame não precisam ser da mesma classe.
#' 
#' Um data frame pode ser criado explicitamente por meio da função data.frame().
#' Entretanto existem extensão ao data frame do base R, como por exemplo os
#' tibbles (do pacote `tibble` que faz parte do tidyverse) e o `data.table`.
#' Faremos bastante uso deste último nas aulas práticas.
#' 
pacientes <- data.frame(
    id = c("P1","P2","P3","P4"), 
    sexo = c("feminino", "feminino", "masculino", "masculino"), 
    pad = c(80, 85, 100, 95), pas = c(130, 140, 150, 145)
)
pacientes
names(pacientes)
#' Cada coluna em um data.frame é nomeada e representa uma variável.
#' 
#' Os elementos de um data frame podem ser acessados como os elementos de uma 
#' lista ou de uma matriz. As variáveis podem ser identificadas pela sua 
#' posição no data frame.
#' 
pacientes$pad
pacientes[3, 2]
pacientes[, c(2, 4)]
pacientes[, c("sexo", "pas")]
pacientes[, -c(1, 3)]

cols <- c("sexo", "pas")
pacientes[, cols]

pacientes[pacientes$sexo == "feminino", ]
#' 
#' ## Funções
#' 
#' Novas funcionalidades são adicionadas ao ambiente por meio da implementação 
#' de funções agrupadas em módulos ou pacotes. As funções no R possuem um nome 
#' seguido de zero ou mais argumentos entre parênteses. Vejamos alguns exemplos 
#' de funções.
#' 
x <- c(1, 4, 6, 1, 10) # cria um vetor contendo os números 1, 4, 6, 1 e 10
sort(x, decreasing = TRUE) # ordena x em ordem decrescente
min(x) # menor valor de x
sum(x)
prod(x)
mean(x)
unique(x)
#'
#' Podemos também criar nossas próprias funções! Para criarmos uma função, 
#' usamos a palavra chave function seguida de uma lista de argumentos formais 
#' entre parênteses e separados por vírgulas. O código que é executado cada vez 
#' que a função é chamada aparece entre chaves.
#' 
twosam <- function(y1, y2) {
    n1  <- length(y1) 
    n2  <- length(y2)
    yb1 <- mean(y1)   
    yb2 <- mean(y2)
    s1  <- var(y1)    
    s2  <- var(y2)
    s <- ((n1 - 1)*s1 + (n2 - 1)*s2)/(n1 + n2 - 2)
    tst <- (yb1 - yb2)/sqrt(s*(1/n1 + 1/n2))
    
    return(tst)
}

a <- 1:10
b <- 3:12
twosam(a, b)
