#Alexander Costa Brandao
# NAIVE BAYS – inconsistentes + faltantes + escalonamento

#a base de dados "aids" O conjunto de dados aqui registra os casos relatados de aids diagnosticados entre julho de 1983 e até o final de 1992.

#comando para carregar a base de dados
base = read.csv('aids.csv')

#comando para cancelar uma coluna irrelevante
base$X = NULL

#a base de dados 'aids' nao tem dados faltantes nem dados negativos 

#comando para escalonar todas as 5 primeiras colunas tirando a ultima que é o ultimo parametro de avaliacao no caso
base [ , 1:5] = as.numeric(scale(base[ , 1:5]))

#comando para instalar pacote "tidyverse'
install.packages("tidyverse")

library(tidyverse)

#comando para instalar um pacote da biblioteca
install.packages('caTools')

library(caTools)

View(base)

#comando para setar dados aleatorios necessario para criar uma base de treinamento
set.seed(1)

#comando para dividir a base de dados com a finalidade de criar uma (base de treinamento) e uma (base de teste)
#coloca dentro de um objeto uma funcao chamada (sample split) que indica uma coluna alvo(no caso y) para dividir 
divisao = sample.split(base$y, SplitRatio = 0.75)

#comando para criar a variavel base_treinamento
base_treinamento = subset(base, divisao == TRUE)

#comando para criar a variavel base_teste
base_teste = subset(base, divisao == FALSE)

#chamando a biblioteca do algoritmo Neive Bayes
install.packages('e1071')
library(e1071)

#criando o modelo de treinamento 
classificador = naiveBayes(x = base_treinamento[-6], y = base_treinamento$y)
print(classificador)

#criando o modelo de previsao
previsoes = predict(classificador, newdata = base_teste[-6])

matriz_confusao = table(base_teste[, 6], previsoes)
print(matriz_confusao)


install.packages('caret')
library(caret)

confusionMatrix(matriz_confusao)
plot(matriz_confusao)
