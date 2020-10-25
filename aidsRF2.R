#Alexander Costa Brandao
# RANDOM FOREST – sem pre-processamento
#a base de dados "aids" O conjunto de dados aqui registra os casos relatados de aids diagnosticados entre julho de 1983 e até o final de 1992.

#comando para carregar a base de dados
base = read.csv('aids.csv')

#comando para cancelar uma coluna irrelevante
base$X = NULL


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

#comando para instalar o pacote (random forest)
install.packages('randomForest')
library(randomForest)
set.seed(1)


#laço de repetiçao com 30 elementos
for (i in 1:30) {
  
  # O algoritmo random forest trabalha com 2 parametros x= base de treinamento e y= classe alvo
  
  # escolhemos a quantidades de 'arvores para o execuçao do algoritmo (no caso 30)
  
  #lembrando que o classificador e um modelo de aprendizado
  classificador = randomForest(x= base_treinamento[-6], y= base_treinamento$y, ntree = i)
  
  previsoes = predict(classificador, newdata = base_teste[-6], type = 'class')
  matriz_confusao = table(base_teste[, 6], previsoes )
  print(matriz_confusao)
  library(caret)
  
  print(confusionMatrix(matriz_confusao))
  conf_Marix <- confusionMatrix(matriz_confusao)
  print(conf_Marix)
  
}