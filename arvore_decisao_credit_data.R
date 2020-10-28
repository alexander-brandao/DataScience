#comando para carregar a base de dados
base = read.csv('credit_data.csv')

#comando para cancelar uma coluna irrelevante
base$clientid = NULL

#comando para mostrar tabela com informacoes gerais
summary(base)

#comando para criar varialvel com valores negativos e N/A(not avaible)
(a = base[base$age < 0 & !is.na(base$age),  ])

#comando para calcular a media sem contar os N/A
mean(base$age[base$age > 0], na.rm = TRUE)

#comando para setar a media nos valores negativos
base$age = ifelse(base$age < 0, 40.92, base$age)

#comando para consultar os valores N/A e suas posicoes
base[is.na(base$age), ]

#comando para setar a media nos valores N/A
base$age = ifelse(is.na(base$age), mean(base$age,na.rm = TRUE), base$age)

#comando para escalonar todas as 3 primeiras colunas tirando a ultima que é o ultimo parametro de avaliacao no caso
base [ , 1:3] = scale(base[ , 1:3])

#comando para transformar a classe alvo(coluna default) em dados discretos
base$default = factor(base$default, levels = c(0,1))

#comando para instalar um pacote da biblioteca
install.packages('caTools')

library(caTools)

View(base)

#comando para setar dados aleatorios necessario para criar uma base de treinamento
set.seed(1)

#comando para dividir a base de dados com a finalidade de criar uma (base de treinamento) e uma (base de teste)
#coloca dentro de um objeto uma funcao chamada (sample split) que indica uma coluna alvo(no caso default) para dividir 
divisao = sample.split(base$default, SplitRatio = 0.75)

#comando para criar a variavel base_treinamento
base_treinamento = subset(base, divisao == TRUE)

#comando para criar a variavel base_teste
base_teste = subset(base, divisao == FALSE)


library(rpart)
classificador = rpart(formula = default ~ ., data = base_treinamento)

library(rpart.plot)
rpart.plot(classificador)

previsoes = predict(classificador, newdata = base_teste[-4], type = 'class')
matriz_confusao = table( base_teste[, 4], previsoes)

install.packages("caret")
install.packages("lattice")
install.packages("ggplot2")
library(caret)
library(lattice)
library(ggplot2)

confusionMatrix(matriz_confusao)
