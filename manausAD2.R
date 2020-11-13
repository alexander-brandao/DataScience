#Alexander Costa Brandao
# Arvore de Decisao – inconsistentes + faltantes 

#a base de dados "manaus" mede a altura do rio negro em manaus ao longo do tempo. Os dados cobrem 90 anos, de janeiro de 1903 a dezembro de 1992.

#comando para carregar a base de dados
base = read.csv('manaus.csv')
#comando para cancelar uma coluna irrelevante
base$X = NULL

#comando para criar varialvel com valores negativos e N/A(not avaible)
(a = base[base$value < 0 & !is.na(base$value),  ])

#comando para calcular a media sem contar os N/A
mean(base$value[base$value > 0], na.rm = TRUE)

#comando para setar a media nos valores negativos
base$value = ifelse(base$value < 0, 1.094544, base$value)

#comando para setar a media nos valores N/A
base$value = ifelse(is.na(base$value), mean(base$value,na.rm = TRUE), base$value
                    
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
#coloca dentro de um objeto uma funcao chamada (sample split) que indica uma coluna alvo(no caso value) para dividir 
divisao = sample.split(base$value, SplitRatio = 0.75)
                    
#comando para criar a variavel base_treinamento
base_treinamento = subset(base, divisao == TRUE)
                    
#comando para criar a variavel base_teste
base_teste = subset(base, divisao == FALSE)
                    
library(rpart)
classificador = rpart(formula = value ~ ., data = base_treinamento)
                    
library(rpart.plot)
rpart.plot(classificador)
                    
previsoes = predict(classificador, newdata = base_teste[-2], type = 'class')
matriz_confusao = table( base_teste[, 2], previsoes)
                    
install.packages("caret")
install.packages("lattice")
install.packages("ggplot2")
library(caret)
library(lattice)
library(ggplot2)
                    
confusionMatrix(matriz_confusao)