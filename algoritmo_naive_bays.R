#comando para puxar arquivo .csv e jogar no objeto 'base
base = read.csv('risco_credito.csv')

#importando o pacote 'e1071' para utilizar o algoritmo Naive Bayes
install.packages('e1071')
library(e1071)

#vamos criar um objeto chamado 'classificador' que representa a tabela de probabilidades do algoritmo 
#O algoritmo Naive Bayes recebe 2 paremetros 'x' = uma tabela ou matrix e 'y' = um vetor (parametro alvo)  
# 'base[-5]' significa que ele vai usar todos os atributos menos a coluna 5
classificador = naiveBayes(x= base[-5], y = base$risco)

#comando para mostrar o objeto
print(classificador)

#criando um data frame(df)
#o comando c permite a criancao de um vetor
historia = c('ruim')
divida = c('alta')
garantias = c('adequada')
renda = c('0_15')
df = data.frame(historia, divida, garantias, renda)

#comando para fazer a previsao
previsao = predict(classificador, newdata = df, 'raw')
print(previsao)
