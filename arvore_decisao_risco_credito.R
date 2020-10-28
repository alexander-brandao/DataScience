base = read.csv('risco_credito.csv')

# apaga a coluna clientID

#pacote com o algoritmo arvore de decisao
library(rpart)

#funcao rpart recebe atributos previsores e atributo alvo
classificador = rpart(formula = risco ~., data = base)
classificador = rpart(formula = risco ~ ., data = base, control = rpart.control(minbucket = 1))

print(classificador)

#comando para mostra grafico
plot(classificador)

#comando para inserir texto no grafico
text(classificador)

#pacode interessante para visualizar a arvore
install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(classificador)

#estudo de caso 
historia = c('boa', 'ruim')
divida = c('alta', 'alta')
garantias = c('nenhuma', 'adequada')
renda = c('acima_35', '0_15')
df = data.frame(historia, divida, garantias, renda)

previsoes= predict(classificador, newdata = df)
print(previsoes)
