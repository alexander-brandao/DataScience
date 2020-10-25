base = read_csv('retinopatia.csv')
print(base)

base$X1 = NULL

summary(base)

base$eye = factor(base$eye, levels = c('left', 'right'), labels = c(0,1))
base$risk = factor(base$risk, levels = c(6,8,9,10,11,12), labels = c(1,2,3,4,5,6))
base$trt = factor(base$trt, levels = c(0,1))
base$status = factor(base$status, levels = c(0,1))
base$id = factor(base$id, levels = unique(base$id))
base$laser = factor(base$laser, levels = unique(base$laser), labels = c(0,1))
base$type = factor(base$type, levels = unique(base$type), labels = c(0,1))

set.seed(1)
divisao = sample.split(base$status, SplitRatio = 0.70)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

classificador = naiveBayes(x= base_treinamento[-8], y = base_treinamento$status)
print(classificador)

previsoes = predict(classificador, newdata = base_teste[-8])
print(previsoes)

matriz_confusao = table(base_teste[, 8], previsoes) 
print(matriz_confusao)

confusionMatrix(matriz_confusao)

