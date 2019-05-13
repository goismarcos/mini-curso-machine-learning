#BAIXANDO O PACOTE PARA NAIVE BAYES
install.packages("e1071", dependencies=T)
#IMPORTANDO A BIBLIOTECA NAIVE BAYES
library(e1071)
#IMPORTANDO OS DADOS
bd_cancer = read.csv(file.choose(),sep=';',header=T)
#VISUALIZANDO OS DADOS IMPORTADOS
head(bd_cancer)
#OBTENDO OS MESMOS NÚMEROS ALEATÓRIOS
set.seed(1)
#SORTEANDO DOIS NÚMEROS COM REPOSIÇÃO (AMOSTRAGEM SIMPLES) ONDE CADA ELEMENTO DA AMOSTRA
#TEM A MESMA PROBABILIDADE SE SER ESCOLHIDO
amostra = sample(2,683,replace=T, prob=c(0.7,0.3))
#POSIÇÃO DE CADA AMOSTRA SENDO SETADA NOS DADOS, PARA SEPARAÇÃO ENTRE TREINO E TESTE
treino = bd_cancer[amostra==1,]
teste = bd_cancer[amostra==2,]
#VISUALIZANDO A DIMENÇÃO DE TREINO E TESTE
dim(treino)
dim(teste)
#CRIANDO UM MODELO NAIVE BAYES E TREINANDO
modelo = naiveBayes(classe ~ at1 + at2 + at3 + at4 + at5 + at6 + at7 + at8 + at9, treino)
#VISUALIZANDO OS DADOS 
modelo
#FAZENDO ENTÃO A PREVISÃO DOS DADOS DE TESTE
predicao = predict(modelo,teste)
#FAZENDO UM TABELA DE CONFUSÃO PARA VERIFICAR OS RESULTADOS DA PREVISÃO
confusao = table(teste$classe,predicao)
#VISUALIZANDO
confusao
#SEPARANDO ERROS E ACERTOS
taxaacerto = (confusao[1] + confusao[4]) / sum(confusao)
taxaerro = (confusao[2] + confusao[3]) / sum(confusao)
#VISUALIZANDO ERROS E ACERTOS
taxaacerto*100
taxaerro*100

#SIMULANDO UM TESTE EM PRODUÇÃO
at1 = 5
at2 = 4
at3 = 2
at4 = 3
at5 = 6
at6 = 3
at7 = 7
at8 = 8
at9 = 2
paciente = data.frame(at1,at2,at3,at4,at5,at6,at7,at8,at9)
resultado = predict(modelo,paciente)
#VISUALIZANDO RESULTADO
resultado