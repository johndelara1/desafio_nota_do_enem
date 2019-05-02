# Machine Learning - Regressão 
# Prevendo Notas de Matemática dos Participantes do ENEM

# Configurando o diretório de trabalho
setwd("C:/Users/Matilde/Dropbox/DESAFIO-Codenation(ENEM)/desafio_nota_do_enem")
getwd()

library(dplyr)        # Filtragens
library(psych)        # Scatterplot Matrix
library(e1071)        # SVM
library(rpart)        # decision tree
require(randomForest) # RANDON FOREST
library(readr)        # GERAR CSV
library(tidyverse)    # Drop_NA()
library(readxl)       # Importar xls
#library(catboost)    # Algoritmo de boost
#library(devtools)    # Github
#devtools::install_   github('catboost/catboost', subdir = 'catboost/R-package')

# ****************************************************
# ***                   DADOS                      ***
# ***                                              ***
# ****************************************************
# Etapa 1 - Coletando os dados
nota <- read.csv("train.csv")
notateste <- read.csv("test.csv")
PIB_2016 <- read_excel("PIB-2016.xls")
TP_DEPENDENCIA_ADM_ESC

# ****************************************************
# ***                   INCLUIR                    ***
# ***                NOVAS VARIÁVEIS               ***
# ****************************************************
#média do pib por estado
PIB_2016 = PIB_2016 %>% filter(Ano == 2016)
PIB_2016 = PIB_2016[c(names(PIB_2016)[5], names(PIB_2016)[42])]
SOMA_PIB_2016 = aggregate(PIB_2016$`Produto Interno Bruto per capita
(R$ 1,00)`, 
                           by=list(PIB_2016$`Sigla da Unidade da Federação`),
                           FUN = sum)
colnames(SOMA_PIB_2016) = c("SG_UF_RESIDENCIA","media")
lookup <- c( AC = "13.751",  AL = "49.456",  AP = "14.339",  AM = "89.017 ",  BA = "258.649", CE = "138.379", DF = "235.497", ES = "109.227", GO = "181.692", MA = "85.286", MT = "123.834", MS = "91.866", MG = "544.634", PR = "401.662", PB = "59.089", PA = "138.068", PE = "167.290", PI = "41.406", RJ = "640.186", RN = "59.661", RS = "408.645", RO = "39.451", RR = "11.011", SC = "256.661", SE = "38.867", SP = "2.038.005", TO = "31.576")
SOMA_PIB_2016$media = lookup

# Criar regioes com os estados por prova
glimpse(notateste$SG_UF_RESIDENCIA)
table(notateste$SG_UF_RESIDENCIA)
x <- c(notateste$SG_UF_RESIDENCIA)
lookup <- c( AC = "NORTE",  AL = "NORDESTE",  AM = "NORTE",  AP = "NORTE",  BA = "NORDESTE", CE = "NORDESTE", DF = "CENTRO-OESTE", ES = "SUDESTE", GO = "CENTRO-OESTE", MA = "NORDESTE", MG = "SUDESTE", MS = "CENTRO-OESTE", MT = "CENTRO-OESTE", PA = "NORTE", PB = "NORDESTE", PE = "NORDESTE", PI = "NORDESTE", PR = "SUL", RJ = "SUDESTE", RN = "NORDESTE", RO = "NORTE", RR = "NORTE", RS = "SUL", SC = "SUL", SE = "NORDESTE", SP = "SUDESTE", TO = "NORTE")
uniao = lookup[x]
regioes = unname(uniao)
table(regioes)
notateste$regioes = regioes
table(notateste$regioes)
names(table(notateste$regioes))

#Transformar os dois datasets com as mesmas variáveis
notateste = merge(notateste,SOMA_PIB_2016)
nomes <- names(notateste)



# Criar regioes com os estados por prova
glimpse(nota$SG_UF_RESIDENCIA)
table(nota$SG_UF_RESIDENCIA)
x <- c(nota$SG_UF_RESIDENCIA)
lookup <- c( AC = "NORTE",  AL = "NORDESTE",  AM = "NORTE",  AP = "NORTE",  BA = "NORDESTE", CE = "NORDESTE", DF = "CENTRO-OESTE", ES = "SUDESTE", GO = "CENTRO-OESTE", MA = "NORDESTE", MG = "SUDESTE", MS = "CENTRO-OESTE", MT = "CENTRO-OESTE", PA = "NORTE", PB = "NORDESTE", PE = "NORDESTE", PI = "NORDESTE", PR = "SUL", RJ = "SUDESTE", RN = "NORDESTE", RO = "NORTE", RR = "NORTE", RS = "SUL", SC = "SUL", SE = "NORDESTE", SP = "SUDESTE", TO = "NORTE")
uniao = lookup[x]
regioes = unname(uniao)
table(regioes)
nota$regioes = regioes
table(nota$regioes)
names(table(nota$regioes))

#Transformar os dois datasets com as mesmas variáveis
nota = merge(nota,SOMA_PIB_2016)


# ****************************************************
# ***                   TREINO                     ***
# ***                                              ***
# ****************************************************
# Segmentando apenas os dados que temos no teste
treinando = nota[c(nomes, "NU_NOTA_MT")]
treinando$Q027 = as.numeric(treinando$Q027)
treinando$Q027[(treinando$Q027 == 1)] <- mean(treinando$Q027)
glimpse(treinando)
#summary(treinando$TP_DEPENDENCIA_ADM_ESC)
#treinando$TP_DEPENDENCIA_ADM_ESC[(is.na(treinando$TP_DEPENDENCIA_ADM_ESC))] <- 2.26
treinando$TP_ENSINO = NULL
treinando$TP_DEPENDENCIA_ADM_ESC = NULL
#Para que eu possa pegar exatamente os nomes das variáveis mais a target
treinando = treinando %>% drop_na()
#treinando$Q027 = NULL
#treinando$TP_DEPENDENCIA_ADM_ESC = NULL

any(is.na(treinando))
# ****************************************************
# ***              FORMATO NUMÉRICO                ***
# ****************************************************
glimpse(treinando)
#Transformar para numérico
treinando$SG_UF_RESIDENCIA = as.numeric(treinando$SG_UF_RESIDENCIA)
treinando$TP_SEXO = as.numeric(treinando$TP_SEXO)
treinando$Q001 = as.numeric(treinando$Q001)
treinando$Q002 = as.numeric(treinando$Q002)
treinando$Q006 = as.numeric(treinando$Q006)
treinando$Q024 = as.numeric(treinando$Q024)
treinando$Q025 = as.numeric(treinando$Q025)
treinando$Q026 = as.numeric(treinando$Q026)
treinando$Q047 = as.numeric(treinando$Q047)
treinando$CO_PROVA_CN = as.numeric(treinando$CO_PROVA_CN)
treinando$CO_PROVA_CH = as.numeric(treinando$CO_PROVA_CH)
treinando$CO_PROVA_LC = as.numeric(treinando$CO_PROVA_LC)
treinando$CO_PROVA_MT = as.numeric(treinando$CO_PROVA_MT)
treinando$regioes = as.factor(treinando$regioes)
treinando$regioes = as.numeric(treinando$regioes)
treinando$NU_INSCRICAO = NULL
treinando$TP_LINGUA = NULL
treinando$CO_PROVA_CH = NULL
treinando$TP_COR_RACA = NULL
treinando$idade2 = treinando$NU_IDADE *2
treinando$media = as.factor(treinando$media)
treinando$media = as.numeric(treinando$media)
treinando$teste = treinando$NU_NOTA_CN + treinando$NU_NOTA_CN

glimpse(treinando)


# ****************************************************
# ***                   TESTE                      ***
# ***                                              ***
# ****************************************************

# Criar variável de segurança
testando = notateste
testando$Q027 = as.numeric(testando$Q027)
testando$Q027[(treinando$Q027 == 1)] <- mean(testando$Q027)

#testando$Q027 = NULL
testando$TP_DEPENDENCIA_ADM_ESC = NULL
testando$TP_ENSINO = NULL

## LIMPEZA Retirando valores NA da tabela
testando = testando %>% drop_na()
any(is.na(testando))

# Transformar variaveis em numeric
testando$SG_UF_RESIDENCIA = as.numeric(testando$SG_UF_RESIDENCIA)
testando$TP_SEXO = as.numeric(testando$TP_SEXO)
testando$Q001 = as.numeric(testando$Q001)
testando$Q002 = as.numeric(testando$Q002)
testando$Q006 = as.numeric(testando$Q006)
testando$Q024 = as.numeric(testando$Q024)
testando$Q025 = as.numeric(testando$Q025)
testando$Q026 = as.numeric(testando$Q026)
testando$Q047 = as.numeric(testando$Q047)
testando$CO_PROVA_CN = as.numeric(testando$CO_PROVA_CN)
testando$CO_PROVA_CH = as.numeric(testando$CO_PROVA_CH)
testando$CO_PROVA_LC = as.numeric(testando$CO_PROVA_LC)
testando$CO_PROVA_MT = as.numeric(testando$CO_PROVA_MT)
testando$regioes = as.factor(testando$regioes)
testando$regioes = as.numeric(testando$regioes)
notateste = testando
testando$NU_INSCRICAO = NULL
testando$TP_LINGUA = NULL
testando$CO_PROVA_CH = NULL
testando$TP_COR_RACA = NULL
testando$idade2 = testando$NU_IDADE *2
testando$media = as.factor(testando$media)
testando$media = as.numeric(testando$media)
testando$teste = testando$NU_NOTA_CN + testando$NU_NOTA_LC

# Vizualizar se os dados estão em estado numérico para envolver no algoritmo
glimpse(testando)

# ****************************************************
# ***               MODELOS DE ML                  ***
# ***                                              ***
# ****************************************************

##Regressão linear múltipla
# Etapa 3: Treinando o Modelo (usando os dados de treino)
modeloLM <- lm(NU_NOTA_MT ~ ., data = treinando)

# Visualizando os coeficientes
modeloLM

previsao1 <- predict(modelo, testando)
#View(previsao1)

#treinando$Prev = previsao1

# Etapa 4: Avaliando a Performance do Modelo
# Mais detalhes sobre o modelo
summary(modeloLM)
####### -> 48%


# Lembre-se que correlação não implica causalidade


#### Variáveis ####
trainset = treinando
testset = testando

#### Arvore de decisão ####
modelo_rf_v1 = rpart(NU_NOTA_MT ~ ., data = trainset, control = rpart.control( cp = .000999999999999999)) 
#summary(modelo_rf_v1)

modelo_rf_v1

##FILTER SELECTION
#Modelo Melhorado Ideal 600|50
modelo <- randomForest(NU_NOTA_MT ~ . , 
                       data = trainset, 
                       ntree = 600, 
                       nodesize = 52,
                       importance = TRUE)
summary(modelo)

#print(model)
names(trainset)

# Previsões nos dados de teste
tree_pred = predict(modelo, testset)

enviotree = data.frame(notateste$NU_INSCRICAO)
colnames(enviotree) = c("NU_INSCRICAO")
enviotree$NU_NOTA_MT = tree_pred

write.csv(enviotree, "answer.csv", row.names = FALSE)


#### Gradiente boosting ####
require(gbm)
modelo.boost=gbm(NU_NOTA_MT ~ . ,data = trainset,distribution = "gaussian",n.trees = 10000,
                 shrinkage = 0.01, interaction.depth = 4)
modelo.boost

summary(modelo.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance


n.trees = seq(from=100 ,to=10000, by=100) #no of trees-a vector of 100 values 

# Previsões nos dados de teste
predmatrix <- predict(modelo.boost,testset,n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix

#Calculating The Mean squared Test Error
test.error<-with(testset,apply( (predmatrix-medv)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

#Plotting the test error vs number of trees

plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.err),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)


#93.64 peguei coluna AJ

envioboost = data.frame(notateste$NU_INSCRICAO)
colnames(envioboost) = c("NU_INSCRICAO")
envioboost$NU_NOTA_MT = predmatrix

write.csv(envioboost, "answer.csv", row.names = FALSE)



#Variáveis mais relevantes Importância das feautures####
library(ggpubr)
importancia_pred <- as.data.frame(importance(modelo, scale = TRUE))
importancia_pred <- rownames_to_column(importancia_pred, var = "variable")
p1 <- ggplot(data = importancia_pred, aes(x = reorder(variable, `%IncMSE`),
                                          y = `%IncMSE`,
                                          fill = `%IncMSE`)) +
  labs(x = "variable", title = "Redução do MSE") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importancia_pred, aes(x = reorder(variable, IncNodePurity),
                                          y = IncNodePurity,
                                          fill = IncNodePurity)) +
  labs(x = "variable", title = "Redução de pureza") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggarrange(p1, p2)


oob_mse <- data.frame(oob_mse = modelo$mse,
                      arvores = seq_along(modelo$mse))
ggplot(data = oob_mse, aes(x = arvores, y = oob_mse )) +
  geom_line() +
  labs(title = "Evolução do out-of-bag-error vs número árvores",
       x = "nº árvores") +
  theme_bw()
