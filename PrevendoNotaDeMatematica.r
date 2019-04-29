# Machine Learning - Regressão 
# Prevendo Notas de Matemática dos Participantes do ENEM

# Configurando o diretório de trabalho
setwd("C:/Users/Matilde/Dropbox/DESAFIO-Codenation(ENEM)/desafio_nota_do_enem")
getwd()

library(dplyr)     # Filtragens
library(psych)     # Scatterplot Matrix
library(e1071)     # SVM
library(rpart)     # RANDON FOREST
library(readr)     # GERAR CSV
library(tidyverse) # Drop_NA()
library(readxl)    # Importar xls

# ****************************************************
# ***                   DADOS                      ***
# ***                                              ***
# ****************************************************
# Etapa 1 - Coletando os dados
nota <- read.csv("train.csv")
notateste <- read.csv("test.csv")
PIB_2016 <- read_excel("PIB-2016.xls")


# ****************************************************
# ***                   INCLUIR                    ***
# ***                NOVAS VARIÁVEIS               ***
# ****************************************************
#média do pib por estado
PIB_2016 = PIB_2016 %>% filter(Ano == 2016)
PIB_2016 = PIB_2016[c(names(PIB_2016)[5], names(PIB_2016)[42])]
MEDIA_PIB_2016 = aggregate(PIB_2016$`Produto Interno Bruto per capita
                           (R$ 1,00)`, 
                           by=list(PIB_2016$`Sigla da Unidade da Federação`),
                           FUN = mean)
colnames(MEDIA_PIB_2016) = c("SG_UF_RESIDENCIA","media")

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
notateste = merge(notateste,MEDIA_PIB_2016)
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
nota = merge(nota,MEDIA_PIB_2016)


# ****************************************************
# ***                   TREINO                     ***
# ***                                              ***
# ****************************************************
# Segmentando apenas os dados que temos no teste
treinando = nota[c(nomes, "NU_NOTA_MT")]
treinando$Q027 = as.numeric(treinando$Q027)
treinando$Q027[(treinando$Q027 == 1)] <- mean(treinando$Q027)
glimpse(treinando)

#Para que eu possa pegar exatamente os nomes das variáveis mais a target
treinando = treinando %>% drop_na()
#treinando$Q027 = NULL
treinando$TP_DEPENDENCIA_ADM_ESC = NULL
treinando$TP_ENSINO = NULL
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
treinando = treinando[c(-1)]
nota = treinando
treinando = treinando[c(-1)]

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


# Vizualizar se os dados estão em estado numérico para envolver no algoritmo
glimpse(testando)

# ****************************************************
# ***               MODELOS DE ML                  ***
# ***                                              ***
# ****************************************************

##Regressão linear múltipla
# Etapa 3: Treinando o Modelo (usando os dados de treino)
modelo <- lm(NU_NOTA_MT ~ ., data = treinando)

# Visualizando os coeficientes
modelo

previsao1 <- predict(modelo)
View(previsao1)

#treinando$Prev = previsao1

# Etapa 4: Avaliando a Performance do Modelo
# Mais detalhes sobre o modelo
summary(modelo)
####### -> 48%


# Lembre-se que correlação não implica causalidade


#### RANDONFOREST (Arvores de decisão) ####
trainset = treinando
testset = testando


modelo_rf_v1 = rpart(NU_NOTA_MT ~ ., data = trainset, control = rpart.control( cp = .0009999999)) 
summary(modelo_rf_v1)

# Previsões nos dados de teste
tree_pred = predict(modelo_rf_v1, testset)

enviotree = data.frame(notateste$NU_INSCRICAO)
colnames(enviotree) = c("NU_INSCRICAO")
enviotree$NU_NOTA_MT = tree_pred

write.csv(enviotree, "answer.csv", row.names = FALSE)
