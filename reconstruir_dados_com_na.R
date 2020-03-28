# TRUE -> NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_REDACAO, NU_NOTA_COMP2, NU_NOTA_COMP3, NU_NOTA_COMP5, NU_NOTA_COMP4, NU_NOTA_COMP1, Q027, NU_NOTA_MT
# FALSE -> Q006, TP_ESCOLA, Q047, CO_UF_RESIDENCIA, TP_SEXO, Q001, Q024, NU_IDADE, SG_UF_RESIDENCIA

# Q006(renda da família)
# TP_ESCOLA(Tipo de escola do Ensino Médio)
# Q047(Em que tipo de escola você frequentou o Ensino Médio?)
# CO_UF_RESIDENCIA(Código da Unidade da Federação de residência)
# TP_SEXO(SEXO)
# Q001(Até que série seu pai, ou o homem responsável por você, estudou?)
# Q024(Na sua residência tem computador?)
# NU_IDADE(Idade)
# Q027(Com que idade você começou a exercer uma atividade remunerada?)

setwd("~/pCloudDrive/Profissional/ciencia-de-dados/codenation.dev/desafio_nota_do_enem")
library(randomForest) # MODELO RANDONFOREST
library(dplyr)
VariavelMutavel <- c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_REDACAO", "NU_NOTA_COMP2", "NU_NOTA_COMP3", "NU_NOTA_COMP5", "NU_NOTA_COMP4", "NU_NOTA_COMP1", "Q027", "NU_NOTA_MT")
reconstruirBase <- read.csv("train.csv")
ncol(reconstruirBase)
acuracia_do_modelo = FALSE
if(acuracia_do_modelo){
  variaveis <-  c("NU_INSCRICAO", paste("", VariavelMutavel[11], sep = ""), "Q006", "TP_ESCOLA", "Q047", "CO_UF_RESIDENCIA", "TP_SEXO", "Q001", "Q024", "NU_IDADE")
  baseVariavelTotal <- reconstruirBase[,variaveis]
  teste <- as.data.frame(sapply(baseVariavelTotal[,1:ncol(baseVariavelTotal)], as.numeric))
  teste$NU_INSCRICAO <- baseVariavelTotal$NU_INSCRICAO
  baseVariavelTotal <- teste
  rm(teste)
  baseTreinoVariavel <- baseVariavelTotal %>% filter(!is.na(baseVariavelTotal[[2]]))
  baseTesteVariavel <- baseVariavelTotal %>% filter(is.na(baseVariavelTotal[[2]]))
  
  model <- randomForest(NU_NOTA_MT ~ . , 
                        data = baseTreinoVariavel[2:ncol(baseTreinoVariavel)], 
                        ntree = 600,
                        nodesize = 50)
  
  previsao = predict(model, baseTreinoVariavel)
  enviotree = data.frame(baseTreinoVariavel$NU_INSCRICAO)
  enviotree$nomeDaVariavel = previsao
  colnames(enviotree) <- c("NU_INSCRICAO", sprintf("%s", VariavelMutavel[1]))
  result <- data.frame(Actual=baseTreinoVariavel[2],Predicted=previsao)
  result$Difference <- abs(result[1] - result$Predicted)
  #summary(result$Difference)
  sprintf("%s percentual de erro até a margem média dos erros", round(nrow(result %>% filter(Difference < 93))/nrow(result)*100, 2))
  # 90 % Ciencias da Natureza
  # 86 % Ciencias Humanas
  # 89 % Linguagens e Códigos
  # 60 % Redação
  # 98 % Computação 2
  # 98 % Computação 3
  # 99 % Computação 5
  # 98 % Computação 4
  # 97 % Computação 1
  # 100 % Idade do primeiro emprego
  # 77 % Nota de Matemática
  
  
  previsao = predict(model, baseTesteVariavel)
  baseTesteVariavel[2] <- previsao
  notasVariavel <- rbind(baseTreinoVariavel[,1:2], baseTesteVariavel[,1:2])
}

########################### Funções para cada previsão #############################

previsao_NU_NOTA_CN <- function(x, y){
  variaveis <-  c("NU_INSCRICAO", paste("", y, sep = ""), "Q006", "TP_ESCOLA", "Q047", "CO_UF_RESIDENCIA", "TP_SEXO", "Q001", "Q024", "NU_IDADE")
  baseVariavelTotal <- x[,variaveis]
  teste <- as.data.frame(sapply(baseVariavelTotal[,1:ncol(baseVariavelTotal)], as.numeric))
  teste$NU_INSCRICAO <- baseVariavelTotal$NU_INSCRICAO
  baseVariavelTotal <- teste
  rm(teste)
  baseTreinoVariavel <- baseVariavelTotal %>% filter(!is.na(baseVariavelTotal[[2]]))
  baseTesteVariavel <- baseVariavelTotal %>% filter(is.na(baseVariavelTotal[[2]]))
  model <- randomForest(NU_NOTA_CN ~ . , 
                        data = baseTreinoVariavel[2:ncol(baseTreinoVariavel)], 
                        ntree = 600,
                        nodesize = 50)
  
  previsao = predict(model, baseTesteVariavel)
  baseTesteVariavel[2] <- previsao
  notasVariavel <- rbind(baseTreinoVariavel[,1:2], baseTesteVariavel[,1:2])
  return(notasVariavel)
}

previsao_NU_NOTA_CH <- function(x, y){
  variaveis <-  c("NU_INSCRICAO", paste("", y, sep = ""), "Q006", "TP_ESCOLA", "Q047", "CO_UF_RESIDENCIA", "TP_SEXO", "Q001", "Q024", "NU_IDADE")
  baseVariavelTotal <- x[,variaveis]
  teste <- as.data.frame(sapply(baseVariavelTotal[,1:ncol(baseVariavelTotal)], as.numeric))
  teste$NU_INSCRICAO <- baseVariavelTotal$NU_INSCRICAO
  baseVariavelTotal <- teste
  rm(teste)
  baseTreinoVariavel <- baseVariavelTotal %>% filter(!is.na(baseVariavelTotal[[2]]))
  baseTesteVariavel <- baseVariavelTotal %>% filter(is.na(baseVariavelTotal[[2]]))
  model <- randomForest(NU_NOTA_CH ~ . , 
                        data = baseTreinoVariavel[2:ncol(baseTreinoVariavel)], 
                        ntree = 600,
                        nodesize = 50)
  
  previsao = predict(model, baseTesteVariavel)
  baseTesteVariavel[2] <- previsao
  notasVariavel <- rbind(baseTreinoVariavel[,1:2], baseTesteVariavel[,1:2])
  return(notasVariavel)
}

previsao_NU_NOTA_LC <- function(x, y){
  variaveis <-  c("NU_INSCRICAO", paste("", y, sep = ""), "Q006", "TP_ESCOLA", "Q047", "CO_UF_RESIDENCIA", "TP_SEXO", "Q001", "Q024", "NU_IDADE")
  baseVariavelTotal <- x[,variaveis]
  teste <- as.data.frame(sapply(baseVariavelTotal[,1:ncol(baseVariavelTotal)], as.numeric))
  teste$NU_INSCRICAO <- baseVariavelTotal$NU_INSCRICAO
  baseVariavelTotal <- teste
  rm(teste)
  baseTreinoVariavel <- baseVariavelTotal %>% filter(!is.na(baseVariavelTotal[[2]]))
  baseTesteVariavel <- baseVariavelTotal %>% filter(is.na(baseVariavelTotal[[2]]))
  model <- randomForest(NU_NOTA_LC ~ . , 
                        data = baseTreinoVariavel[2:ncol(baseTreinoVariavel)], 
                        ntree = 600,
                        nodesize = 50)
  
  previsao = predict(model, baseTesteVariavel)
  baseTesteVariavel[2] <- previsao
  notasVariavel <- rbind(baseTreinoVariavel[,1:2], baseTesteVariavel[,1:2])
  return(notasVariavel)
}

previsao_NU_NOTA_REDACAO <- function(x, y){
  variaveis <-  c("NU_INSCRICAO", paste("", y, sep = ""), "Q006", "TP_ESCOLA", "Q047", "CO_UF_RESIDENCIA", "TP_SEXO", "Q001", "Q024", "NU_IDADE")
  baseVariavelTotal <- x[,variaveis]
  teste <- as.data.frame(sapply(baseVariavelTotal[,1:ncol(baseVariavelTotal)], as.numeric))
  teste$NU_INSCRICAO <- baseVariavelTotal$NU_INSCRICAO
  baseVariavelTotal <- teste
  rm(teste)
  baseTreinoVariavel <- baseVariavelTotal %>% filter(!is.na(baseVariavelTotal[[2]]))
  baseTesteVariavel <- baseVariavelTotal %>% filter(is.na(baseVariavelTotal[[2]]))
  model <- randomForest(NU_NOTA_REDACAO ~ . , 
                        data = baseTreinoVariavel[2:ncol(baseTreinoVariavel)], 
                        ntree = 600,
                        nodesize = 50)
  
  previsao = predict(model, baseTesteVariavel)
  baseTesteVariavel[2] <- previsao
  notasVariavel <- rbind(baseTreinoVariavel[,1:2], baseTesteVariavel[,1:2])
  return(notasVariavel)
}

previsao_NU_NOTA_COMP2 <- function(x, y){
  variaveis <-  c("NU_INSCRICAO", paste("", y, sep = ""), "Q006", "TP_ESCOLA", "Q047", "CO_UF_RESIDENCIA", "TP_SEXO", "Q001", "Q024", "NU_IDADE")
  baseVariavelTotal <- x[,variaveis]
  teste <- as.data.frame(sapply(baseVariavelTotal[,1:ncol(baseVariavelTotal)], as.numeric))
  teste$NU_INSCRICAO <- baseVariavelTotal$NU_INSCRICAO
  baseVariavelTotal <- teste
  rm(teste)
  baseTreinoVariavel <- baseVariavelTotal %>% filter(!is.na(baseVariavelTotal[[2]]))
  baseTesteVariavel <- baseVariavelTotal %>% filter(is.na(baseVariavelTotal[[2]]))
  model <- randomForest(NU_NOTA_COMP2 ~ . , 
                        data = baseTreinoVariavel[2:ncol(baseTreinoVariavel)], 
                        ntree = 600,
                        nodesize = 50)
  
  previsao = predict(model, baseTesteVariavel)
  baseTesteVariavel[2] <- previsao
  notasVariavel <- rbind(baseTreinoVariavel[,1:2], baseTesteVariavel[,1:2])
  return(notasVariavel)
}

previsao_NU_NOTA_COMP3 <- function(x, y){
  variaveis <-  c("NU_INSCRICAO", paste("", y, sep = ""), "Q006", "TP_ESCOLA", "Q047", "CO_UF_RESIDENCIA", "TP_SEXO", "Q001", "Q024", "NU_IDADE")
  baseVariavelTotal <- x[,variaveis]
  teste <- as.data.frame(sapply(baseVariavelTotal[,1:ncol(baseVariavelTotal)], as.numeric))
  teste$NU_INSCRICAO <- baseVariavelTotal$NU_INSCRICAO
  baseVariavelTotal <- teste
  rm(teste)
  baseTreinoVariavel <- baseVariavelTotal %>% filter(!is.na(baseVariavelTotal[[2]]))
  baseTesteVariavel <- baseVariavelTotal %>% filter(is.na(baseVariavelTotal[[2]]))
  model <- randomForest(NU_NOTA_COMP3 ~ . , 
                        data = baseTreinoVariavel[2:ncol(baseTreinoVariavel)], 
                        ntree = 600,
                        nodesize = 50)
  
  previsao = predict(model, baseTesteVariavel)
  baseTesteVariavel[2] <- previsao
  notasVariavel <- rbind(baseTreinoVariavel[,1:2], baseTesteVariavel[,1:2])
  return(notasVariavel)
}

previsao_NU_NOTA_COMP5 <- function(x, y){
  variaveis <-  c("NU_INSCRICAO", paste("", y, sep = ""), "Q006", "TP_ESCOLA", "Q047", "CO_UF_RESIDENCIA", "TP_SEXO", "Q001", "Q024", "NU_IDADE")
  baseVariavelTotal <- x[,variaveis]
  teste <- as.data.frame(sapply(baseVariavelTotal[,1:ncol(baseVariavelTotal)], as.numeric))
  teste$NU_INSCRICAO <- baseVariavelTotal$NU_INSCRICAO
  baseVariavelTotal <- teste
  rm(teste)
  baseTreinoVariavel <- baseVariavelTotal %>% filter(!is.na(baseVariavelTotal[[2]]))
  baseTesteVariavel <- baseVariavelTotal %>% filter(is.na(baseVariavelTotal[[2]]))
  model <- randomForest(NU_NOTA_COMP5 ~ . , 
                        data = baseTreinoVariavel[2:ncol(baseTreinoVariavel)], 
                        ntree = 600,
                        nodesize = 50)
  
  previsao = predict(model, baseTesteVariavel)
  baseTesteVariavel[2] <- previsao
  notasVariavel <- rbind(baseTreinoVariavel[,1:2], baseTesteVariavel[,1:2])
  return(notasVariavel)
} 

previsao_NU_NOTA_COMP4 <- function(x, y){
  variaveis <-  c("NU_INSCRICAO", paste("", y, sep = ""), "Q006", "TP_ESCOLA", "Q047", "CO_UF_RESIDENCIA", "TP_SEXO", "Q001", "Q024", "NU_IDADE")
  baseVariavelTotal <- x[,variaveis]
  teste <- as.data.frame(sapply(baseVariavelTotal[,1:ncol(baseVariavelTotal)], as.numeric))
  teste$NU_INSCRICAO <- baseVariavelTotal$NU_INSCRICAO
  baseVariavelTotal <- teste
  rm(teste)
  baseTreinoVariavel <- baseVariavelTotal %>% filter(!is.na(baseVariavelTotal[[2]]))
  baseTesteVariavel <- baseVariavelTotal %>% filter(is.na(baseVariavelTotal[[2]]))
  model <- randomForest(NU_NOTA_COMP4 ~ . , 
                        data = baseTreinoVariavel[2:ncol(baseTreinoVariavel)], 
                        ntree = 600,
                        nodesize = 50)
  
  previsao = predict(model, baseTesteVariavel)
  baseTesteVariavel[2] <- previsao
  notasVariavel <- rbind(baseTreinoVariavel[,1:2], baseTesteVariavel[,1:2])
  return(notasVariavel)
}

previsao_NU_NOTA_COMP1 <- function(x, y){
  variaveis <-  c("NU_INSCRICAO", paste("", y, sep = ""), "Q006", "TP_ESCOLA", "Q047", "CO_UF_RESIDENCIA", "TP_SEXO", "Q001", "Q024", "NU_IDADE")
  baseVariavelTotal <- x[,variaveis]
  teste <- as.data.frame(sapply(baseVariavelTotal[,1:ncol(baseVariavelTotal)], as.numeric))
  teste$NU_INSCRICAO <- baseVariavelTotal$NU_INSCRICAO
  baseVariavelTotal <- teste
  rm(teste)
  baseTreinoVariavel <- baseVariavelTotal %>% filter(!is.na(baseVariavelTotal[[2]]))
  baseTesteVariavel <- baseVariavelTotal %>% filter(is.na(baseVariavelTotal[[2]]))
  model <- randomForest(NU_NOTA_COMP1 ~ . , 
                        data = baseTreinoVariavel[2:ncol(baseTreinoVariavel)], 
                        ntree = 600,
                        nodesize = 50)
  
  previsao = predict(model, baseTesteVariavel)
  baseTesteVariavel[2] <- previsao
  notasVariavel <- rbind(baseTreinoVariavel[,1:2], baseTesteVariavel[,1:2])
  return(notasVariavel)
}

previsao_Q027 <- function(x, y){
  variaveis <-  c("NU_INSCRICAO", paste("", y, sep = ""), "Q006", "TP_ESCOLA", "Q047", "CO_UF_RESIDENCIA", "TP_SEXO", "Q001", "Q024", "NU_IDADE")
  baseVariavelTotal <- x[,variaveis]
  teste <- as.data.frame(sapply(baseVariavelTotal[,1:ncol(baseVariavelTotal)], as.numeric))
  teste$NU_INSCRICAO <- baseVariavelTotal$NU_INSCRICAO
  baseVariavelTotal <- teste
  rm(teste)
  baseTreinoVariavel <- baseVariavelTotal %>% filter(!is.na(baseVariavelTotal[[2]]))
  baseTesteVariavel <- baseVariavelTotal %>% filter(is.na(baseVariavelTotal[[2]]))
  model <- randomForest(Q027 ~ . , 
                        data = baseTreinoVariavel[2:ncol(baseTreinoVariavel)], 
                        ntree = 600,
                        nodesize = 50)
  
  previsao = predict(model, baseTesteVariavel)
  baseTesteVariavel[2] <- previsao
  notasVariavel <- rbind(baseTreinoVariavel[,1:2], baseTesteVariavel[,1:2])
  return(notasVariavel)
}

previsao_NU_NOTA_MT <- function(x, y){
  variaveis <-  c("NU_INSCRICAO", paste("", y, sep = ""), "Q006", "TP_ESCOLA", "Q047", "CO_UF_RESIDENCIA", "TP_SEXO", "Q001", "Q024", "NU_IDADE")
  baseVariavelTotal <- x[,variaveis]
  teste <- as.data.frame(sapply(baseVariavelTotal[,1:ncol(baseVariavelTotal)], as.numeric))
  teste$NU_INSCRICAO <- baseVariavelTotal$NU_INSCRICAO
  baseVariavelTotal <- teste
  rm(teste)
  baseTreinoVariavel <- baseVariavelTotal %>% filter(!is.na(baseVariavelTotal[[2]]))
  baseTesteVariavel <- baseVariavelTotal %>% filter(is.na(baseVariavelTotal[[2]]))
  model <- randomForest(NU_NOTA_MT ~ . , 
                        data = baseTreinoVariavel[2:ncol(baseTreinoVariavel)], 
                        ntree = 600,
                        nodesize = 50)
  
  previsao = predict(model, baseTesteVariavel)
  baseTesteVariavel[2] <- previsao
  notasVariavel <- rbind(baseTreinoVariavel[,1:2], baseTesteVariavel[,1:2])
  return(notasVariavel)
}

teste1 <- previsao_NU_NOTA_CN(reconstruirBase, VariavelMutavel[1])
teste2 <- previsao_NU_NOTA_CH(reconstruirBase, VariavelMutavel[2])
teste3 <- previsao_NU_NOTA_LC(reconstruirBase, VariavelMutavel[3])
teste4 <- previsao_NU_NOTA_REDACAO(reconstruirBase, VariavelMutavel[4])
teste5 <- previsao_NU_NOTA_COMP2(reconstruirBase, VariavelMutavel[5])
teste6 <- previsao_NU_NOTA_COMP3(reconstruirBase, VariavelMutavel[6])
teste7 <- previsao_NU_NOTA_COMP5(reconstruirBase, VariavelMutavel[7])
teste8 <- previsao_NU_NOTA_COMP4(reconstruirBase, VariavelMutavel[8])
teste9 <- previsao_NU_NOTA_COMP1(reconstruirBase, VariavelMutavel[9])
teste10 <- previsao_Q027(reconstruirBase, VariavelMutavel[10])
teste11 <- previsao_NU_NOTA_MT(reconstruirBase, VariavelMutavel[11])

reconstrucaoDataSet <- left_join(teste1, teste2)
reconstrucaoDataSet <- reconstrucaoDataSet %>% left_join(teste3)
reconstrucaoDataSet <- reconstrucaoDataSet %>% left_join(teste4)
reconstrucaoDataSet <- reconstrucaoDataSet %>% left_join(teste5)
reconstrucaoDataSet <- reconstrucaoDataSet %>% left_join(teste6)
reconstrucaoDataSet <- reconstrucaoDataSet %>% left_join(teste7)
reconstrucaoDataSet <- reconstrucaoDataSet %>% left_join(teste8)
reconstrucaoDataSet <- reconstrucaoDataSet %>% left_join(teste9)
reconstrucaoDataSet <- reconstrucaoDataSet %>% left_join(teste10)
reconstrucaoDataSet <- reconstrucaoDataSet %>% left_join(teste11)

restante_das_variaveis <- c("NU_INSCRICAO", "SG_UF_RESIDENCIA", "Q006", "TP_ESCOLA", "Q047", "CO_UF_RESIDENCIA", "TP_SEXO", "Q001", "Q024", "NU_IDADE") 
teste <- left_join(reconstrucaoDataSet, reconstruirBase[, restante_das_variaveis])
variaveisParaExcluir <- ls()
nota <- teste
inicio <- tempo
rm(list = variaveisParaExcluir)










