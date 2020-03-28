# Machine Learning - Regressão 
# Prevendo Notas de Matemática dos Participantes do ENEM

# Configurando o diretório de trabalho
setwd("~/pCloudDrive/Profissional/ciencia-de-dados/codenation.dev/desafio_nota_do_enem")
getwd()

library(dplyr)        # Filtragens
library(psych)        # Scatterplot Matrix
library(e1071)        # SVM
library(rpart)        # RANDON FOREST
library(readr)        # GERAR CSV
library(tidyverse)    # Drop_NA()
library(readxl)       # Importar xls
library("data.table") # Comando FREAD
library(DT)           # Tabelas Editáveis

# ****************************************************
# ***                   DADOS                      ***
# ***                                              ***
# ****************************************************
# Etapa 1 - Coletando os dados
nota <- fread("train.csv")
notateste <- fread("test.csv")


# ****************************************************
# ***                   INCLUIR                    ***
# ***                NOVAS VARIÁVEIS               ***
# ****************************************************
macro <- read_excel("macro.xls")
x <- c(macro$`UF [-]`)
lookup <- c(Acre = "AC",  Alagoas = "AL",  Amazonas = "AM",  Amapá = "AP",  Bahia = "BA", Ceará = "CE", `Distrito Federal` = "DF", `Espírito Santo` = "ES", Goiás = "GO", Maranhão = "MA", `Minas Gerais` = "MG", `Mato Grosso do Sul` = "MS", `Mato Grosso` = "MT", Pará = "PA", Paraíba = "PB", Pernambuco = "PE", Piauí = "PI", Paraná = "PR", `Rio de Janeiro` = "RJ", `Rio Grande do Norte` = "RN", Rondônia = "RO", Roraima = "RR",`Rio Grande do Sul` =  "RS", `Santa Catarina` =  "SC", Sergipe = "SE", `São Paulo` = "SP", Tocantins = "TO")
uniao = lookup[x]
UF = unname(uniao)
macro$UF = UF
macro$`UF [-]` = NULL
macro$`Capital [2010]` = NULL
names(macro)
colnames(macro) = c("Area_territorial_2018", "Pop_estimada_2018","Dens_demografica_2010","Matriculas_ensino_fundamental_2017","IDH_2010","Receitas_2017","Despesas_empenhadas_2017","Rend_domiciliar_2017_Percapita","Total_de_Veiculos_2016","SG_UF_RESIDENCIA")


PIB_2016 <- read_excel("PIB-2016.xls")


nomes <- names(notateste)
#soma do pib por estado
PIB_2016 = PIB_2016 %>% filter(Ano == 2016)
PIB_2016 = PIB_2016[c(names(PIB_2016)[5], names(PIB_2016)[42])]
SOMA_PIB_2016 = aggregate(PIB_2016$`Produto Interno Bruto per capita
(R$ 1,00)`, by=list(PIB_2016$`Sigla da Unidade da Federação`), FUN = sum)
#DT::datatable(SOMA_PIB_2016, editable = TRUE)
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
notateste = merge(macro, notateste)
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
nota = merge(nota, SOMA_PIB_2016)
nota = merge(macro, nota)



# ****************************************************
# ***                   TESTE                      ***
# ***                                              ***
# ****************************************************
testando = notateste
testando$Q027 = as.factor(testando$Q027)
testando$Q027 = as.numeric(testando$Q027)
testando$Q027[(testando$Q027 == 1)] <- mean(testando$Q027)
glimpse(testando)
testando$TP_DEPENDENCIA_ADM_ESC = NULL
testando$TP_ENSINO = NULL

## LIMPEZA Retirando valores NA da tabela
testando = testando %>% drop_na()
any(is.na(testando))

# Transformar variaveis em factor
testando$SG_UF_RESIDENCIA = as.factor(testando$SG_UF_RESIDENCIA)
testando$TP_SEXO = as.factor(testando$TP_SEXO)
testando$Q001 = as.factor(testando$Q001)
testando$Q002 = as.factor(testando$Q002)
testando$Q006 = as.factor(testando$Q006)
testando$Q024 = as.factor(testando$Q024)
testando$Q025 = as.factor(testando$Q025)
testando$Q026 = as.factor(testando$Q026)
testando$Q047 = as.factor(testando$Q047)
testando$CO_PROVA_CN = as.factor(testando$CO_PROVA_CN)
testando$CO_PROVA_CH = as.factor(testando$CO_PROVA_CH)
testando$CO_PROVA_LC = as.factor(testando$CO_PROVA_LC)
testando$CO_PROVA_MT = as.factor(testando$CO_PROVA_MT)
testando$regioes = as.factor(testando$regioes)
testando$regioes = as.factor(testando$regioes)

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
# ***                   TREINO                     ***
# ***                                              ***
# ****************************************************
# Segmentando apenas os dados que temos no treino
treinando = nota
#treinando = nota[c(nomes, "NU_NOTA_MT")]
#treinando <- treinando[,!(names(treinando)%in% nomes)]
#treinando[,names(treinando)]= lapply(treinando[,names(treinando)], as.numeric)
# ****************************************************
# ***     Tratamento de variáveis vazias           ***
# ****************************************************
treinando$Q027 = as.factor(treinando$Q027)
treinando$Q027 = as.numeric(treinando$Q027)
treinando$Q027[is.na(treinando$Q027)] <- mean(treinando$Q027)
treinando$Q028 = as.factor(treinando$Q028)
treinando$Q028 = as.numeric(treinando$Q028)
treinando$Q028[is.na(treinando$Q028)] <- mean(treinando$Q028)
treinando$NO_ENTIDADE_CERTIFICACAO = as.factor(treinando$NO_ENTIDADE_CERTIFICACAO)
treinando$NO_ENTIDADE_CERTIFICACAO = as.numeric(treinando$NO_ENTIDADE_CERTIFICACAO)
treinando$NO_ENTIDADE_CERTIFICACAO[is.na(treinando$NO_ENTIDADE_CERTIFICACAO)] <- mean(treinando$NO_ENTIDADE_CERTIFICACAO)
treinando$SG_UF_ENTIDADE_CERTIFICACAO  = as.factor(treinando$SG_UF_ENTIDADE_CERTIFICACAO )
treinando$SG_UF_ENTIDADE_CERTIFICACAO  = as.numeric(treinando$SG_UF_ENTIDADE_CERTIFICACAO )
treinando$SG_UF_ENTIDADE_CERTIFICACAO [is.na(treinando$SG_UF_ENTIDADE_CERTIFICACAO )] <- mean(treinando$SG_UF_ENTIDADE_CERTIFICACAO )
treinando$SG_UF_ESC  = as.factor(treinando$SG_UF_ESC )
treinando$SG_UF_ESC  = as.numeric(treinando$SG_UF_ESC )
treinando$SG_UF_ESC [is.na(treinando$SG_UF_ESC )] <- mean(treinando$SG_UF_ESC )
treinando$NO_MUNICIPIO_ESC  = as.factor(treinando$NO_MUNICIPIO_ESC )
treinando$NO_MUNICIPIO_ESC  = as.numeric(treinando$NO_MUNICIPIO_ESC )
treinando$NO_MUNICIPIO_ESC [is.na(treinando$NO_MUNICIPIO_ESC )] <- mean(treinando$NO_MUNICIPIO_ESC )
# ****************************************************
# ***     Tratamento de variáveis NULAS            ***
# ****************************************************
treinando$CO_UF_ENTIDADE_CERTIFICACAO = NULL
treinando$TP_DEPENDENCIA_ADM_ESC = NULL
treinando$TP_LOCALIZACAO_ESC = NULL
treinando$TP_SIT_FUNC_ESC = NULL
treinando$CO_UF_ESC = NULL
treinando$CO_ESCOLA = NULL
treinando$CO_MUNICIPIO_ESC = NULL
treinando$Q029 = NULL
treinando$Q030 = NULL
treinando$Q031 = NULL
treinando$Q032 = NULL
treinando$Q033 = NULL
treinando$Q041 = NULL
treinando$TP_ENSINO = NULL
treinando = treinando %>% drop_na()
any(is.na(treinando))
glimpse(treinando)

# ****************************************************
# ***              FORMATO FACTOR                  ***
# ****************************************************
treinando$SG_UF_RESIDENCIA = as.factor(treinando$SG_UF_RESIDENCIA)
treinando$TP_SEXO = as.factor(treinando$TP_SEXO)
treinando$Q001 = as.factor(treinando$Q001)
treinando$Q002 = as.factor(treinando$Q002)
treinando$Q003 = as.factor(treinando$Q003)
treinando$Q004 = as.factor(treinando$Q004)
treinando$Q006 = as.factor(treinando$Q006)
treinando$Q007 = as.factor(treinando$Q007)
treinando$Q008 = as.factor(treinando$Q008)
treinando$Q009 = as.factor(treinando$Q009)
treinando$Q010 = as.factor(treinando$Q010)
treinando$Q011 = as.factor(treinando$Q011)
treinando$Q012 = as.factor(treinando$Q012)
treinando$Q013 = as.factor(treinando$Q013)
treinando$Q014 = as.factor(treinando$Q014)
treinando$Q015 = as.factor(treinando$Q015)
treinando$Q016 = as.factor(treinando$Q016)
treinando$Q017 = as.factor(treinando$Q017)
treinando$Q018 = as.factor(treinando$Q018)
treinando$Q019 = as.factor(treinando$Q019)
treinando$Q020 = as.factor(treinando$Q020)
treinando$Q021 = as.factor(treinando$Q021)
treinando$Q022 = as.factor(treinando$Q022)
treinando$Q023 = as.factor(treinando$Q023)
treinando$Q024 = as.factor(treinando$Q024)
treinando$Q025 = as.factor(treinando$Q025)
treinando$Q026 = as.factor(treinando$Q026)
treinando$Q042 = as.factor(treinando$Q042)
treinando$Q043 = as.factor(treinando$Q043)
treinando$Q044 = as.factor(treinando$Q044)
treinando$Q045 = as.factor(treinando$Q045)
treinando$Q046 = as.factor(treinando$Q046)
treinando$Q047 = as.factor(treinando$Q047)
treinando$Q048 = as.factor(treinando$Q048)
treinando$Q049 = as.factor(treinando$Q049)
treinando$Q050 = as.factor(treinando$Q050)
treinando$CO_PROVA_CN = as.factor(treinando$CO_PROVA_CN)
treinando$CO_PROVA_CH = as.factor(treinando$CO_PROVA_CH)
treinando$CO_PROVA_LC = as.factor(treinando$CO_PROVA_LC)
treinando$CO_PROVA_MT = as.factor(treinando$CO_PROVA_MT)
treinando$regioes = as.factor(treinando$regioes)
treinando$regioes = as.factor(treinando$regioes)
treinando$TX_GABARITO_MT = as.factor(treinando$TX_GABARITO_MT)
treinando$TX_GABARITO_LC = as.factor(treinando$TX_GABARITO_LC)
treinando$TX_GABARITO_CH = as.factor(treinando$TX_GABARITO_CH)
treinando$TX_GABARITO_CN = as.factor(treinando$TX_GABARITO_CN)
treinando$TX_RESPOSTAS_CN = as.factor(treinando$TX_RESPOSTAS_CN)
treinando$TX_RESPOSTAS_MT = as.factor(treinando$TX_RESPOSTAS_MT)
treinando$TX_RESPOSTAS_LC = as.factor(treinando$TX_RESPOSTAS_LC)
treinando$TX_RESPOSTAS_CH = as.factor(treinando$TX_RESPOSTAS_CH)
treinando$SG_UF_PROVA = as.factor(treinando$SG_UF_PROVA)
treinando$NO_MUNICIPIO_PROVA = as.factor(treinando$NO_MUNICIPIO_PROVA)
treinando$SG_UF_NASCIMENTO = as.factor(treinando$SG_UF_NASCIMENTO)
treinando$NO_MUNICIPIO_NASCIMENTO = as.factor(treinando$NO_MUNICIPIO_NASCIMENTO)
treinando$NO_MUNICIPIO_RESIDENCIA = as.factor(treinando$NO_MUNICIPIO_RESIDENCIA)

# ****************************************************
# ***              FORMATO NUMÉRICO                ***
# ****************************************************
treinando$SG_UF_RESIDENCIA = as.numeric(treinando$SG_UF_RESIDENCIA)
treinando$TP_SEXO = as.numeric(treinando$TP_SEXO)
treinando$Q001 = as.numeric(treinando$Q001)
treinando$Q002 = as.numeric(treinando$Q002)
treinando$Q003 = as.numeric(treinando$Q003)
treinando$Q004 = as.numeric(treinando$Q004)
treinando$Q006 = as.numeric(treinando$Q006)
treinando$Q007 = as.numeric(treinando$Q007)
treinando$Q008 = as.numeric(treinando$Q008)
treinando$Q009 = as.numeric(treinando$Q009)
treinando$Q010 = as.numeric(treinando$Q010)
treinando$Q011 = as.numeric(treinando$Q011)
treinando$Q012 = as.numeric(treinando$Q012)
treinando$Q013 = as.numeric(treinando$Q013)
treinando$Q014 = as.numeric(treinando$Q014)
treinando$Q015 = as.numeric(treinando$Q015)
treinando$Q016 = as.numeric(treinando$Q016)
treinando$Q017 = as.numeric(treinando$Q017)
treinando$Q018 = as.numeric(treinando$Q018)
treinando$Q019 = as.numeric(treinando$Q019)
treinando$Q020 = as.numeric(treinando$Q020)
treinando$Q021 = as.numeric(treinando$Q021)
treinando$Q022 = as.numeric(treinando$Q022)
treinando$Q023 = as.numeric(treinando$Q023)
treinando$Q024 = as.numeric(treinando$Q024)
treinando$Q025 = as.numeric(treinando$Q025)
treinando$Q026 = as.numeric(treinando$Q026)
treinando$Q042 = as.numeric(treinando$Q042)
treinando$Q043 = as.numeric(treinando$Q043)
treinando$Q044 = as.numeric(treinando$Q044)
treinando$Q045 = as.numeric(treinando$Q045)
treinando$Q046 = as.numeric(treinando$Q046)
treinando$Q047 = as.numeric(treinando$Q047)
treinando$Q048 = as.numeric(treinando$Q048)
treinando$Q049 = as.numeric(treinando$Q049)
treinando$Q050 = as.numeric(treinando$Q050)
treinando$CO_PROVA_CN = as.numeric(treinando$CO_PROVA_CN)
treinando$CO_PROVA_CH = as.numeric(treinando$CO_PROVA_CH)
treinando$CO_PROVA_LC = as.numeric(treinando$CO_PROVA_LC)
treinando$CO_PROVA_MT = as.numeric(treinando$CO_PROVA_MT)
treinando$regioes = as.factor(treinando$regioes)
treinando$regioes = as.numeric(treinando$regioes)
treinando$TX_GABARITO_MT = as.numeric(treinando$TX_GABARITO_MT)
treinando$TX_GABARITO_LC = as.numeric(treinando$TX_GABARITO_LC)
treinando$TX_GABARITO_CH = as.numeric(treinando$TX_GABARITO_CH)
treinando$TX_GABARITO_CN = as.numeric(treinando$TX_GABARITO_CN)
treinando$TX_RESPOSTAS_CN = as.numeric(treinando$TX_RESPOSTAS_CN)
treinando$TX_RESPOSTAS_MT = as.numeric(treinando$TX_RESPOSTAS_MT)
treinando$TX_RESPOSTAS_LC = as.numeric(treinando$TX_RESPOSTAS_LC)
treinando$TX_RESPOSTAS_CH = as.numeric(treinando$TX_RESPOSTAS_CH)
treinando$SG_UF_PROVA = as.numeric(treinando$SG_UF_PROVA)
treinando$NO_MUNICIPIO_PROVA = as.numeric(treinando$NO_MUNICIPIO_PROVA)
treinando$SG_UF_NASCIMENTO = as.numeric(treinando$SG_UF_NASCIMENTO)
treinando$NO_MUNICIPIO_NASCIMENTO = as.numeric(treinando$NO_MUNICIPIO_NASCIMENTO)
treinando$NO_MUNICIPIO_RESIDENCIA = as.numeric(treinando$NO_MUNICIPIO_RESIDENCIA)
nota = treinando
treinando$NU_INSCRICAO = NULL
glimpse(treinando)

# ****************************************************
# ***          Excluir Especificidades             ***
# ****************************************************
treinando$V1 = NULL
treinando$NU_ANO = NULL
treinando$CO_MUNICIPIO_RESIDENCIA = NULL
treinando$NO_MUNICIPIO_RESIDENCIA = NULL
treinando$TP_ESTADO_CIVIL = NULL
treinando$CO_MUNICIPIO_NASCIMENTO = NULL
treinando$NO_MUNICIPIO_NASCIMENTO = NULL
treinando$CO_UF_NASCIMENTO = NULL
treinando$SG_UF_NASCIMENTO = NULL
treinando$NO_MUNICIPIO_ESC = NULL
treinando$SG_UF_ESC = NULL
treinando$IN_DEFICIENCIA_AUDITIVA = NULL
treinando$IN_SURDO_CEGUEIRA = NULL
treinando$IN_DEFICIENCIA_FISICA = NULL
treinando$IN_DEFICIENCIA_MENTAL = NULL
treinando$IN_DEFICIT_ATENCAO = NULL
treinando$IN_AUTISMO = NULL
treinando$IN_VISAO_MONOCULAR = NULL
treinando$IN_OUTRA_DEF = NULL
treinando$IN_LACTANTE = NULL
treinando$IN_ESTUDA_CLASSE_HOSPITALAR = NULL
treinando$IN_SEM_RECURSO = NULL
treinando$IN_BRAILLE = NULL
treinando$IN_AMPLIADA_24 = NULL
treinando$IN_AMPLIADA_18 = NULL
treinando$IN_LEDOR = NULL
treinando$IN_ACESSO = NULL
treinando$IN_TRANSCRICAO = NULL
treinando$IN_LIBRAS = NULL
treinando$IN_LEITURA_LABIAL = NULL
treinando$IN_MESA_CADEIRA_RODAS = NULL
treinando$IN_MESA_CADEIRA_SEPARADA = NULL
treinando$IN_APOIO_PERNA = NULL
treinando$IN_GUIA_INTERPRETE = NULL
treinando$IN_MACA = NULL
treinando$IN_COMPUTADOR = NULL
treinando$IN_CADEIRA_ESPECIAL = NULL
treinando$IN_CADEIRA_CANHOTO = NULL
treinando$IN_PROVA_DEITADO = NULL
treinando$IN_MOBILIARIO_OBESO = NULL
treinando$IN_LAMINA_OVERLAY = NULL
treinando$IN_PROTETOR_AURICULAR = NULL
treinando$IN_MEDIDOR_GLICOSE = NULL
treinando$IN_MAQUINA_BRAILE = NULL
treinando$IN_SOROBAN = NULL
treinando$IN_MARCA_PASSO = NULL
treinando$IN_SONDA = NULL
treinando$IN_MEDICAMENTOS = NULL
treinando$IN_SALA_INDIVIDUAL = NULL
treinando$IN_SALA_ESPECIAL = NULL
treinando$IN_SALA_ACOMPANHANTE = NULL
treinando$IN_MOBILIARIO_ESPECIFICO = NULL
treinando$IN_MATERIAL_ESPECIFICO = NULL
treinando$IN_NOME_SOCIAL = NULL
treinando$IN_CERTIFICADO = NULL
treinando$NO_ENTIDADE_CERTIFICACAO = NULL
treinando$SG_UF_ENTIDADE_CERTIFICACAO = NULL
treinando$CO_MUNICIPIO_PROVA = NULL
treinando$NO_MUNICIPIO_PROVA = NULL
treinando$CO_UF_PROVA = NULL
treinando$SG_UF_PROVA = NULL
treinando$TP_PRESENCA_MT = NULL
treinando$TX_RESPOSTAS_CN = NULL
treinando$TX_RESPOSTAS_CH = NULL
treinando$TX_RESPOSTAS_LC = NULL
treinando$TX_RESPOSTAS_MT = NULL
treinando$TX_GABARITO_CN = NULL
treinando$TX_GABARITO_CH = NULL
treinando$TX_GABARITO_LC = NULL
treinando$TX_GABARITO_MT = NULL
treinando$Q003 = NULL
treinando$Q004 = NULL
treinando$Q005 = NULL
treinando$Q007 = NULL
treinando$Q008 = NULL
treinando$Q009 = NULL
treinando$Q010 = NULL
treinando$Q011 = NULL
treinando$Q012 = NULL
treinando$Q013 = NULL
treinando$Q014 = NULL
treinando$Q015 = NULL
treinando$Q016 = NULL
treinando$Q017 = NULL
treinando$Q018 = NULL
treinando$Q019 = NULL
treinando$Q020 = NULL
treinando$Q021 = NULL
treinando$Q022 = NULL
treinando$Q023 = NULL
treinando$Q028 = NULL
treinando$Q034 = NULL
treinando$Q035 = NULL
treinando$Q036 = NULL
treinando$Q037 = NULL
treinando$Q038 = NULL
treinando$Q039 = NULL
treinando$Q040 = NULL
treinando$Q042 = NULL
treinando$Q043 = NULL
treinando$Q044 = NULL
treinando$Q045 = NULL
treinando$Q046 = NULL
treinando$Q048 = NULL
treinando$Q049 = NULL
treinando$Q050 = NULL
treinando$IN_CADEIRA_ACOLCHOADA = NULL






#####
# ****************************************************####
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
####### -> 45.14%


# Lembre-se que correlação não implica causalidade


#### RANDONFOREST (Arvores de decisão) ####
trainset = treinando
testset = testando


modelo_rf_v1 = rpart(NU_NOTA_MT ~ ., data = trainset, control = rpart.control( cp = .00099999999999)) 
#summary(modelo_rf_v1)

# Previsões nos dados de teste
tree_pred = predict(modelo_rf_v1, testset)

enviotree = as.data.frame(notateste$NU_INSCRICAO)
colnames(enviotree) = c("NU_INSCRICAO")
enviotree$NU_NOTA_MT = tree_pred

write.csv(enviotree, "answer.csv", row.names = FALSE)
# 93.3%



