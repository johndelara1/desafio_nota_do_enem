# Machine Learning - Regressão 
# Prevendo Notas de Matemática dos Participantes do ENEM

# Configurando o diretório de trabalho
setwd("C:/Users/Matilde/Dropbox/DESAFIO-Codenation(ENEM)/desafio_nota_do_enem")
getwd()

# Problema de Negócio: Previsão de Notas de Matemática dos participantes

# Para esta análise, vamos usar um conjunto de dados simulando do Exame Nacional de Ensino Médio (ENEM) hipotéticas 
# para um conjunto de participantes espalhados por 5 regiões do Brasil.
# Esse dataset de treino possui 13.730 observações e 167 variáveis.


#### TESTE #####
#Transformar os dois datasets com as mesmas variáveis
notateste <- read.csv("test.csv")
library(dplyr)
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

# Criar vetor com nomes
nomes = names(notateste)
testando = notateste[c(nomes, "NU_INSCRICAO")]
#testando = testando[c(nomes[-40:-47])]
testando = testando[c(-25:-28)]
testando = testando[c(-1)]
testando = testando[c(-44)]

# Abastecer de informações nossa base ----
## LIMPEZA Retirando valores NA da tabela
library(tidyr)
# 13730*26.43/100 -> 3628.839 de dados, Sem valores NA dataset corresponde a 26.43%
testando = testando %>% drop_na()

# Transformar variaveis em numeric

testando$SG_UF_RESIDENCIA = as.numeric(testando$SG_UF_RESIDENCIA)
testando$TP_SEXO = as.numeric(testando$TP_SEXO)
testando$regioes = as.factor(testando$regioes)
testando$regioes = as.numeric(testando$regioes)
testando$Q001 = as.numeric(testando$Q001)
testando$Q002 = as.numeric(testando$Q002)
testando$Q006 = as.numeric(testando$Q006)
testando$Q024 = as.numeric(testando$Q024)
testando$Q025 = as.numeric(testando$Q025)
testando$Q026 = as.numeric(testando$Q026)
testando$Q027 = as.numeric(testando$Q027)
testando$Q047 = as.numeric(testando$Q047)

names(testando[35:42])


# Vizualizar se os dados estão em estado numérico para envolver no algoritmo
glimpse(testando)
names(glimpse(testando))
summary(testando)
nomes = names(testando)


#### TREINO #####
# Etapa 1 - Coletando os dados
nota <- read.csv("train.csv")
#View(nota)

# Etapa 2: Explorando e Preparando os Dados
# Visualizando as variáveis
str(nota)

# Medias de Tendência Central da variável NOTAS_MT
summary(nota$NU_NOTA_MT)

# Visualizar se possui alguma nota com valor NA
any(is.na(nota$NU_NOTA_MT))

# Construindo um histograma
hist(nota$NU_NOTA_MT, main = 'Histograma', xlab = 'Notas')

# Criar Tabela de contingência das regiões
names(table(nota$SG_UF_RESIDENCIA))

# Criar regioes com os estados por prova
#intalar pacote dplyr
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

# Segmentando apenas os dados que temos no teste
nota = nota[c(nomes, "NU_NOTA_MT")]
treinando =  nota %>% drop_na()
#treinando = treinando[c(-37)]
treinando$SG_UF_RESIDENCIA = as.numeric(treinando$SG_UF_RESIDENCIA)
treinando$TP_SEXO = as.numeric(treinando$TP_SEXO)
treinando$regioes = as.factor(treinando$regioes)
treinando$regioes = as.numeric(treinando$regioes)
treinando$Q001 = as.numeric(treinando$Q001)
treinando$Q002 = as.numeric(treinando$Q002)
treinando$Q006 = as.numeric(treinando$Q006)
treinando$Q024 = as.numeric(treinando$Q024)
treinando$Q025 = as.numeric(treinando$Q025)
treinando$Q026 = as.numeric(treinando$Q026)
treinando$Q027 = as.numeric(treinando$Q027)
treinando$Q047 = as.numeric(treinando$Q047)
nota = treinando
treinando = treinando[c(-1)]

glimpse(treinando)

# Scatterplot Matrix
#install.packages("psych")
library(psych)

# Etapa 3: Treinando o Modelo (usando os dados de treino)
modelo <- lm(NU_NOTA_MT ~ ., data = treinando)

# Visualizando os coeficientes
modelo

# Aqui verificamos as notas previstas pelo modelo que devem ser iguais aos dados de treino
previsao1 <- predict(modelo)
View(previsao1)

# Etapa 4: Avaliando a Performance do Modelo
# Mais detalhes sobre o modelo
summary(modelo)


# ****************************************************
# *** Estas informações abaixo é que farão de você ***
# *** um verdadeiro conhecedor de Machine Learning ***
# ****************************************************

# Equação de Regressão
# y = a + bx (simples)
# y = a + b0x0 + b1x1 (múltipla)

# Resíduos
# Diferença entre os valores observados de uma variável e seus valores previstos
# Seus resíduos devem se parecer com uma distribuição normal, o que indica
# que a média entre os valores previstos e os valores observados é próximo de 0 (o que é bom)

# Coeficiente - Intercept - a (alfa)
# Valor de a na equação de regressão

# Coeficientes - Nomes das variáveis - b (beta)
# Valor de b na equação de regressão

# Obs: A questão é que lm() ou summary() têm diferentes convenções de 
# rotulagem para cada variável explicativa. 
# Em vez de escrever slope_1, slope_2, .... 
# Eles simplesmente usam o nome da variável em qualquer saída para 
# indicar quais coeficientes pertencem a qual variável.

# Erro Padrão
# Medida de variabilidade na estimativa do coeficiente a (alfa). O ideal é que este valor 
# seja menor que o valor do coeficiente, mas nem sempre isso irá ocorrer.

# Asteriscos 
# Os asteriscos representam os níveis de significância de acordo com o p-value.
# Quanto mais estrelas, maior a significância.
# Atenção --> Muitos astericos indicam que é improvável que não exista 
# relacionamento entre as variáveis.

# Valor t
# Define se coeficiente da variável é significativo ou não para o modelo. 
# Ele é usado para calcular o p-value e os níveis de significância.

# p-value
# O p-value representa a probabilidade que a variável não seja relevante. 
# Deve ser o menor valor possível. 
# Se este valor for realmente pequeno, o R irá mostrar o valor 
# como notação científica

# Significância
# São aquelas legendas próximas as suas variáveis
# Espaço em branco - ruim
# Pontos - razoável
# Asteriscos - bom
# Muitos asteriscos - muito bom

# Residual Standar Error
# Este valor representa o desvio padrão dos resíduos

# Degrees of Freedom
# É a diferença entre o número de observações na amostra de treinamento 
# e o número de variáveis no seu modelo

# R-squared (coeficiente de determinação - R^2)
# Ajuda a avaliar o nível de precisão do nosso modelo. 
# Quanto maior, melhor, sendo 1 o valor ideal.

# F-statistics
# É o teste F do modelo. Esse teste obtém os parâmetros do nosso modelo 
# e compara com um modelo que tenha menos parâmetros.
# Em teoria, um modelo com mais parâmetros tem um desempenho melhor. 

# Se o seu modelo com mais parâmetros NÃO tiver perfomance
# melhor que um modelo com menos parâmetros, o valor do p-value será bem alto. 

# Se o modelo com mais parâmetros tiver performance
# melhor que um modelo com menos parâmetros, o valor do p-value será mais baixo.

# Lembre-se que correlação não implica causalidade


# Prevendo as notas de matemática com Dados de teste----

# Etapa 5: Otimizando a Performance do Modelo

# Adicionando uma variável com o dobro do valor das idades
treinando$idade2 <- treinando$NU_IDADE ^ 2

# Criando o modelo final
modelo_v2 <- lm(NU_NOTA_MT ~ . * idade2 * TP_SEXO * regioes * TP_DEPENDENCIA_ADM_ESC * 
                  TP_ESCOLA + TP_SEXO * IN_SABATISTA * NU_NOTA_CN * NU_NOTA_CH * NU_NOTA_LC * TP_STATUS_REDACAO * NU_NOTA_COMP2 * regioes, data = treinando)

summary(modelo_v2)

# Dados de teste
glimpse(treinando)
notateste <- read.csv("test.csv")
View(notateste)
previsao <- predict(modelo, notateste)
class(previsao)
View(previsao)

