# Neste projeto, o objetivo é utilizar dados de acelerômetros na cintura, antebraço e braço 
# de 6 participantes. Eles foram solicitados a executar exercícios corretamente e incorretamente 
# em 5 maneiras diferentes. 

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Definindo o diretório de trabalho
getwd()
setwd("C:/Users/Matilde/Dropbox/DESAFIO-Codenation(ENEM)/desafio_nota_do_enem")

# Vamos analisar os dados e gerar um modelo preditivo de desempenho que vai prever a 
# probabilidade de um dos 5 possíveis resultados do teste: A, B, C, D, ou E

# Modelo preditivo com XGBoost

# OpenMP

# Dataset
# http://groupware.les.inf.puc-rio.br/har

# Instalando e carregando os pacotes
#install.packages("caret")
#install.packages("corrplot")
#install.packages("Rtsne")
#install.packages("xgboost")
#install.packages("knitr")
#install.packages("ggplot2")
#install.packages("e1071")
#install.packages("Ckmeans.1d.dp")

require(caret)
require(corrplot)
require(Rtsne)
require(xgboost)
require(stats)
require(knitr)
require(ggplot2)
require(e1071)
require(Ckmeans.1d.dp)

# Carregando os dados
test = read.csv("pml-training.csv")
train = read.csv("pml-testing.csv")

# Visualizando as dimensões
dim(train)
dim(test)
names(train)

# Variável de saída
outcome.org = train[, "NU_NOTA_MT"]
outcome = outcome.org 
levels(outcome)

# Convertendo os labesl para numéricos (requerimento do XGBoost)
num.class = length(levels(outcome))
levels(outcome) = 1:num.class
head(outcome)

train$classe = NULL

# Filtrando as colunas por belt, forearm, arm, dumbell
filter = grepl("belt|arm|dumbell", names(train))
train = train[, filter]
test = test[, filter]

# Removendo as colunas com NA
cols.without.na = colSums(is.na(test)) == 0
train = train[, cols.without.na]
test = test[, cols.without.na]

# Checando zero variância
zero.var = nearZeroVar(train, saveMetrics = TRUE)
zero.var

# Plot de Atributos x Labels
featurePlot(train, outcome.org, "strip")

# Plot de Correlação
corrplot.mixed(cor(train), 
               lower = "circle", 
               upper = "color", 
               tl.pos = "lt", 
               diag = "n", 
               order = "hclust", 
               hclust.method = "complete")


# Visualização em 2D de redução de dimensionalidade usando a função Rtsne

# t-Distributed Stochastic Neighbor Embedding
tsne = Rtsne(as.matrix(train), 
             check_duplicates = FALSE, 
             pca = TRUE, 
             perplexity = 30, 
             theta = 0.5, 
             dims = 2)

# Preparando as variáveis para o Plot
embedding = as.data.frame(tsne$Y)
embedding$Class = outcome.org

# Visualização o Plot com ggplot
g = ggplot(embedding, aes(x = V1, y = V2, color = Class)) +
  geom_point(size = 1.25) +
  guides(colour = guide_legend(override.aes = list(size = 6))) +
  xlab("") + ylab("") +
  ggtitle("t-SNE 2D Embedding of 'Classe' Outcome") +
  theme_light(base_size = 20) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())
print(g)


# Construindo um modelo de Machine Learning com XGBoost

# Convertendo os dados em Mztriz
train.matrix = as.matrix(train)
mode(train.matrix) = "numeric"
test.matrix = as.matrix(test)
mode(test.matrix) = "numeric"

# Convertendo de fator para Matriz
y = as.matrix(as.integer(outcome)-1)

# Lista de parâmetros XGBoost
param <- list("objective" = "multi:softprob",    # multiclass classification 
              "num_class" = num.class,           # number of classes 
              "eval_metric" = "merror",          # evaluation metric 
              "nthread" = 8,                     # number of threads to be used 
              "max_depth" = 16,                  # maximum depth of tree 
              "eta" = 0.3,                       # step size shrinkage 
              "gamma" = 0,                       # minimum loss reduction 
              "subsample" = 1,                   # part of data instances to grow tree 
              "colsample_bytree" = 1,            # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 12            # minimum sum of instance weight needed in a child 
)


# Set seed para reproducibilidade
set.seed(1234)

# k-fold cross validation
nround.cv = 200
system.time( bst.cv <- xgb.cv(param = param, 
                              data = train.matrix, 
                              label = y, 
                              nfold = 4, 
                              nrounds = nround.cv, 
                              prediction = TRUE, 
                              verbose = FALSE) )

### ALTERANDO ACESSO AO VALOR 
tail(bst.cv$evaluation_log) 

### ALTERANDO ACESSO AO VALOR 
# Índice do erro mínimo
min.merror.idx = which.min(bst.cv$evaluation_log[, test_merror_mean]) 
min.merror.idx 

# Erro mínimo
bst.cv$evaluation_log[min.merror.idx,]

# Obtendo previsões da CrossValidation
pred.cv = matrix(bst.cv$pred, nrow = length(bst.cv$pred)/num.class, ncol = num.class)
pred.cv = max.col(pred.cv, "last")

# Confusion matrix
confusionMatrix(factor(y+1), factor(pred.cv))

# Modelo final com todos os dados de treino
system.time( bst <- xgboost(param = param, 
                            data = train.matrix, 
                            label = y, 
                            nrounds = min.merror.idx, 
                            verbose = 0) )

# Previsões nos dados de teste
pred <- predict(bst, test.matrix)  
head(pred, 10)  

# Decode das previsões
pred = matrix(pred, nrow = num.class, ncol = length(pred)/num.class)
pred = t(pred)
pred = max.col(pred, "last")
pred.char = toupper(letters[pred])
pred.char

# Dump do modelo treinado
model = xgb.dump(bst, with.stats = TRUE)

# Obtendo nomes dos atributos
names = dimnames(train.matrix)[[2]]

# Gerando a matriz de importância dos atrobutos
importance_matrix = xgb.importance(names, model = bst)

# Plot
gp = xgb.plot.importance(importance_matrix)
print(gp) 







