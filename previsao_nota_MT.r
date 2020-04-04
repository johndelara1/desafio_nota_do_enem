library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(doParallel)
replace_na <- function(x){
  x <- ifelse(is.na(x), -3, x)
  
  return(x)
}
rmse <- function(y, y_hat){
  e <- y - y_hat
  mse <- mean(e^2)
  rmse <- sqrt(mse)
  
  return(rmse)
}

#####################################
########### Configurações ###########
#####################################
# cl <- makePSOCKcluster(8)
# registerDoParallel(cl)

###########################
########### ETL ###########
###########################
train <- 
  read_csv(file = 'Dados/train.csv') %>% 
  filter(!is.na(NU_NOTA_MT)) %>% 
  mutate_all(replace_na)

y <- train$NU_NOTA_MT
X <- 
  train %>% 
  select(NU_IDADE, TP_SEXO, TP_COR_RACA, TP_ST_CONCLUSAO, NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, Q001, Q002, Q006, Q024, Q025, Q026, Q047) %>% 
  mutate(TP_COR_RACA = factor(TP_COR_RACA), TP_ST_CONCLUSAO = factor(TP_ST_CONCLUSAO)) %>% mutate_if(is.character, as.factor)
ux <- X %>% select(NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC) %>% colMeans()

test <- read_csv(file = 'Dados/test.csv')
X_test <- 
  test %>% 
  select(NU_IDADE, TP_SEXO, TP_COR_RACA, TP_ST_CONCLUSAO, NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, Q001, Q002, Q006, Q024, Q025, Q026, Q047) %>% 
  mutate(TP_COR_RACA = factor(TP_COR_RACA), TP_ST_CONCLUSAO = factor(TP_ST_CONCLUSAO)) %>% mutate_if(is.character, as.factor) %>% 
  mutate(NU_NOTA_CH = ifelse(is.na(NU_NOTA_CH), ux['NU_NOTA_CH'], NU_NOTA_CH), 
         NU_NOTA_CN = ifelse(is.na(NU_NOTA_CN), ux['NU_NOTA_CN'], NU_NOTA_CN), 
         NU_NOTA_LC = ifelse(is.na(NU_NOTA_LC), ux['NU_NOTA_LC'], NU_NOTA_LC))

###########################
########### AED ###########
###########################
summary(y)
hist(y)

nomes <- X_test %>% colnames
p <- list()
for(i in nomes){
  dummy <-  data.frame(x = X[, i, T], y = y)
  p[[i]] <- 
    ggplot(data = dummy, mapping = aes(x = x, y = y)) +
    geom_point() +
    labs(title = i)
}
pdf(file = 'train.pdf')
p
dev.off()


#################################
########### Modelagem ###########
#################################
train_set <- cbind(X, y_t = y)

fit_lm <- lm(y_t ~ ., data = train_set)
rmse(y, fitted(fit_lm)) %>% data.frame(model = 'lm') %>% write_csv(path = 'metrics.csv', append = T)
data.frame(NU_INSCRICAO = test$NU_INSCRICAO, NU_NOTA_MT = predict(fit_lm, X_test)) %>% write_csv(path = 'lm_predict.csv')


fit1 <- train(y_t ~.,
              data = train_set,
              method = 'treebag',
              preProcess = c("center", "scale"),
              tuneLength = 10,
              trControl = trainControl(method = "cv", search = 'random'))
rmse(y, fitted(fit1)) %>% data.frame(model = 'treebag') %>% write_csv(path = 'metrics.csv', append = T)
data.frame(NU_INSCRICAO = test$NU_INSCRICAO, NU_NOTA_MT = predict(fit1, X_test)) %>% write_csv(path = 'fit1_predict.csv')

fit2 <- train(y_t ~.,
              data = train_set,
              method = 'bagEarth',
              preProcess = c("center", "scale"),
              tuneLength = 10,
              trControl = trainControl(method = "cv", search = 'random'))
rmse(y, fitted(fit2)) %>% data.frame(model = 'bagEarth') %>% write_csv(path = 'metrics.csv', append = T)
data.frame(NU_INSCRICAO = test$NU_INSCRICAO, NU_NOTA_MT = predict(fit2, X_test)) %>% write_csv(path = 'fit2_predict.csv')

fit3 <- train(y_t ~.,
              data = train_set,
              method = 'ranger',
              preProcess = c("center", "scale"),
              tuneLength = 10,
              trControl = trainControl(method = "cv", search = 'random'))
rmse(y, fitted(fit3)) %>% data.frame(model = 'ranger') %>% write_csv(path = 'metrics.csv', append = T)
data.frame(NU_INSCRICAO = test$NU_INSCRICAO, NU_NOTA_MT = predict(fit3, X_test)) %>% write_csv(path = 'fit3_predict.csv')

fit4 <- train(y_t ~.,
              data = train_set,
              method = 'gamboost',
              preProcess = c("center", "scale"),
              tuneLength = 10,
              trControl = trainControl(method = "cv", search = 'random'))
rmse(y, fitted(fit4)) %>% data.frame(model = 'gamboost') %>% write_csv(path = 'metrics.csv', append = T)
data.frame(NU_INSCRICAO = test$NU_INSCRICAO, NU_NOTA_MT = predict(fit4, X_test)) %>% write_csv(path = 'fit4_predict.csv')

fit5 <- train(y_t ~.,
              data = train_set,
              method = 'gaussprLinear',
              preProcess = c("center", "scale"),
              tuneLength = 10,
              trControl = trainControl(method = "cv", search = 'random'))
rmse(y, fitted(fit5)) %>% data.frame(model = 'gaussprLinear') %>% write_csv(path = 'metrics.csv', append = T)
data.frame(NU_INSCRICAO = test$NU_INSCRICAO, NU_NOTA_MT = predict(fit5, X_test)) %>% write_csv(path = 'fit5_predict.csv')

fit6 <- train(y_t ~.,
              data = train_set,
              method = 'earth',
              preProcess = c("center", "scale"),
              tuneLength = 10,
              trControl = trainControl(method = "cv", search = 'random'))
rmse(y, fitted(fit6)) %>% data.frame(model = 'earth') %>% write_csv(path = 'metrics.csv', append = T)
data.frame(NU_INSCRICAO = test$NU_INSCRICAO, NU_NOTA_MT = predict(fit6, X_test)) %>% write_csv(path = 'fit6_predict.csv')

fit7 <- train(y_t ~.,
              data = train_set,
              method = 'xgbTree',
              preProcess = c("center", "scale"),
              tuneLength = 10,
              trControl = trainControl(method = "cv", search = 'random'))
rmse(y, fitted(fit7)) %>% data.frame(model = 'xgbTree') %>% write_csv(path = 'metrics.csv', append = T)
data.frame(NU_INSCRICAO = test$NU_INSCRICAO, NU_NOTA_MT = predict(fit7, X_test)) %>% write_csv(path = 'fit7_predict.csv')