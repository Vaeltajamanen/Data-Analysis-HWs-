library(readr)
library(highcharter)
library(ggplot2)
library(dplyr)
library(tidyr)
library(HistData)
library(Hmisc)
library(checkmate)
library(corrplot)
library("ggthemes")
library("scales")
library("car")
library(lawstat)

dic_var <- read_delim("dictionnaire_variables.csv", delim = ";")
house <- read_csv("train.csv")
dic_nvx <- read_csv("dictionnaire_niveaux.csv")

#function
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  = (cormat)[ut],
    p = pmat[ut]
  )
}


#1--------------------


Filter(is.numeric, house) -> numeric_house


cor(numeric_house, method = c("pearson", "kendall", "spearman")) -> house_corr


as.data.frame(house_corr) -> house_corr


as.matrix(numeric_house) -> num_house
rcorr(num_house) -> house_corr_2

as.data.frame(house_corr_2$r) -> correlations

as.data.frame(house_corr_2$P) -> pValues

flattenCorrMatrix(correlations, pValues) -> corr_and_pValues

as.matrix(correlations) -> cor_plot

corrplot(
  cor_plot,
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45
)


corr_and_pValues %>% filter(column == "SalePrice") -> price_correlation

price_correlation %>% arrange(desc(cor)) %>% head(10) -> price_high_cor


#2------------------

price_high_cor %>% select(row) -> rows

as.vector(rows) -> rows



strsplit(paste(unlist(rows), collapse = ' '), " ") -> rows_names

as.vector(rows_names[[1]]) -> rows_names

numeric_house %>% select(c(rows_names , "SalePrice")) -> price_high_data


price_high_data %>% gather(key = "parameter" , value = "amount",-SalePrice) -> price_high_data_gathered

pricePlot <-
  ggplot(data = price_high_data_gathered, aes(x = amount , y = SalePrice))
pricePlot + geom_point(aes(color = parameter)) + geom_smooth(method = "lm") + facet_grid( ~ parameter, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  theme_hc(bgcolor = "darkunica") +
  scale_fill_hc("darkunica")

rcorr(as.matrix(price_high_data)) -> price_high_data_correlations

as.data.frame(price_high_data_correlations$r) -> price_high_data_correlations
scatterplotMatrix(price_high_data_correlations)
#3--------------

fit = lm(
  data = numeric_house ,
  SalePrice ~  OverallQual + GrLivArea +
    GarageCars + GarageArea + TotalBsmtSF + `1stFlrSF` + FullBath + TotRmsAbvGrd + YearBuilt + YearRemodAdd
)
fit

summary(fit)

#4-------------

numeric_house %>% mutate(pricePredict = predict.lm(fit, type = "response")) -> numeric_house

numeric_house %>% hchart(hcaes(x = SalePrice, y = pricePredict) , type = "scatter")  %>%
  hc_add_series(
    name = 'Y = x',
    color = 'red',
    data = 0:280000
  ) %>%
  hc_add_theme(hc_theme_monokai())

numeric_house %>% mutate(error = (SalePrice - pricePredict) ^ 2) -> numeric_house


ggplot(data = numeric_house, aes(x = error)) + geom_histogram(aes(y = ..density..), fill = "blue") + geom_density(color = "pink") + xlim(0 , 50000000) +  theme_hc(bgcolor = "darkunica") +
  scale_fill_hc("darkunica")

#5---------------


summary(fit) -> sum_of_lm_fit

sum_of_lm_fit$r.squared
#The F value in regression is the result of a test where the null hypothesis is that all of the regression coefficients are equal to zero. In other words, the model has no predictive capability. Basically, the f-test compares your model with zero predictor variables (the intercept only model), and decides whether your added coefficients improved the model. If you get a significant result, then whatever coefficients you included in your model improved the model’s fit.

#Read your p-value first. If the p-value is small (less than your alpha level), you can accept the null hypothesis. Only then should you consider the f-value. If you fail to reject the null, discard the f-value result.

sum_of_lm_fit$fstatistic


#6----------------
numeric_house$MasVnrArea[is.na(numeric_house$MasVnrArea)] <-
  mean(numeric_house$MasVnrArea, na.rm = T)

price_correlation %>% arrange(p) %>% head(10) -> price_high_cor_2

price_high_cor_2 %>% select(row) -> rows

as.vector(rows) -> rows



strsplit(paste(unlist(rows), collapse = ' '), " ") -> rows_names

as.vector(rows_names[[1]]) -> rows_names


numeric_house %>% select(c(rows_names , "SalePrice")) -> price_high_data_2

price_high_data_2 %>% gather(key = "parameter" , value = "amount",-SalePrice) -> price_high_data_gathered_2

fit2 = lm(
  data = numeric_house ,
  SalePrice ~ LotArea + OverallQual + YearBuilt + YearRemodAdd +
    MasVnrArea + BsmtFinSF1 + BsmtUnfSF + TotalBsmtSF + `1stFlrSF`
)

fit2

summary(fit2)

numeric_house %>% mutate(predictPrice2 = fitted(fit2)) -> numeric_house

numeric_house %>% mutate(error2 = (SalePrice - predictPrice2) ^ 2) -> numeric_house
summary(fit2) -> sum_of_fit2
sum_of_fit2$r.squared
sum_of_fit2$fstatistic

numeric_house %>% hchart(hcaes(x = SalePrice, y = predictPrice2) , type = "scatter") %>%
  hc_add_theme(hc_theme_monokai())

ggplot(data = numeric_house, aes(x = error2)) + geom_histogram(aes(y = ..density..) , fill = "blue") +
  geom_density(aes(y = ..density..), color = "pink")  +
  xlim(0, 50000000)  + theme_hc(bgcolor = "darkunica") +
  scale_fill_hc("darkunica")
#6------------ using the first model

fit3 <-
  lm(
    data = numeric_house,
    SalePrice ~ OverallQual + GrLivArea + GarageCars + TotalBsmtSF + YearBuilt + YearRemodAdd + `1stFlrSF`
  )

summary(fit3) -> sum_of_fit3

sum_of_fit3$r.squared
sum_of_fit3$fstatistic

numeric_house %>% mutate(pricePredict3 = predict.lm(fit3, type = "response")) -> numeric_house

numeric_house %>% mutate(error3 = (SalePrice - pricePredict3) ^ 2) -> numeric_house

numeric_house %>% hchart(hcaes(x = SalePrice, y = pricePredict3), type = "scatter") %>% hc_add_theme(hc_theme_monokai())

ggplot(data = numeric_house, aes(x = error3)) + geom_histogram(aes(y = ..density..), fill = "blue") + geom_density(color = "pink") + xlim(0, 50000000) +
  theme_hc(bgcolor = "darkunica") +
  scale_fill_hc("darkunica")

#7---------------
#constant variance
par(mfrow=c(2,2))
plot(fit3)


#normality

car::qqPlot(fit3, id.method="identify",
            simulate = TRUE, main="Q-Q Plot")
numeric_house %>% filter(SalePrice <=0350000 ) -> numeric_house
fit3 <-
  lm(
    data = numeric_house,
    SalePrice ~ OverallQual + GrLivArea + GarageCars + TotalBsmtSF + YearBuilt + YearRemodAdd + `1stFlrSF`
  )
car::qqPlot(fit3, id.method="identify",
            simulate = TRUE, main="Q-Q Plot")

#independence
acf(fit3$residuals)

#ba tavajoh be p-value nemitoonim farze sefr ke barabar ba mostaghel boodan residual ha az hame rad konim
lawstat::runs.test(fit3$residuals)



#8--------------
index = sample(x = 1 : nrow(numeric_house), size = 0.8 * nrow(numeric_house), replace = F)

train <- numeric_house[index,]
test <- numeric_house[-index,]

fit_train <- lm(data = train,  SalePrice ~ OverallQual + GrLivArea + GarageCars + TotalBsmtSF + YearBuilt + YearRemodAdd + `1stFlrSF`)


train$prediction = predict(fit_train, newdata = train)
test$prediction = predict(fit_train, newdata = test)

test %>% hchart(hcaes(x = SalePrice, y = prediction), type = "scatter") %>% hc_add_theme(hc_theme_monokai())

test$error = (test$SalePrice - test$prediction) ^ 2

ggplot(data = test, aes(x = error)) + geom_histogram(aes(y = ..density..), fill = "blue") + geom_density(color = "pink") + xlim(0, 50000000) + 
  theme_hc(bgcolor = "darkunica") +
  scale_fill_hc("darkunica")


#9-----------------

fit4 <-  lm(data = train,  SalePrice ~ OverallQual + I(OverallQual ^ 3)+ I(OverallQual ^ 2) + GrLivArea + GarageCars + TotalBsmtSF+  I(TotalBsmtSF ^ 2) + YearBuilt + YearRemodAdd + `1stFlrSF`)
train$prediction = predict(fit4, newdata = train)
test$prediction = predict(fit4, newdata = test)

test %>% hchart(hcaes(x = SalePrice, y = prediction), type = "scatter") %>% hc_add_theme(hc_theme_monokai())
test$error = (test$SalePrice - test$prediction) ^ 2

ggplot(data = test, aes(x = error)) + geom_histogram(aes(y = ..density..), fill = "blue") + geom_density(color = "pink") + xlim(0, 50000000) + 
  theme_hc(bgcolor = "darkunica") +
  scale_fill_hc("darkunica")



#10---------------

house_test = read_csv("test.csv")

house_test$SalePricePrediciton = predict(fit4, newdata = house_test)

house_test %>% select(Id, SalePrice = SalePricePrediciton) -> final_result



final_result$SalePrice[is.na(final_result$SalePrice)] <- mean(final_result$SalePrice, na.rm = T)

write.csv(final_result, "final_result.csv", row.names = FALSE)
