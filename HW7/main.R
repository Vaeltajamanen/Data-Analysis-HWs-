alibrary(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(h2o)
library("car")
library(ggthemes)
source("unbalanced_functions.R")
death = read.csv("../data/murder_suicide.csv")


#1----------------

death %>% select(
  ActivityCode,
  DayOfWeekOfDeath,
  Education2003Revision,
  HispanicOriginRaceRecode,
  PlaceOfInjury,
  MaritalStatus,
  PlaceOfDeathAndDecedentsStatus,
  MannerOfDeath,
  Race ,
  Sex,
  Age,
  MethodOfDisposition
) -> death_data_selected

death_data_selected %>% mutate(
  BinaryGender = ifelse(Sex == "M", 0 , 1),
  ActivityCode = ifelse(ActivityCode == 99, mean(1:9), ActivityCode)  ,
  DayOfWeekOfDeath = ifelse(DayOfWeekOfDeath == 9 , mean(1:7) , DayOfWeekOfDeath),
  Education2003Revision = ifelse(Education2003Revision == 9 , mean(1:8), Education2003Revision),
  HispanicOriginRaceRecode = ifelse(
    HispanicOriginRaceRecode == 9 ,
    mean(1:8),
    HispanicOriginRaceRecode
  ),
  PlaceOfInjury = ifelse(PlaceOfInjury == 99 , mean(1:9), PlaceOfInjury),
  MannerOfDeath = ifelse(MannerOfDeath == 0 , mean(1:7), MannerOfDeath) ,
  Race = ifelse(Race >= 18, (Race - 18) / 10 + 9, Race)
) -> death_data_selected

death_data_selected$factorMaritualStatus = as.factor(death_data_selected$MaritalStatus)

death_data_selected$factorMaritualStatus = as.integer(death_data_selected$factorMaritualStatus)

death_data_selected$MethodOfDisposition = as.integer(as.factor(death_data_selected$MethodOfDisposition))

Filter(is.numeric, death_data_selected) -> death_data_selected


cor(death_data_selected,
    method =  c("pearson", "kendall", "spearman")) -> death_data_correlation


corrplot::corrplot(death_data_correlation,
                   order = "hclust",
                   tl.col = "black", )

scatterplotMatrix(death_data_correlation, col = "purple")

death_data_selected %>% mutate(isSuicide = ifelse(MannerOfDeath == 2 , 1 , 0)) -> death_data_selected
#2----------------


#Gender
cor.test(
  death_data_selected$BinaryGender,
  death_data_selected$isSuicide ,
  method = c("pearson", "kendall", "spearman")
)

chisq.test(death_data_selected$BinaryGender, death_data_selected$isSuicide)

#Race

cor.test(
  death_data_selected$isSuicide,
  death_data_selected$Race,
  method = c("pearson", "kendall", "spearman")
)

chisq.test(death_data_selected$Race, death_data_selected$isSuicide)
#Education

cor.test(
  death_data_selected$Education2003Revision,
  death_data_selected$isSuicide,
  method = c("pearson", "kendall", "spearman")
)
chisq.test(death_data_selected$Education2003Revision, death_data_selected$isSuicide)

#Age
cor.test(
  death_data_selected$Age,
  death_data_selected$isSuicide,
  method = c("pearson", "kendall", "spearman")
)
t.test(death_data_selected$Age, death_data_selected$isSuicide)


#disposition
cor.test(
  death_data_selected$MethodOfDisposition,
  death_data_selected$isSuicide,
  method = c("pearson", "kendall", "spearman")
)
chisq.test(death_data_selected$MethodOfDisposition, death_data_selected$isSuicide)

#3-----------------

glm(
  data = death_data_selected,
  isSuicide ~ Race + Age + BinaryGender + MethodOfDisposition + Education2003Revision ,
  family = "binomial"
) -> model

summary(model)

summary(glm(data = death_data_selected,
    isSuicide ~ Race + Age + DayOfWeekOfDeath + MethodOfDisposition + Education2003Revision ,
    family = "binomial"))


glm(
  data = death_data_selected,
  isSuicide ~ ActivityCode +  Race + Age + MethodOfDisposition + Education2003Revision + PlaceOfInjury +
    PlaceOfDeathAndDecedentsStatus + factorMaritualStatus,
  family = "binomial"
) -> model

summary(model)

glm.diag.plots(model, glmdiag = glm.diag(model))
#4--------------------
death_data_selected %>% mutate(pred = fitted(model), type = 'response') -> death_data_selected
ggplot(data = death_data_selected, aes(x = pred, color = as.factor(isSuicide))) + geom_density(size = 1.5) + theme_hc(bgcolor = "darkunica") +
  scale_fill_hc("darkunica")

death_data_selected %>% group_by(isSuicide) %>%
  summarise(mean = mean(as.numeric(pred))) -> prediction_ratio

ggplot(data = prediction_ratio, aes(x = as.factor(isSuicide), y = (mean))) + 
  geom_histogram( stat = "identity",aes(fill = as.factor(isSuicide))) + xlab("is suicide") + ylab("mean of prediction") + theme_hc(bgcolor = "darkunica") +
  scale_fill_hc("darkunica")
  

ggplot(
  data = death_data_selected,
  aes(
    x = ActivityCode * model$coefficients[2] +  Race * model$coefficients[3] + Age * model$coefficients[4] +
      MethodOfDisposition  * model$coefficients[5] + Education2003Revision * model$coefficients[6] +
      PlaceOfInjury * model$coefficients[7] +
      PlaceOfDeathAndDecedentsStatus * model$coefficients[8] + factorMaritualStatus * model$coefficients[9],
    y = isSuicide
  )
) + geom_point(color = "yellow") +
  geom_line(
    aes(
      x = ActivityCode * model$coefficients[2] +  Race * model$coefficients[3] + Age * model$coefficients[4] +
        MethodOfDisposition  * model$coefficients[5] + Education2003Revision * model$coefficients[6] +
        PlaceOfInjury * model$coefficients[7] +
        PlaceOfDeathAndDecedentsStatus * model$coefficients[8] + factorMaritualStatus * model$coefficients[9],
      y = pred
    ) , color = "blue" , size = 2
  ) + xlab("linear model") + theme_hc(bgcolor = "darkunica") +
  scale_fill_hc("darkunica")

#5--------------------
index = sample(1:nrow(death_data_selected), size = nrow(death_data_selected) * 0.8 , replace = F)
train = death_data_selected[index,]
test = death_data_selected[-index,]

glm(
  data = train,
  isSuicide ~ ActivityCode +  Race + Age + MethodOfDisposition + Education2003Revision + PlaceOfInjury +
    PlaceOfDeathAndDecedentsStatus + factorMaritualStatus,
  family = "binomial"
) -> new_model


test$prediction = predict(new_model, newdata = test, type = "response", na.action = na.pass)

test$result <- ifelse(test$prediction > 0.5 , 1 , 0)

positiveSamples <- sum(test$result)

negativeSamples = nrow(test) - positiveSamples

truePositive = length(test$result[test$result == test$isSuicide & test$result == 1])

trueNegative = length(test$result[test$result == test$isSuicide & test$result == 0])

falsePositive = length(test$result[test$result == 0 & test$isSuicide == 1])

falseNegative = length(test$result[test$result == 1 & test$isSuicide == 0])

Accuracy = (trueNegative + truePositive) / nrow(test) #not sure

falsePositiveRate = 1 - (trueNegative / negativeSamples)

truePositiveRate = truePositive / positiveSamples

cm_info <-
  ConfusionMatrixInfo(
    data = test,
    predict = "prediction",
    actual = "isSuicide",
    cutoff = 0.5
  )
cm_info$plot
 #6--------------------
train$prediction = predict(new_model, newdata = train , type = "response", na.action = na.pass)

accuracy_info = AccuracyCutoffInfo( train = train, test = test, 
                                    predict = "prediction", actual = "isSuicide" )
accuracy_info$plot

cut_off <- seq(0.3, 0.8, by = 0.01)

cutOff_accuracy <- data.frame(cutoff = cut_off, accuracy = vector(mode = "integer" , length = 51))

for(i in 1: length(cut_off)) {
  cutOff_accuracy[i, 2] <- Accuracy_calcualtor(test, cut_off[i])
}

cutOff_accuracy %>% 
  hchart(hcaes(x = cutoff, y = accuracy) , type = "line")%>%
  hc_add_theme(hc_theme_monokai())

print(cutOff_accuracy[which(cutOff_accuracy$accuracy == max(cutOff_accuracy$accuracy)), 1])
Accuracy_calcualtor <- function(test, cutoff) {
  test$result  = ifelse(test$pred > cutoff, 1, 0)
  truePositive = length(test$result[test$result == test$isSuicide & test$result == 1])
  
  trueNegative = length(test$result[test$result == test$isSuicide & test$result == 0])
  
  acc = (trueNegative + truePositive) / nrow(test) #not sure
  return(acc)
}

#7------------------

cost_fp = 100; cost_fn = 200
roc_info = ROCInfo( data = cm_info$data, predict = "predict", 
                    actual = "actual", cost.fp = cost_fp, cost.fn = cost_fn )
grid.draw(roc_info$plot)


#8--------------
h2o.init()
hsuicide = as.h2o(death_data_selected)
chglm = h2o.glm(y = "isSuicide", x= c("ActivityCode" ,  "Race"  ,"Age" , 
                                      "MethodOfDisposition", "Education2003Revision",  "PlaceOfInjury" ,
                                         "PlaceOfDeathAndDecedentsStatus" ,"factorMaritualStatus"),
                training_frame = hsuicide, family="binomial",nfolds = 5)
chglm





