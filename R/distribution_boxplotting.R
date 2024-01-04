rm(list=ls())
library(tidyverse) # for data wrangling
library(tidytext) # for data wrangling

# reading data 
data = read.csv('C:/Users/joaop/Desktop/Corpora/C-Oral-Brasil/dissertation_codes/text_output_files/metadataWithTranscription_csv_2023-11-05.csv')
data$age[data$age=="M"] = "A"
data$age[data$age=="D"] = "C"

# putting social variables as factors
age = as.factor(data$age)
sex = as.factor(data$sex)
schooling = as.factor(data$schooling)

names(data) #colums

# response variable
resposta = data$count_diminutive_ratio

rm(data)

#boxplotting

par(mfrow=c(1,3))
boxplot(resposta~sex,ylab="Non-standard negation forms",xlab = "Sex")
boxplot(resposta~age,ylab=" ",xlab = "Age")
boxplot(resposta~schooling,ylab=" ",xlab = "Schooling")
par(mfrow=c(1,1))
