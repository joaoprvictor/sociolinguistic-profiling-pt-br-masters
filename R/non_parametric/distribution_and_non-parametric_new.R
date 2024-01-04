rm(list=ls())
library(tidyverse) # for data wrangling
library(tidytext) # for data wrangling

# reading data 
data = read.csv('C:/Users/joaop/Desktop/Corpora/C-Oral-Brasil/dissertation_codes/text_output_files/metadataWithTranscription_csv_2024-01-03.csv')
data$age[data$age=="M"] = "A"
data$age[data$age=="D"] = "C"

# putting social variables as factors
age = as.factor(data$age)
sex = as.factor(data$sex)
schooling = as.factor(data$schooling)

names(data) #colums

# response variable
resposta = data$total_count_negation

dados = data.frame(resposta,age)

#A e B
tirar = "C"
wilcox.test(dados$resposta[dados$age!=tirar]~age[dados$age!=tirar]) 
#A e C
tirar = "B"
wilcox.test(dados$resposta[dados$age!=tirar]~age[dados$age!=tirar]) 
#B e C
tirar = "A"
wilcox.test(dados$resposta[dados$age!=tirar]~age[dados$age!=tirar]) 

# response variable
resposta = data$total_nonstandard_verb_agreement

dados = data.frame(resposta,age)

#A e B
tirar = "C"
wilcox.test(dados$resposta[dados$age!=tirar]~age[dados$age!=tirar]) 
#A e C
tirar = "B"
wilcox.test(dados$resposta[dados$age!=tirar]~age[dados$age!=tirar]) 
#B e C
tirar = "A"
wilcox.test(dados$resposta[dados$age!=tirar]~age[dados$age!=tirar]) 


analise_age = function(dados,comparacoes=3){
  
  #A e B
  tirar = "C"
  analise = wilcox.test(dados$resposta[dados$age!=tirar]~age[dados$age!=tirar])
  pvalorAB = analise$p.value
  resultadoAB = ifelse(pvalorAB<(0.05/comparacoes),"Diferente",'Igual')

  #A e C
  tirar = "B"
  analise = wilcox.test(dados$resposta[dados$age!=tirar]~age[dados$age!=tirar])
  pvalorAC = analise$p.value
  resultadoAC = ifelse(pvalorAC<(0.05/comparacoes),"Diferente",'Igual')
  
  #B e C
  tirar = "A"
  analise = wilcox.test(dados$resposta[dados$age!=tirar]~age[dados$age!=tirar])
  pvalorBC = analise$p.value
  resultadoBC = ifelse(pvalorBC<(0.05/comparacoes),"Diferente",'Igual')
  
  return(list(pvalorAB=pvalorAB,resultadoAB=resultadoAB,pvalorAC=pvalorAC,resultadoAC=resultadoAC,pvalorBC=pvalorBC,resultadoBC=resultadoBC))
  
}

# response variable
resposta = data$total_count_negation
dados = data.frame(resposta,age)
analise_age(dados)

boxplot(resposta~age)
# response variable
resposta = data$total_nonstandard_verb_agreement
dados = data.frame(resposta,age)
analise_age(dados)

# response variable
resposta = data$total_nonstandard_verb_conjugation
dados = data.frame(resposta,age)
analise_age(dados)

# response variable
resposta = data$total_count_negation
dados = data.frame(resposta,age)
analise_age(dados)

# response variable
resposta = data$total_non_standard_plural
dados = data.frame(resposta,age)
analise_age(dados)

# response variable
resposta = data$count_apheresis
dados = data.frame(resposta,age)
analise_age(dados)

# response variable
resposta = data$count_rhotacism
dados = data.frame(resposta,age)
analise_age(dados)

# response variable
resposta = data$count_senhor_senhora
dados = data.frame(resposta,age)
analise_age(dados)

# response variable
resposta = data$count_diminutive
dados = data.frame(resposta,age)
analise_age(dados)

# response variable
resposta = data$count_foreign_words
dados = data.frame(resposta,age)
analise_age(dados)

kruskal.test(resposta~age)

# response variable
resposta = data$count_prepositions
dados = data.frame(resposta,age)
analise_age(dados)

# response variable
resposta = data$count_pronouns
dados = data.frame(resposta,age)
analise_age(dados)

# response variable
resposta = data$count_interjections
dados = data.frame(resposta,age)
analise_age(dados)



analise_escolaridade = function(dados,comparacoes=3){
  
  #A e B
  tirar = 3
  analise = wilcox.test(dados$resposta[dados$schooling!=tirar]~schooling[dados$schooling!=tirar])
  pvalorAB = analise$p.value
  resultadoAB = ifelse(pvalorAB<(0.05/comparacoes),"Diferente",'Igual')
  
  #A e C
  tirar = 2
  analise = wilcox.test(dados$resposta[dados$schooling!=tirar]~schooling[dados$schooling!=tirar])
  pvalorAC = analise$p.value
  resultadoAC = ifelse(pvalorAC<(0.05/comparacoes),"Diferente",'Igual')
  
  #B e C
  tirar = 1
  analise = wilcox.test(dados$resposta[dados$schooling!=tirar]~schooling[dados$schooling!=tirar])
  pvalorBC = analise$p.value
  resultadoBC = ifelse(pvalorBC<(0.05/comparacoes),"Diferente",'Igual')
  
  return(list(pvalorAB=pvalorAB,resultadoAB=resultadoAB,pvalorAC=pvalorAC,resultadoAC=resultadoAC,pvalorBC=pvalorBC,resultadoBC=resultadoBC))
  
}

# response variable
resposta = data$total_count_negation
dados = data.frame(resposta,schooling)
analise_escolaridade(dados)

# response variable
resposta = data$total_nonstandard_verb_agreement
dados = data.frame(resposta,schooling)
analise_escolaridade(dados)

# response variable
resposta = data$total_nonstandard_verb_conjugation
dados = data.frame(resposta,schooling)
analise_escolaridade(dados)

# response variable
resposta = data$total_count_negation
dados = data.frame(resposta,schooling)
analise_escolaridade(dados)

# response variable
resposta = data$total_non_standard_plural
dados = data.frame(resposta,schooling)
analise_escolaridade(dados)

# response variable
resposta = data$count_apheresis
dados = data.frame(resposta,schooling)
analise_escolaridade(dados)

# response variable
resposta = data$count_rhotacism
dados = data.frame(resposta,schooling)
analise_escolaridade(dados)

# response variable
resposta = data$count_senhor_senhora
dados = data.frame(resposta,schooling)
analise_escolaridade(dados)

# response variable
resposta = data$count_diminutive
dados = data.frame(resposta,schooling)
analise_escolaridade(dados)

# response variable
resposta = data$count_foreign_words
dados = data.frame(resposta,schooling)
analise_escolaridade(dados)

# response variable
resposta = data$count_prepositions
dados = data.frame(resposta,schooling)
analise_escolaridade(dados)

# response variable
resposta = data$count_pronouns
dados = data.frame(resposta,schooling)
analise_escolaridade(dados)

# response variable
resposta = data$count_interjections
dados = data.frame(resposta,schooling)
analise_escolaridade(dados)


#sexo
# response variable
resposta = data$total_count_negation
wilcox.test(resposta~sex)

# response variable
resposta = data$total_nonstandard_verb_agreement
wilcox.test(resposta~sex)

# response variable
resposta = data$total_nonstandard_verb_conjugation
wilcox.test(resposta~sex)

# response variable
resposta = data$total_count_negation
wilcox.test(resposta~sex)

# response variable
resposta = data$total_non_standard_plural
wilcox.test(resposta~sex)

# response variable
resposta = data$count_apheresis
wilcox.test(resposta~sex)

# response variable
resposta = data$count_rhotacism
wilcox.test(resposta~sex)

# response variable
resposta = data$count_senhor_senhora
wilcox.test(resposta~sex)

# response variable
resposta = data$count_diminutive
wilcox.test(resposta~sex)

# response variable
resposta = data$count_foreign_words
wilcox.test(resposta~sex)

# response variable
resposta = data$count_prepositions
wilcox.test(resposta~sex)

# response variable
resposta = data$count_pronouns
wilcox.test(resposta~sex)

# response variable
resposta = data$count_interjections
wilcox.test(resposta~sex)
