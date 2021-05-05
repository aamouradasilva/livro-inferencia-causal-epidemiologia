# Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
# Capítulo 8
# 8.2.1. Script para estimar o efeito causal por meio da ponderação pelo inverso da probabilidade de seleção no R
# página 193-194

# arquivo salvo em UTF-8
# Se os caracteres acentuados e a cedilha não aparecerem corretamente
# No R Studio, escolha a opção File : Reopen with Encoding: UTF-8

# Instalando pacote survey
install.packages("survey")

# Construindo o banco de dados
esf<- rbind (c(1,1,0.4), c(1,0,0.20), c(1,1,0.30), c(1,1,0.45), c(1,1,0.40), c(0,0,0.30), c(0,0,0.40), c(0,1,0.50), c(0,0,0.10), c(0,0,0.15)) 
esf<-as.data.frame (esf) 
names (esf) <-c("esf", "pobreza", "vac")

# Obtendo listagem do banco de dados
esf

# Calculando a média da vacinação segundo participação no esf
tapply(esf$vac, esf$esf, summary)

# Rodando regressão logística com tratamento e confundidor
# Estimando o escore de propensão
esflog<-glm(esf ~ pobreza, family="binomial", data=esf)
summary(esflog)

# Calculando e incluindo o escore de propensão no banco de dados
esf$ep <- predict(esflog,type="response")

# Listagem do banco de dados com o escore de propensão
list(esf)

# Ponderação com escore de propensão
# calculando os pesos 
# para o grupo de tratamento = 1/ep
# para o grupo controle= 1(1-ep)
esf$peso <- ifelse(esf$esf == 1, 1/esf$ep, 1/(1-esf$ep))

# Listagem do banco de dados com o peso
list(esf)

# Cálculo do efeito causal em modelo explicativo
# ponderado pelo inverso da probabilidade de seleção
# carregando pacote survey
library(survey)
pslog <- svydesign(ids=~1,weights=~peso, data=esf)
reg <- svyglm(vac ~ esf, design=pslog)
summary(reg)
