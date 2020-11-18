# Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
# Capítulo 12
# 12.3. Cálculo da computação G paramétrica no R
# páginas 259-264


# Construindo o banco de dados
esf<- rbind (c(1,1,0.4), c(1,0,0.20), c(1,1,0.30), c(1,1,0.45), c(1,1,0.40), c(0,0,0.30), c(0,0,0.40), c(0,1,0.50), c(0,0,0.10), c(0,0,0.15)) 
esf<-as.data.frame (esf) 
names (esf) <-c("esf", "pobreza", "vac")

# Obtendo listagem do banco de dados
esf

# Calculando a média da vacinação segundo participação na ESF
tapply(esf$vac, esf$esf, summary)

# Rodando regressão linear com tratamento e desfecho
reg<-lm(vac~ esf, data=esf)
summary(reg)
# Obtendo intervalo de confiança de 95%
confint(reg)

# Rodando regressão linear com tratamento e confundidor incluindo interação entre tratamento e confundidor
# Estimando o percentual de vacinação infantil incompleta a partir das variáveis do modelo
reg<-lm(vac~ esf*pobreza, data=esf)
summary(reg)
# Obtendo intervalo de confiança de 95%
confint(reg)

# Copiando dados
esfexp <- esfnexp <- esf

# Recodificando esf=1 em esfexp e esf=0 em esfnexp
esfexp$esf <-1
esfnexp$esf <-0

# Verificando se a recodificação foi realizada corretamente
library(descr)
freq(esf$esf)
freq(esfexp$esf)
freq(esfnexp$esf)


# Usando a regressão para predizer as respostas potenciais se todos estivessem expostos
predict.resp.exp <-predict(reg, newdata=esfexp, type= "response")
predict.resp.exp
# Gravando as respostas potenciais no banco de dados
esfexp$resppot <- predict.resp.exp 

# Usando a regressão para predizer as respostas potenciais se todos não estivessem expostos
predict.resp.nexp <-predict(reg, newdata=esfnexp, type= "response")
predict.resp.nexp
# Gravando as respostas potenciais no banco de dados
esfnexp$resppot <- predict.resp.nexp 

# Calculando a média das respostas potenciais se todos estivessem expostos
exp<-mean(predict.resp.exp)
exp
# Calculando a média das respostas potenciais se todos não estivessem expostos
nexp<-mean(predict.resp.nexp)
nexp

# Calculando efeito causal - diferença entre as médias das respostas potenciais
compgd<-exp-nexp
compgd

# Juntando os dois bancos de dados
esf2=rbind(esfexp, esfnexp)

# Obtendo o efeito causal pela regressão linear - regredindo esf nas respostas potenciais
reg<-lm(resppot~ esf, data=esf2)
summary(reg)

# Intervalo de confiança deve ser calculado por bootstrap
confint(reg)
