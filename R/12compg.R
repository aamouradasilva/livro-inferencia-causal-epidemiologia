# Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
# Capítulo 12
# 12.3. Cálculo da computação G paramétrica no R
# páginas 259-264


# Construindo o banco de dados
psf<- rbind (c(1,1,0.4), c(1,0,0.20), c(1,1,0.30), c(1,1,0.45), c(1,1,0.40), c(0,0,0.30), c(0,0,0.40), c(0,1,0.50), c(0,0,0.10), c(0,0,0.15)) 
psf<-as.data.frame (psf) 
names (psf) <-c("psf", "pobreza", "vac")

# Obtendo listagem do banco de dados
psf

# Calculando a média da vacinação segundo participação no PSF
tapply(psf$vac, psf$psf, summary)

# Rodando regressão linear com tratamento e desfecho
reg<-lm(vac~ psf, data=psf)
summary(reg)
# Obtendo intervalo de confiança de 95%
confint(reg)

# Rodando regressão linear com tratamento e confundidor incluindo interação entre tratamento e confundidor
# Estimando o percentual de vacinação infantil incompleta a partir das variáveis do modelo
reg<-lm(vac~ psf*pobreza, data=psf)
summary(reg)
# Obtendo intervalo de confiança de 95%
confint(reg)

# Copiando dados
psfexp <- psfnexp <- psf

# Recodificando psf=1 em psfexp e psf=0 em psfnexp
psfexp$psf <-1
psfnexp$psf <-0

# Verificando se a recodificação foi realizada corretamente
library(descr)
freq(psf$psf)
freq(psfexp$psf)
freq(psfnexp$psf)


# Usando a regressão para predizer as respostas potenciais se todos estivessem expostos
predict.resp.exp <-predict(reg, newdata=psfexp, type= "response")
predict.resp.exp
# Gravando as respostas potenciais no banco de dados
psfexp$resppot <- predict.resp.exp 

# Usando a regressão para predizer as respostas potenciais se todos não estivessem expostos
predict.resp.nexp <-predict(reg, newdata=psfnexp, type= "response")
predict.resp.nexp
# Gravando as respostas potenciais no banco de dados
psfnexp$resppot <- predict.resp.nexp 

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
psf2=rbind(psfexp, psfnexp)

# Obtendo o efeito causal pela regressão linear - regredindo psf nas respostas potenciais
reg<-lm(resppot~ psf, data=psf2)
summary(reg)

# Intervalo de confiança deve ser calculado por bootstrap
confint(reg)
