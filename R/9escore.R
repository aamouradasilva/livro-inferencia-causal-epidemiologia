# Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
# Capítulo 9
# 9.2.1. Script para estimar o escore de propensão por meio de regressão logística no R
# página 182

# Construindo o banco de dados
psf<- rbind (c(1,0.25,2), c(1,1,4), c(1,0.75,6), c(1,0.50,2), c(1,0.50,8), c(0,1,3), c(0,0.75,7), c(0,0.50,5), c(0,2,10), c(0,3,15)) 
psf<-as.data.frame (psf) 
names (psf) <-c("psf", "renda", "escolaridade")
  
# Modelo de regressão logística
psflog<-glm(psf ~ renda + escolaridade, family=binomial(), data=psf)
summary(psflog)

# Incluindo o escore de propensão no banco de dados
psf$ep <- predict(psflog,type="response")

# Listagem do banco de dados com o escore de propensão
list(psf)