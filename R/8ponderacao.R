# Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
# Capítulo 8
# 8.2.1. Script para estimar o efeito causal por meio da ponderação pelo inverso da probabilidade de seleção no R
# página 151-154

# Instalando pacote survey
install.packages("survey")

# Construindo o banco de dados
psf<- rbind (c(1,1,0.4), c(1,0,0.20), c(1,1,0.30), c(1,1,0.45), c(1,1,0.40), c(0,0,0.30), c(0,0,0.40), c(0,1,0.50), c(0,0,0.10), c(0,0,0.15)) 
psf<-as.data.frame (psf) 
names (psf) <-c("psf", "pobreza", "vac")

# Obtendo listagem do banco de dados
psf

# Calculando a média da vacinação segundo participação no PSF
tapply(psf$vac, psf$psf, summary)

# Rodando regressão logística com tratamento e confundidor
# Estimando o escore de propensão
psflog<-glm(psf ~ pobreza, family="binomial", data=psf)
summary(psflog)

# Calculando e incluindo o escore de propensão no banco de dados
psf$ep <- predict(psflog,type="response")

# Listagem do banco de dados com o escore de propensão
list(psf)

# Ponderação com escore de propensão
# calculando os pesos 
# para o grupo de tratamento = 1/ep
# para o grupo controle= 1(1-ep)
psf$peso <- ifelse(psf$psf == 1, 1/psf$ep, 1/(1-psf$ep))

# Listagem do banco de dados com o peso
list(psf)

# Cálculo do efeito causal em modelo explicativo
# ponderado pelo inverso da probabilidade de seleção
# carregando pacote survey
library(survey)
pslog <- svydesign(ids=~1,weights=~peso, data=psf)
reg <- svyglm(vac ~ psf, design=pslog)
summary(reg)
