# Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
# Capítulo 13
# 13. Exemplo de análise usando computação G paramétrica no R
# páginas 335-343

# arquivo salvo em UTF-8
# Se os caracteres acentuados e a cedilha não aparecerem corretamente
# No R Studio, escolha a opção File : Reopen with Encoding: UTF-8

# Instalando os pacotes necessários
install.packages("twang")
install.packages("descr")

# Carregando pacote twang
library(twang)

# Abrindo banco de dados - lalonde
data(lalonde)

# Calculando a média da renda em 1978 segundo participação no programa de retreinamento profissional
tapply(lalonde$re78, lalonde$treat, summary)

# Estimando associação em modelo de regressão não ajustado
reg <- lm(re78 ~ treat, data=lalonde)
summary(reg)
# Obtendo intervalo de confiança de 95%
confint(reg)

# Estimando associação em modelo de regressao ajustado sem interações ou termos quadráticos
reg <- lm(re78 ~ treat + age + educ + black + hispan + nodegree + married + re74 + re75, data=lalonde)
summary(reg)
# Obtendo intervalo de confiança de 95%
confint(reg)

# Rodando regressão linear com tratamento e confundidor incluindo interação entre tratamento e renda em 1974
# e termo quadrático para renda em 1974
reg <- lm(re78 ~ treat + age + educ + black + hispan + nodegree + married + re74 + treat:re74 + I(re74^2) + re75, data=lalonde)
summary(reg)
# Obtendo intervalo de confiança de 95%
confint(reg)

# Copiando dados
lalondeexp <- lalondenexp <- lalonde

# Recodificando treat=1 em lalondeexp e treat=0 em lalondenexp
lalondeexp$treat <-1
lalondenexp$treat <-0

# Verificando se a recodificação foi realizada corretamente
library(descr) 
freq(lalonde$treat)
freq(lalondeexp$treat)
freq(lalondenexp$treat)

# Usando a regressão para predizer as respostas potenciais se todos estivessem expostos
predict.resp.exp <-predict(reg, newdata=lalondeexp, type= "response")
head(predict.resp.exp)
# Gravando as respostas potenciais no banco de dados
lalondeexp$resppot <- predict.resp.exp 

# Usando a regressão para predizer as respostas potenciais se ninguém estivesse exposto
predict.resp.nexp <-predict(reg, newdata=lalondenexp, type= "response")
head(predict.resp.nexp)
# Gravando as respostas potenciais no banco de dados
lalondenexp$resppot <- predict.resp.nexp 

# Calculando a média das respostas potenciais se todos estivessem expostos
exp<-mean(predict.resp.exp)
exp

# Calculando a média das respostas potenciais se ninguém estivesse exposto
nexp<-mean(predict.resp.nexp)
nexp

# Calculando o efeito causal - diferença entre as médias das respostas potenciais
compgd<-exp-nexp
compgd

# Cálculo do erro padrão e do intervalo de confiança de 95% do efeito causal por boostrapping
# Fixando inicio casual
set.seed(1)

# Função para o cálculo do efeito causal pela computação G paramétrica
comp.g=function(lalonde,indices) 
{
  lalonde=lalonde[indices,] 
  # Rodando regressao linear com tratamento e confundidores, incluindo interação entre tratamento e renda em 1974
  # e termo quadrático para renda em 1974
  # Estimando a renda em 1978 a partir das variáveis do modelo
  reg <- lm(re78 ~ treat + age + educ + black + hispan + nodegree + married + re74 + treat:re74 + I(re74^2) + re75, data=lalonde)
  # copiando banco de dados para estimar as respostas potenciais
  lalondeexp <- lalondenexp <- lalonde 
  # Recodificando treat=1, colocando todos os participantes na situação de exposição
  lalondeexp$treat <- 1 
  # Estimando as respostas potenciais para cada participante na situação de exposição
  resp.exp <- predict(reg,newdata=lalondeexp,type="response")
  # Recodificando treat=0, colocando todos os participantes na situação de não exposição
  lalondenexp$treat <- 0 
  # Estimando as respostas potenciais para cada participante na situação de não exposição
  resp.nexp <- predict(reg,newdata=lalondenexp,type="response")
  # Calculando o efeito causal - diferença entre as médias das respostas potenciais
  mean(resp.exp)-mean(resp.nexp)
}

# Obtendo o efeito causal, informando indices 1:614
comp.g(lalonde,indices=1:nrow(lalonde))

# Bootstrap 1000 repetições
library(boot)
set.seed(1)
boot.saida=boot(data=lalonde, statistic=comp.g, R=1000)

# Erro padrão
boot.saida

# Intervalo de Confiança de 95% usando aproximação normal
boot.ci(boot.saida,type="norm",conf=0.95)

# Intervalo de Confiança de 95% usando percentis
boot.ci(boot.saida,type="perc",conf=0.95)
