# Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
# Capítulo 10
# 10. Exemplo de análise com escore de propensão em Epidemiologia no R
# páginas 197-230

# arquivo salvo em UTF-8
# Se os caracteres acentuados e a cedilha não aparecerem corretamente
# No R Studio, escolha a opção File : Reopen with Encoding: UTF-8

# Instalando os pacotes necessários
install.packages("survey")
install.packages("twang")
install.packages("descr")
install.packages("MatchIt")
install.packages("Matching")
install.packages("rbounds")
install.packages("optmatch")

# 10.1. Estimativa do efeito causal por meio do escore de propensão

# 10.1.1. Ponderação com escore de propensão

# Carregando pacote twang
library(twang)

# Abrindo banco de dados - lalonde
data(lalonde)

# Obtendo listagem das 5 primeiras observações
head(lalonde, n=5)

# Obtendo listagem das 5 últimas observações
tail(lalonde, n=5)

# Carregando biblioteca descr
library(descr)

# Obtendo a distribuição de frequências da variável tratamento - treat
freq(lalonde$treat)

# Estimando associação em modelo de regressão não ajustado
reg <- lm(re78 ~ treat,data=lalonde)
summary(reg)

# Obtendo intervalo de confiança de 95%
confint(reg)

# Estimando associação em modelo de regressão ajustado
reg <- lm(re78 ~ treat + age + educ + black + hispan + nodegree + married + + re74 + re75, data=lalonde)
summary(reg)

# Obtendo intervalo de confiança de 95%
confint(reg)

# Checando balanceamento entre os grupos antes da implementação do escore de propensão
# variavel contínua - test t de Student
t.test(age~treat, data=lalonde, var.equal=TRUE)
t.test(educ~treat, data=lalonde, var.equal=TRUE)
t.test(re74~treat, data=lalonde, var.equal=TRUE)
t.test(re75~treat, data=lalonde, var.equal=TRUE)

# variavel categórica - teste do qui-quadrado
crosstab(lalonde$treat, lalonde$nodegree, prop.r=TRUE, chisq=TRUE, plot = F)
crosstab(lalonde$treat, lalonde$married, prop.r=TRUE, chisq=TRUE, plot = F)
crosstab(lalonde$treat, lalonde$black, prop.r=TRUE, chisq=TRUE, plot = F)
crosstab(lalonde$treat, lalonde$hispan, prop.r=TRUE, chisq=TRUE, plot = F)

# variavel contínua - regressão linear com o tratamento como variável explanatória e a variável contínua (renda em 1974) como resposta
reg <- lm(re74 ~ treat,data=lalonde)
summary(reg)

# variável categórica - regressão logística com o tratamento como variável explanatória e a variável categórica (sem diploma universitário) como resposta
reg <- glm(nodegree ~ treat, family=binomial(), data=lalonde)
summary(reg)

# Estimação do escore de propensão por regressão logística
pslog<- glm(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, 
            family=binomial(), data=lalonde)
summary(pslog)

# Incluindo o escore de propensão no banco de dados
lalonde$pslog <- predict(pslog,type="response")

# Obtendo listagem das 10 primeiras observações
head(lalonde, n=10)

# Obtendo listagem das 10 últimas observações
tail(lalonde, n=10)

# Verificando área de suporte comum pelo boxplot
boxplot(pslog ~ treat, data=lalonde, ylab="Escore de propensao", xlab="Programa de retreinamento profissional")

# Ponderação com escore de propensão
# calculando os pesos usando ATE - Efeito médio do tratamento
# para o grupo de tratamento = 1/ps
# para o grupo controle= 1(1-ps)
lalonde$peso.ATE <- ifelse(lalonde$treat == 1, 1/lalonde$pslog, 1/(1-lalonde$pslog))

# Verificando se o peso foi incorporado ao banco de dados
head(lalonde, n=10)

# Checando balanceamento após a ponderação com escore de propensão

# Carregando o pacote survey para obter estimativa ponderada
library(survey)

# Variável contínua - regressão linear ponderada com o tratamento como variável explanatória e a variável contínua (renda em 1974) como resposta
design.pslog <- svydesign(ids=~1,weights=~peso.ATE, data=lalonde)
glm <- svyglm(re74 ~ treat, design=design.pslog)
summary(glm)

# variável categórica - regressão logística ponderada com o tratamento como variável explanatória e a variável categórica (sem diploma universitário) como resposta
glm <- svyglm(nodegree ~ treat, family=binomial, design=design.pslog)
summary(glm)

# usando rotina do pacote twang para verificar balanceamento
# Diferenças padronizadas absolutas entre as médias
bal.pslog <- dx.wts(x = lalonde$peso.ATE, data=lalonde, vars=c("age","educ","black","hispan","nodegree","married","re74","re75"), treat.var="treat", estimand = "ATE")
bal.pslog
bal <- bal.table(bal.pslog)
bal

# Efeito causal
# Regressão linear incluindo desfecho e apenas o tratamento como variável explanatória
# com ponderação pelo escore de propensão
# pacote survey - obtenção da estimativa ponderada
design.pslog <- svydesign(ids=~1,weights=~peso.ATE, data=lalonde)
glm <- svyglm(re78 ~ treat, design=design.pslog)
summary(glm)

# Obtendo intervalo de confiança de 95%
confint(glm)

# GBM
# estimativa do escore de propensão - ATE
set.seed(1)
psgbmate <- ps(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, data = lalonde, perm.test.iters=0, estimand = "ATE", stop.method=("es.mean"), verbose="F")

# verificando balanceamento
bal <- bal.table(psgbmate)
bal
plot(psgbmate, plots=3)

# extraindo os pesos 
lalonde$wate <- get.weights(psgbmate, stop.method="es.mean")
design.psate <- svydesign(ids=~1, weights=~wate, data=lalonde)

# obtendo listagem do banco de dados
head(lalonde, n=10)
tail(lalonde, n=10)

# Efeito causal - GBM - ATE
# Regressão linear incluindo desfecho e apenas o tratamento como variável explanatória
# com ponderação pelo escore de propensão
glm1 <- svyglm(re78 ~ treat, design=design.psate)
summary(glm1)

# Obtendo intervalo de confiança de 95%
confint(glm1)


# GBM
# estimativa do escore de propensão - ATT
set.seed(1)
psgbmatt <- ps(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, data = lalonde, perm.test.iters=0, estimand = "ATT", stop.method=("es.mean"), verbose="F")

# verificando balanceamento
bal <- bal.table(psgbmatt)
bal
plot(psgbmatt, plots=3)

# extraindo os pesos 
lalonde$watt <- get.weights(psgbmatt, stop.method="es.mean")
design.psatt <- svydesign(ids=~1, weights=~watt, data=lalonde)

# obtendo listagem do banco de dados
head(lalonde, n=10)
tail(lalonde, n=10)

# Efeito causal - GBM - ATT
# Regressão linear incluindo desfecho e apenas o tratamento como variável explanatória
# com ponderação pelo escore de propensão
glm1 <- svyglm(re78 ~ treat, design=design.psatt)
summary(glm1)

# Obtendo intervalo de confiança de 95%
confint(glm1)







# 10.1.2. Pareamento com escore de propensão

# Carregando pacote MatchIt
library(MatchIt)

# Abrindo banco de dados - lalonde
data(lalonde)

# Estimando o escore de propensão por regressão logística
ps <- glm(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, 
          family=binomial(), data=lalonde)
summary(ps)

# Calculando e convertendo escore de propensão para logito
# Gravando o logito do escore de propensão no banco de dados
lalonde$pslogit <- log(fitted(ps)/(1-fitted(ps)))

# Obtendo listagem 
head(lalonde)

# Pareamento vizinho mais próximo razão 1:1
set.seed(1)
nnmatch <- matchit(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, 
                   data =lalonde, distance="linear.logit", method= "nearest", ratio = 1)

# Checando balanceamento
summary(nnmatch, standardize=TRUE)

# Criando o arquivo pareado
match <- match.data(nnmatch)

# Obtendo listagem do arquivo pareado
head(match)
tail(match)

# Calculando o efeito causal ATT
# teste t de Student pareado
t.test(match$re78[match$treat==1],match$re78[match$treat==0],paired=TRUE)

# regressão linear assumindo independência
fit <- lm(re78~ treat, data=match)
summary(fit)
confint(fit)


# Pareamento vizinho mais próximo com distância máxima de 0.1 desvio padrão
set.seed(1)
nnmatchc <- matchit(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, 
                    data =lalonde, distance="linear.logit", method= "nearest", caliper = 0.1)
summary(nnmatchc, standardize=TRUE)

# Criação do arquivo pareado
match <- match.data(nnmatchc)

# Obtendo listagem do arquivo pareado
head(match)
tail(match)

# Calculando o efeito causal ATT
# teste t de Student pareado
t.test(match$re78[match$treat==1],match$re78[match$treat==0],paired=TRUE)

# Calculando o efeito causal ATT
# regressão linear assumindo independência
fit <- lm(re78~ treat, data=match)
summary(fit)
confint(fit)


# Pareamento ótimo
library(optmatch)
set.seed(1)
opmatch <- matchit(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, 
                   data =lalonde, distance="linear.logit")
summary(opmatch, standardize=TRUE)

# Criação do arquivo pareado
match <- match.data(opmatch)

# Obtendo listagem do arquivo pareado
head(match)
tail(match)

# Calculando o efeito causal ATT
# teste t de Student pareado
t.test(match$re78[match$treat==1],match$re78[match$treat==0],paired=TRUE)

# Calculando o efeito causal ATT
# regressão linear assumindo independência
fit <- lm(re78~ treat, data=match)
summary(fit)
confint(fit)





# 10.2. Análise de sensibilidade

# Carregando pacotes Matching e rbounds
library(Matching)
library(rbounds)

# Usando o comando attach para acessar variáveis de um banco de dados pelo nome
attach(lalonde)

# Definindo Y desfecho Tr tratamento X preditores do tratamento
Y <- re78
Tr <-treat
X = cbind(age, educ, black, hispan, married, nodegree, re74, re75)

# Calculando escore de propensão
ps<- glm(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, family=binomial(), data=lalonde)

# Definindo início casual
set.seed(1)

# Pareamento pelo escore de propensão - padrão - 1:1 estimativa do ATT com reposição
# sem reposição - replace=FALSE
Match <- Match(Y=Y, Tr=Tr, X=X, replace=FALSE) 
summary(Match)

# Análise de sensibilidade - cálculo do gama
psens (Match, Gamma=2, GammaInc=0.1)
