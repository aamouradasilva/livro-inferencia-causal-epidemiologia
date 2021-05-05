# Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
# Capítulo 15
# 15.4. Script no R - Análise de Sensibilidade pelo método de Greenland
# páginas 367-369

# arquivo salvo em UTF-8
# Se os caracteres acentuados e a cedilha não aparecerem corretamente
# No R Studio, escolha a opção File : Reopen with Encoding: UTF-8

# método de Greenland
# Arbitrando os valores de ORdu, OReu e pu0 
# dados Doll & Hill (1952)

install.packages("descr")
library(descr)

# Digitando os valores observados na tabela 2 x 2 (Tabela 15.1)

# E - fumo
# D - câncer de pulmão
# U - gene do fumo

#b1  E=1 e D=0
b1<-1296
#b0  E=0 e D=0
b0<-61
#a1  E=1 e D=1
a1<-1350
#a0  E=0 e D=1
a0<-7

# Arbitrando os valores de ORdu, OReu e pu0 
ORdu<-2.0 # associação entre o gene do fumo e câncer de pulmão
OReu<-81.00 # associação entre o fumo e o gene do fumo
pu0<-0.10 # prevalência do gene do fumo em não fumantes

# Calculando pu1
pu1=(OReu*pu0)/(1-pu0+OReu*pu0)

# Calculando b11 e b01
b11<-pu1*b1
b01<-pu0*b0

# Calculando a11 e a01
a11<-(ORdu*a1*b11)/(ORdu*b11+b1-b11)
a01<-(ORdu*a0*b01)/(ORdu*b01+b0-b01)

# Renomeando as caselas da tabela 15.2
e<-b01
f<-a01
g<-b11
h<-a11

# Calculando as demais caselas da tabela 15.2 
a<-b0-e
b<-a0-f
c<-b1-g
d<-a1-h

# Arredondando as casas decimais 
a<-round(a)
b<-round(b)
c<-round(c)
d<-round(d) 
e<-round(e) 
f<-round(f) 
g<-round(g) 
h<-round(h)

# Recriando o banco de dados
A<-matrix(c(0,0,0),nrow=a, ncol=3, byrow=T) 
B<-matrix(c(0,1,0),nrow=b, ncol=3, byrow=T) 
C<-matrix(c(1,0,0),nrow=c, ncol=3, byrow=T) 
D<-matrix(c(1,1,0),nrow=d, ncol=3, byrow=T) 
E<-matrix(c(0,0,1),nrow=e, ncol=3, byrow=T) 
F<-matrix(c(0,1,1),nrow=f, ncol=3, byrow=T) 
G<-matrix(c(1,0,1),nrow=g, ncol=3, byrow=T) 
H<-matrix(c(1,1,1),nrow=h, ncol=3, byrow=T)

# Nomeando o banco Doll
Doll<-rbind(A,B,C,D,E,F,G,H)

# Dando nome às colunas e reformatando os dados
var<-c("Exposicao", "Doenca", "U")
colnames(Doll)<-var
Doll<-as.data.frame(Doll)

# Descrevendo os dados
crosstab(Doll$Exposicao, Doll$Doenca, prop.r=TRUE, chisq=TRUE, plot = F)

# Rodando regressão logística com os dados recriados, ajustando para u
model<-glm(Doenca~Exposicao+U, family=binomial, data= Doll) 
summary(model)
model.coeff<-glm(Doenca~Exposicao+U, family=binomial, data=Doll)$coeff

# Mostrando ORde ajustado para u com IC 95% 
exp(model.coeff)
exp(confint(model))
