# Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
# Capítulo 15
# 15.6. Abordagens de Rosenbaum
# páginas 309-312

# arquivo salvo em UTF-8
# Se os caracteres acentuados e a cedilha não aparecerem corretamente
# No R Studio, escolha a opção File : Reopen with Encoding: UTF-8

# Teste exato de McNemar
binom.test(16,17, p=0.5, alternative="greater")

# Odds ratio para dados pareados e intervalo de confiança de 95%
n <- 16
d <- 1
print(paste("odds ratio:",n/d))

n/d * exp(c(-1,1)*1.96*sqrt(1/n + 1/d))

# Declarar o valor de gama
gama <- 3

# Calcular p+ usando gama
pmais <- gama/(1+gama)

# Mostrar valor de pmais
pmais

# Calcular o valor do limite superior do OR EU usando o teste binomial exato
# Substituir p=0,5 que corresponde a um gama de 1, pelo valor de pmais, que corresponde a um gama de 3
binom.test(16, 17, p=pmais, alternative="greater")

# Instalar e carregar o pacote rbounds
install.packages("rbounds")
library(rbounds)

binarysens(1,16,6,0.5)


