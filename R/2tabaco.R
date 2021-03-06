# Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
# Capítulo 2
# 2.2.1. Cálculo do efeito causal médio com as respostas potenciais fatual e contrafatual no R
# página 42

# arquivo salvo em UTF-8
# Se os caracteres acentuados e a cedilha não aparecerem corretamente
# No R Studio, escolha a opção File : Reopen with Encoding: UTF-8

# Entrando dados
df <- data.frame(
  d1=c(1,1,0,0),
  d0=c(0,1,1,0),
  n=c(20,5,20,15))

# Criando banco de dados tabaco
tabaco <- data.frame(d1 = rep(df$d1, df$n),
                     d0 = rep(df$d0, df$n))

# Cálculo das diferenças entre as respostas potenciais individuais
d <- tabaco$d1-tabaco$d0

# Cálculo do Efeito Causal médio (média das diferenças entre as respostas potenciais individuais)
summary(d)

# Cálculo das médias das respostas potenciais individuais
summary(tabaco$d1)
md1 <-mean(tabaco$d1)
summary(tabaco$d0)
md0 <-mean(tabaco$d0)

# Cálculo da diferença entre as médias das respostas potenciais
dmd <- md1-md0
summary(dmd)

# Cálculo da razão entre as médias das respostas potenciais
r <- md1/md0
summary(r)
