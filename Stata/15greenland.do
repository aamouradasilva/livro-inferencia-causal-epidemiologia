* Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
* Capítulo 15
* 15.5. Análise de Sensibilidade pelo método de Greenland no Stata
* páginas 306
* resultados da linha 1 da tabela 15.3 (página 302)

* Instalando módulo episens
ssc install episens

* Calculando o OR_(DE|U) a partir de um palpite educado de P_U1 e P_U0 e OR_DU.

* OR_DU=1.5
episensi 1350 7 1296 61, dpexp(c(.90)) dpunexp(c(.10)) drrcd(c(1.5))

* OR_DU=2
episensi 1350 7 1296 61, dpexp(c(.90)) dpunexp(c(.10)) drrcd(c(2))

* OR_DU=2.5
episensi 1350 7 1296 61, dpexp(c(.90)) dpunexp(c(.10)) drrcd(c(2.5))

* OR_DU=3
episensi 1350 7 1296 61, dpexp(c(.90)) dpunexp(c(.10)) drrcd(c(3))
