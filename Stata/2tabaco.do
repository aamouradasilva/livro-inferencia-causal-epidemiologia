* Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
* Capítulo 2
* 2.2.2. Cálculo do efeito causal médio com as respostas potenciais fatual e contrafatual no Stata
* página 23-24

* Entrada de dados
input d1 d0 n
1 0 20
1 1 5
0 1 20
0 0 15
end

* Expansão do banco de dados
expand n

* Salvar o banco de dados
save tabaco

* Cálculo das diferenças entre as respostas potenciais individuais
gen d=d1-d0

* Cálculo do Efeito Causal médio (média das diferenças entre as respostas potenciais individuais)
sum d

* Cálculo das médias das respostas potenciais individuais
sum d1
gen md1=r(mean)
sum d0
gen md0=r(mean)

* Cálculo da diferença entre as médias das respostas potenciais
gen dmd=md1-md0
sum dmd

* Cálculo da razão entre as médias das respostas potenciais
gen r=md1/md0
sum r
