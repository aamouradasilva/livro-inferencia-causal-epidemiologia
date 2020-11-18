* Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
* Capítulo 9
* 9.2.2. Rotina para estimar o escore de propensão por meio de regressão logística no Stata
* página 183-184

* Entrada de dados
input esf renda escolaridade
1 0.25 2
1 1 4
1 0.75 6
1 0.50 2
1 0.50 8
0 1 3
0 0.75 7
0 0.50 5
0 2 10
0 3 15
end

* Salvando os dados
save esf2

* Modelo logístico
logit esf renda escolaridade

* Estimando o escore de propensão (ep)
predict ep

* Listagem para verificar que o escore de propensão foi incorporado ao banco de dados
list esf renda escolaridade ep
