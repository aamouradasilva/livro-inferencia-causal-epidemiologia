* Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
* Capítulo 8
* 8.2.2. Script para estimar o efeito causal por meio da ponderação pelo inverso da probabilidade de seleção no Stata
* página 154-156


* Construindo o banco de dados
input esf pobreza vac
1 1 0.4
1 0 0.20
1 1 0.30
1 1 0.45
1 1 0.40
0 0 0.30
0 0 0.40
0 1 0.50
0 0 0.10
0 0 0.15
end

* Salvando o banco de dados
save esf, replace

* Calculando a méia da vacinação segundo participação na ESF
bysort esf: sum vac

* Rodando regressão logística com tratamento e confundidor
* Estimando o escore de propensão
logit esf pobreza

* Calculando e incluindo o escore de propensão no banco de dados
predict ep

* Listagem do banco de dados com o escore de propensão
list 

* Ponderação com escore de propensão
* calculando os pesos 
* para o grupo de tratamento = 1/ep
* para o grupo controle= 1(1-ep)
gen peso=1/ep if esf==1
replace peso=1/(1-ep) if esf==0

* Listagem do banco de dados com o peso
list

* Cálculo do efeito causal em modelo explicativo
* ponderado pelo inverso da probabilidade de seleção
regress vac esf [pweight=peso]

