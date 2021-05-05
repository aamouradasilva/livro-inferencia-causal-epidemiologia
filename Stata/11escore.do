* Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
* Capítulo 11
* 10. Exemplo de análise com escore de propensão em Epidemiologia no Stata
* páginas 283-309


* 11.1. Estimativa do efeito causal por meio do escore de propensão

* 11.1.1. Ponderação com escore de propensão

* Abrindo banco de dados - lalonde
use lalonde

* Obtendo listagem das 15 primeiras e 15 últimas observações
list treat age educ black hispan married nodegree re78 in 1/15
list treat age educ black hispan married nodegree re78 in 600/614

* Obtendo a distribuição de frequências da variável tratamento
tab treat

* Estimando associação em modelo de regressão não ajustado
regress re78 treat

* Estimando associação em modelo de regressão ajustado
regress re78 treat age educ black hispan nodegree married re74 re75

* Checando balanceamento entre os grupos antes da implementação do escore de propensão
* variável contínua - test t de Student
ttest age, by(treat)
ttest educ, by(treat)
ttest re74, by(treat)
ttest re75, by(treat)

* variável categórica - teste do qui-quadrado
tab treat nodegree, row chi2
tab treat married, row chi2
tab treat black, row chi2
tab treat hispan, row chi2

* variável contínua - regressão linear com o tratamento como variável explanatória 
* e a variável contínua (renda em 1974) como resposta
regress re74 treat

* variável categórica - regressão logística com o tratamento como variável explanatória 
* e a variável categórica (sem diploma universitário) como resposta
logistic nodegree treat

* Estimação do escore de propensão por regressão logística
logistic treat age educ black hispan nodegree married re74 re75

* Incluindo o escore de propensão no banco de dados
predict pslog

* Obtendo listagem das 10 primeiras observações
list treat age educ re78 pslog in 1/10

* Obtendo listagem das 10 últimas observações
list treat age educ re78 pslog in 605/614

* Verificando área de suporte comum pelo boxplot
graph box pslog, ytitle(Escore de propensão) by(treat)

* Ponderação com escore de propensão
* calculando os pesos usando ATE - Efeito médio do tratamento
* para o grupo de tratamento = 1/ps
* para o grupo controle= 1/(1-ps)
gen wa=1/pslog if treat==1
replace wa=1/(1-pslog) if treat==0

* Verificando se o peso foi incorporado ao banco de dados
list treat age educ re78 pslog wa in 1/10

* Checando balanceamento após a ponderaçao com escore de propensão
* variavel contínua - regressão linear ponderada com o tratamento como variável explanatória 
* e a variável contínua como desfecho
regress re74 treat [pweight=wa]

* variável categórica binária- regressão logística ponderada com o tratamento como variável explanatória 
* e a variável categórica (sem diploma universitário) como resposta
logistic nodegree treat [pweight=wa]

* Efeito causal
* Regressão linear incluindo desfecho e apenas o tratamento como variável explanatória
* com ponderação pelo escore de propensão
regress re78 treat [pweight=wa]

* Usando teffects - estimando ATE
teffects ipw (re78) (treat age educ black hispan nodegree married re74 re75)
* Checando balanceamento após a ponderação com escore de propensão
tebalance summ

* Usando teffects - estimando ATT
teffects ipw (re78) (treat age educ black hispan nodegree married re74 re75), atet
* Checando balanceamento após a ponderação com escore de propensão
tebalance summ
tebalance density educ





* 11.1.2 Pareamento com escore de propensão

* Abrindo banco de dados - lalonde
use lalonde

* Estimação do escore de propensão por regressão logística
logistic treat age educ black hispan nodegree married re74 re75

* Incluindo o escore de propensão no banco de dados
predict pslog

* Pareamento - rotina teffects - opção psmatch - ATE
teffects psmatch (re78) (treat age educ black hispan nodegree married re74 re75)

* Checando balanceamento após o pareamento com escore de propensão
tebalance summarize
tebalance density age
tebalance box age

* Pareamento - rotina teffects - opção psmatch - ATT
teffects psmatch (re78) (treat age educ black hispan nodegree married re74 re75), atet

* Checando balanceamento após o pareamento com escore de propensão
tebalance summarize

* Pareamento - rotina teffects - opção nnmatch - ATE
teffects nnmatch (re78 age educ black hispan nodegree married re74 re75) (treat)

* Checando balanceamento após o pareamento com escore de propensão
tebalance summarize

* Pareamento - rotina teffects - opção nnmatch - ATT
teffects nnmatch (re78 age educ black hispan nodegree married re74 re75) (treat), atet

* Checando balanceamento após o pareamento com escore de propensão
tebalance summarize

tebalance density educ
tebalance box educ
