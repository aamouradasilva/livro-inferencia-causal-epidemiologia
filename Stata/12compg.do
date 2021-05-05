* Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
* Capítulo 12
* 12.4. Cálculo da computação G paramétrica no Stata
* páginas 322-325



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
save esf

* Calculando a média da vacinação segundo participação na ESF
bysort esf: sum vac

* Copiando banco de dados (salvando com outro nome)
* para gerar banco onde todos estão expostos
save esfexp
* para gerar banco onde ninguém está exposto
save esfnexp

* Recodificando esf=1 em esfexp e esf=0 em esfnexp
use esfexp
replace esf=1
save, replace

use esfnexp
replace esf=0
save, replace

* Verificando se a recodificação foi realizada corretamente
use esf
tab esf
* Gerando variável indicadora banco de dados original 
gen ind=-1
save, replace

use esfexp
tab esf
* Gerando variável indicadora banco de dados dos expostos 
gen ind=1
* Apagando valores na variável resposta
replace vac=.
save, replace


use esfnexp
tab esf
* Gerando variável indicadora banco de dados dos não expostos
gen ind=0
* Apagando valores na variável resposta
replace vac=.
save, replace

* Unindo bancos de dados
use esf
append using esfexp 
append using esfnexp

save total

* Rodando regressão linear com tratamento e desfecho
regress vac esf

* Rodando regressão linear com tratamento e confundidor incluindo interacao entre tratamento e confundidor
* Como só há 10 casos com vac preenchida (os outros 20 foram recodificados como ignorados), 
* a regressão só usará as observações originais
* Estimando o percentual de vacinação infantil incompleta a partir das variáveis do modelo
regress vac esf##pobreza

* Usando a regressão nas 10 observações originais (ind=-1) para predizer as respostas potenciais se todos estivessem expostos (ind=1) 
* ou se ninguém estivesse exposto (ind=0)
predict resp

* Calculando a média das respostas potenciais se todos estivessem expostos
sum resp if ind==1
gen exp=r(mean)
tab exp

* Calculando a média das respostas potenciais se ninguém estivesse exposto
sum resp if ind==0
gen nexp=r(mean)
tab nexp

* Calculando efeito causal - diferença entre as médias das respostas potenciais
gen compgd=exp-nexp
tab compgd

* Apagando as observações originais
drop if ind==-1

* Obtendo o efeito causal pela regressão linear - regredindo esf nas respostas potenciais
* O intervalo de confiança de 95% deve ser calculado por bootstrap
regress resp esf
