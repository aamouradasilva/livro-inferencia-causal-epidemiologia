* Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
* Capítulo 12
* 12.4. Cálculo da computação G paramétrica no Stata
* páginas 264-268



* Construindo o banco de dados
input psf pobreza vac
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
save psf

* Calculando a média da vacinação segundo participação no PSF
bysort psf: sum vac

* Copiando banco de dados (salvando com outro nome)
* para gerar banco onde todos estão expostos
save psfexp
* para gerar banco onde todos não estão expostos
save psfnexp

* Recodificando psf=1 em psfexp e psf=0 em psfnexp
use psfexp
replace psf=1
save, replace

use psfnexp
replace psf=0
save, replace

* Verificando se a recodificação foi realizada corretamente
use psf
tab psf
* Gerando variável indicadora banco de dados original 
gen ind=-1
save, replace

use psfexp
tab psf
* Gerando variável indicadora banco de dados dos expostos 
gen ind=1
* Apagando valores na variável resposta
replace vac=.
save, replace


use psfnexp
tab psf
* Gerando variável indicadora banco de dados dos não expostos
gen ind=0
* Apagando valores na variável resposta
replace vac=.
save, replace

* Unindo bancos de dados
use psf
append using psfexp 
append using psfnexp

save total

* Rodando regressão linear com tratamento e desfecho
regress vac psf

* Rodando regressão linear com tratamento e confundidor incluindo interacao entre tratamento e confundidor
* Como só há 10 casos com vac preenchida (os outros 20 foram recodificados como ignorados), 
* a regressão só usará as observações originais
* Estimando o percentual de vacinação infantil incompleta a partir das variáveis do modelo
regress vac psf##pobreza

* Usando a regressão nas 10 observações originais (ind=-1) para predizer as respostas potenciais se todos estivessem expostos (ind=1) 
* ou se todos não estivessem expostos (ind=0)
predict resp

* Calculando a média das respostas potenciais se todos estivessem expostos
sum resp if ind==1
gen exp=r(mean)
tab exp

* Calculando a média das respostas potenciais se todos não estivessem expostos
sum resp if ind==0
gen nexp=r(mean)
tab nexp

* Calculando efeito causal - diferença entre as médias das respostas potenciais
gen compgd=exp-nexp
tab compgd

* Apagando as observações originais
drop if ind==-1

* Obtendo o efeito causal pela regressão linear - regredindo psf nas respostas potenciais
* O intervalo de confiança de 95% deve ser calculado por bootstrap
regress resp psf
