* Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
* Capítulo 14
* 14. Exemplo de análise usando computação G paramétrica no Stata
* páginas 345-353

* Abrindo o banco de dados lalonde
use lalonde

* Calculando a média da renda em 1978 segundo participação
* no programa de retreinamento profissional
bysort treat: sum re78

* Estimando associação em modelo de regressão linear não ajustado 
regress re78 treat

* Estimando associação em modelo de regressão linear ajustado 
* sem interações ou termos quadráticos
regress re78 treat age educ black hispan nodegree married re74 re75

* Rodando regressão linear com tratamento e confundidor 
* incluindo interação entre tratamento e renda em 1974 
* e termo quadrático para renda em 1974
regress re78 treat age educ black hispan nodegree married i.treat##c.re74 c.re74##c.re74 re75

* Expandindo banco de dados para gerar cópia do banco  
expand 2, generate(copia)
* gerado automaticamente
* copia = 0  (banco de dados onde ninguém está exposto)
* copia = 1  (banco de dados onde todos estão expostos)
tab copia

* Tornando todos não expostos na copia 0 (treat=0)
replace treat = 0 if copia == 0
* Tornando todos expostos na copia 1 (treat=1)
replace treat = 1 if copia == 1

* Verificando se a recodificação foi realizada corretamente
by copia, sort: summarize treat

* Usando a regressão nas observações originais 
* realizada antes das duplicações
* para predizer os desfechos se todos estivessem expostos (copia=1) 
* ou se ninguém estivesse exposto (copia=0)
predict resp

* Verificando os valores médios das  predições  
* nas cópias de expostos e não expostos
by copia, sort: summarize resp

* Armazenando os valores das estimativas em uma matriz
summarize resp if copia == 1
matrix input est = (`r(mean)')
summarize resp if copia == 0
matrix est = (est \ `r(mean)')
matrix est = (est \ est[1,1]-est[2,1]) 

* Inserindo cabeçalhos para as linhas e colunas da matriz 
* D=desfecho t=tratamento
matrix rownames est = E(D(t=1)) E(D(t=0)) Diferença
matrix colnames est = Estimativa

* Na primeira linha r1 o valor da média entre os expostos E(D(t=1))
* Na segunda linha r2 o valor da média entre os não expostos E(D(t=0)) 
* Na terceira linha r3 o valor da diferença entre expostos e não expostos 
* E(D(t=1))-E(D(t=0))
matrix list est 


* Cálculo do erro padrão e do intervalo de confiança usando bootstrapping

* Apagando o programa da memória
capture program drop bootlalonde

* Rodando o programa 
program define bootlalonde, rclass
	use lalonde, clear
	    * preservar os dados após o término do programa 
		* os dados serão usados posteriormente para o cálculo
		* do intervalo de confiança
		preserve
		* retirar amostra por bootstrap das observações originais
		bsample 
		* Rodando regressão linear com tratamento e confundidor 
		* incluindo interação entre tratamento renda em 1974 
		* e termo quadrático para renda em 1974
		regress re78 treat age educ black hispan nodegree married i.treat##c.re74 c.re74##c.re74 re75
		* Expandindo banco de dados para gerar cópia do banco 
		expand 2, generate(copia)
		* Tornando todos não expostos na copia 0 (treat=0)
		replace treat = 0 if copia == 0
		* Tornando todos expostos na copia 1 (treat=1)
		replace treat = 1 if copia == 1
		* Usando a regressão nas observações originais realizada 
		* antes das duplicações
		* para predizer os desfechos se todos estivessem expostos (copia=1) 
		* ou se ninguém estivesse exposto (copia=0)
		predict resp
		* Calculando a média das respostas potenciais na situação 
		* de não exposição
		summarize resp if copia == 0
		return scalar media0 = r(mean)
		* Calculando a média das respostas potenciais na situação de exposição
		summarize resp if copia == 1
		return scalar media1 = r(mean)
		* Calculando a diferença entre as médias das respostas potenciais
		return scalar dif = return(media1) - return(media0)
	restore
end


* Rodando bootstrap com 1000 replicações
simulate Diferença = r(dif), reps(1000) seed(1): bootlalonde /

* Gerando matriz com as estimativas de ponto nas linhas 
matrix ponto = est[3..3, 1]'
matrix list ponto

* Gerando intervalo de confiança de 95% por aproximação normal usando bootstrap 
* o n contém o número de observações originais (n=614)
bstat, stat(ponto) n(614) 

* Gerando intervalo de confiança de 95% por percentis usando bootstrap
estat bootstrap, p
