* Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
* Capítulo 7
* 7.4. Exemplo numérico de viés de colisão - Dieta e risco de câncer não relacionado com a dieta
* página 164

* Entrada de dados 
input t d c n 
1 1 1 55 
1 0 1 25 
0 1 1 70 
0 0 1 10 
1 1 0 45 
1 0 0 75 
0 1 0 130 
0 0 0 190 
end

save dieta  

* Expansão do banco de dados
expand n  

* Cálculo da razão de risco bruta e ajustada
cs d t, by(c)

