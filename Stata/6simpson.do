* Livro Introdução à Inferência Causal em Epidemiologia: uma abordagem gráfica e contrafatual
* Capítulo 6
* 6.1.2. Exercício resolvido - o paradoxo de Simpson
* página 130

* Entrada de dados 
input t d c n
1 1 1 5
1 0 1 8 
0 1 1 3 
0 0 1 4 
1 1 0 15 
1 0 0 12 
0 1 0 3 
0 0 0 2 
end

save simpson

* Expansão do banco de dados 
expand n
 
* Cálculo da razão de risco bruta e ajustada
cs d t, by(c)

* Cálculo do odds ratio (razão de chances) bruta e ajustada
cc d t, by(c)

