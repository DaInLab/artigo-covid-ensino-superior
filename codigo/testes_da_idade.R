casos_idade_concat <- table(idade_concat$faixa_etaria)
casos_idade_concat
#de 17 a 21 anos de 22 a 26 anos de 27 a 31 anos de 32 a 36 anos de 37 a 41 anos de 42 a 46 anos de 47 a 51 anos de 52 a 56 anos de 57 a 61 anos 
#             23              25               8               5               8               5               6               4               2 
#             ND 
#              1

# Maior número de casos
max(casos_idade_concat)
#[1] 25
# Qual a posição, dentro da array, do maior número de casos
which.max(casos_idade_concat)
# de 22 a 26 anos 
#               2
# A string com a maior quantidade de casos
names(which.max(casos_idade_concat))
#[1] "de 22 a 26 anos"

# A porcentagem da maior quantidade de casos de idade
#Cálculo da porcentagem das faixas etárias
pct_idade3 <- paste0(round(unname(casos_idade_concat) / sum(unname(casos_idade_concat)) * 100,0), "%")
pct_idade3
#[1] "26%" "29%" "9%"  "6%"  "9%"  "6%"  "7%"  "5%"  "2%"  "1%" 
unname(which.max(casos_idade_concat))
#[1] 2
pct_idade3[unname(which.max(casos_idade_concat))]
#[1] "29%

# E o segundo caso com maior número de casos?
max(casos_idade_concat[casos_idade_concat!=max(casos_idade_concat)])
#[1] 23
names(which.max(casos_idade_concat[casos_idade_concat!=max(casos_idade_concat)]))
#[1] "de 17 a 21 anos"
pct_idade3[unname(which.max(casos_idade_concat[casos_idade_concat!=max(casos_idade_concat)]))]
#[1] "26%"

# Uma outra abordagem
n <- length(casos_idade_concat)
n
#[1] 10
pct_idade3 <- paste0(round(unname(sort(casos_idade_concat)) / sum(unname(sort(casos_idade_concat))) * 100,0), "%")
pct_idade3
#[1] "1%"  "2%"  "5%"  "6%"  "6%"  "7%"  "9%"  "9%"  "26%" "29%"

sort(casos_idade_concat,partial=n-0)[n-0] # maior quantidade de faixa etária
#de 22 a 26 anos 
#             25
names(sort(casos_idade_concat,partial=n-0)[n-0]) # descrição do maior
#[1] "de 22 a 26 anos"
pct_idade3[n-0] # percentual do maior

sort(casos_idade_concat,partial=n-1)[n-1] # segundo maior
#de 17 a 21 anos 
#             23 
sort(casos_idade_concat,partial=n-2)[n-2] # terceiro maior
# de 37 a 41 anos 
#               8
names(sort(casos_idade_concat,partial=n-2)[n-2]) # descrição do terceiro maior
# [1] "de 37 a 41 anos"
unname(sort(casos_idade_concat,partial=n-2)[n-2]) # quantidade do terceiro maior
#[1] 8
pct_idade3[n-2] # percentual do terceiro maior
#[1] "9%"
