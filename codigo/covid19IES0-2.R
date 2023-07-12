# - ETAPA INICIAL
#--- Importação e preparação dos dados
# versão inicial em 05/01/2023.
# versão do projeto "Impacto da Covid 19 nos Estudantes de ENsino Superior" no GitHub

# Lendo o arquivo em .xlsx
if (!("readxl") %in% installed.packages()) install.packages("readxl")
library(readxl)
dbf.xlsx <- read_excel("./dados/COVID-19-E-IES-2023.xlsx")

# Lendo o arquivo em .csv
dbf.csv <-read.csv("./dados/COVID-19-E-IES-2023.csv", header = TRUE, sep = ";", quote = "\"", dec = ".")

# Lendo o arquivo em .ods
if (!("readODS") %in% installed.packages()) install.packages("readODS")
library(readODS)
dbf.ods <- read_ods("./dados/COVID-19-E-IES-2023.ods")

# Utilizando o pacote smartEDA no dataframe
if(!("SmartEDA") %in% installed.packages()) install.packages("SmartEDA")
library(SmartEDA)
if(!("ISLR") %in% installed.packages()) install.packages("ISLR")
library("ISLR")

# Função para gerar o dicionário de dados de um dataframe
ExpData(data=dbf.xlsx,type=1)

ExpData(data=dbf.xlsx,type=2)

# número total de observações no dataframe
total_casos <- nrow(dbf.xlsx)
total_casos  
#[1] 87

## Gráfico 1 Faixa Etária dos respondentes
casos_idade <- table(dbf.xlsx$idade)
casos_idade
# 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 33 34 35 37 39 40 41 43 46 47 48 49 51 54 55 57 60 
#  1  1  8  5  8  5  8  3  5  4  3  1  1  1  2  1  2  2  1  1  3  3  4  1  1  2  1  2  2  2  1  1
#Cálculo da porcentagem das faixas etárias
pct_idade <- paste0(round(unname(casos_idade) / sum(unname(casos_idade)) * 100,0), "%")
pct_idade  
#  [1] "1%" "1%" "9%" "6%" "9%" "6%" "9%" "3%" "6%" "5%" "3%" "1%" "1%" "1%" "2%" "1%" "2%" "2%" "1%" "1%" "3%" "3%" "5%" "1%" "1%" "2%" "1%" "2%"
# [29] "2%" "2%" "1%" "1%"

# Gráfico do tipo barra das faixas etárias
graph.idade <- barplot(casos_idade, 
                       main = "Gráfico 1: Faixa etária dos respondentes",
                       xlab = "Faixa Etária", 
                       ylab = "Respondentes",
                       col = "orange",
                       ylim = c(0,max(casos_idade) + 30))
text(x = graph.idade, y = casos_idade, label = unname(casos_idade), cex=1, pos=3)
axis(1, at=graph.idade, labels=paste("(", pct_idade, ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)  
 
# Versão 2, utilizando a definição divisão das faixas etárias:
# 17 - 21
# 22 - 26
# 27 - 31
# 32 - 36
# 37 - 41
# 42 - 46 
# 47 - 51 
# 52 - 56
# 57 - 61

idade_concat <- data.frame(idade=dbf.xlsx$idade, faixa_etaria="")

for (k in 1:nrow(idade_concat)) {
  if(is.na(idade_concat$idade[k])) {idade_concat$faixa_etaria[k] <- "ND" 
  } else if(idade_concat$idade[k] <= 21) {idade_concat$faixa_etaria[k] <- "de 17 a 21 anos"
  } else if(idade_concat$idade[k] >= 22 & idade_concat$idade[k] <= 26) {idade_concat$faixa_etaria[k] <- "de 22 a 26 anos"
  } else if(idade_concat$idade[k] >= 27 & idade_concat$idade[k] <= 31) {idade_concat$faixa_etaria[k] <- "de 27 a 31 anos"
  } else if(idade_concat$idade[k] >= 32 & idade_concat$idade[k] <= 36) {idade_concat$faixa_etaria[k] <- "de 32 a 36 anos"
  } else if(idade_concat$idade[k] >= 37 & idade_concat$idade[k] <= 41) {idade_concat$faixa_etaria[k] <- "de 37 a 41 anos"
  } else if(idade_concat$idade[k] >= 42 & idade_concat$idade[k] <= 46) {idade_concat$faixa_etaria[k] <- "de 42 a 46 anos" 
  } else if(idade_concat$idade[k] >= 47 & idade_concat$idade[k] <= 51) {idade_concat$faixa_etaria[k] <- "de 47 a 51 anos" 
  } else if(idade_concat$idade[k] >= 52 & idade_concat$idade[k] <= 56) {idade_concat$faixa_etaria[k] <- "de 52 a 56 anos"
  } else if(idade_concat$idade[k] >= 57 & idade_concat$idade[k] <= 61) {idade_concat$faixa_etaria[k] <- "de 57 a 61 anos" 
  } else if(idade_concat$idade[k] > 61) {idade_concat$faixa_etaria[k] <- "acima de 61 anos"
  }
}

casos_idade_concat <- table(idade_concat$faixa_etaria)
casos_idade_concat
#de 17 a 21 anos de 22 a 26 anos de 27 a 31 anos de 32 a 36 anos de 37 a 41 anos de 42 a 46 anos de 47 a 51 anos de 52 a 56 anos de 57 a 61 anos 
#             23              25               8               5               8               5               6               4               2 
#             ND 
#              1

#Cálculo da porcentagem das faixas etárias
pct_idade3 <- paste0(round(unname(casos_idade_concat) / sum(unname(casos_idade_concat)) * 100,0), "%")
pct_idade3
#[1] "21%" "27%" "6%"  "6%"  "13%" "10%" "8%"  "8%"  "2%"

# Gráfico do tipo barra das faixas etárias
graph.idade_concat <- barplot(casos_idade_concat, 
                               main = "Gráfico 1: Faixa etária dos respondentes redefinida",
                               xlab = "Faixa Etária", 
                               ylab = "Respondentes",
                               col = "orange",
                               ylim = c(0,max(casos_idade_concat) + 100))
text(x = graph.idade_concat, y = casos_idade_concat, label = unname(casos_idade_concat), cex=1, pos=3)
axis(1, at=graph.idade_concat, labels=paste("(", pct_idade3, ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)

# Dados do Gráfico 2
casos_genero <- table(dbf.xlsx$genero)
casos_genero
#  Feminino      LGBT Masculino 
#        44         3        40                      2 
pct_genero <- paste(round(unname(casos_genero) / sum(unname(casos_genero)) * 100), "%")
pct_genero  
#[1] "51 %" "3 %"  "46 %"

#Gráfico 2: Quantidade de respondentes por sexo
# Gráfico tipo "pizza"
pie(casos_genero,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "green"),
    labels = paste(names(casos_genero), "-", pct_genero),
    main = "Gráfico 2: Quantidade de respondentes por gênero")

# Dados do Gráfico 3
casos_conjugal <- table(dbf.xlsx$situacao_conjugal)
casos_conjugal
#                  Casado(a)   Divorciado(a)/Separado(a)                 Solteiro(a) União Estável/Vivendo junto                    Viúvo(a) 
#                        20                           3                          55                           8                           1 

sort(casos_conjugal)
#Viúvo(a)   Divorciado(a)/Separado(a) União Estável/Vivendo junto                   Casado(a)                 Solteiro(a) 
#       1                           3                           8                          20                          55 
n <- length(casos_conjugal)
n
# [1] 5
sort(casos_conjugal,partial=n-0)[n-0] # maior quantidade de situação conjugal
#Solteiro(a) 
#         55

pct_conjugal <- paste(round(unname(sort(casos_conjugal)) / sum(unname(sort(casos_conjugal))) * 100), "%")
pct_conjugal  
#[1] "1 %"  "3 %"  "9 %"  "23 %" "63 %" 

#Gráfico 3: Quantidade de respondentes por situação conjugal
# Gráfico tipo "pizza"
pie(sort(casos_conjugal),
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("black", "orange", "yellow", "green", "red"),
    labels = paste(names(sort(casos_conjugal)), "-", pct_conjugal),
    main = "Gráfico 3: Quantidade de respondentes por sitação conjugal")

# Dados do Gráfico 4
casos_emprego <- table(dbf.xlsx$situacao_empregaticia)
casos_emprego
#                 Aposentada                 Autônomo(a)                    Bolsista Dependente (dos pais, etc.)             Desempregado(a)                Empregado(a) 
#                           1                          4                          12                          17                           4                          41 
#               Estagiário(a)           Servidora pública 
#                           7                           1 

# Rótulos muito grandes  em grande quantidade. Dificuldade para exibir nos gráficos
# Redução proposital dos textos
names(casos_emprego) <- c("Aposentado","Autônomo", "Bolsista", "Desempregado","Empregado", 
                          "Estagiário","Serv. público", "ND")
casos_emprego
#Aposentado      Autônomo      Bolsista  Desempregado     Empregado    Estagiário Serv. público          <NA> 
#         1             4            12            17             4            41             7             1  

pct_emprego <- paste(round(unname(sort(casos_emprego)) / sum(unname(sort(casos_emprego))) * 100), "%")
pct_emprego
#[1] "1 %"  "1 %"  "5 %"  "5 %"  "8 %"  "14 %" "20 %" "47 %"
n <- length(casos_emprego)
n
#Gráfico 4: Quantidade de respondentes por situação empregatícia
# Gráfico tipo "pizza"
pie(sort(casos_emprego),
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "green", "black"),
    labels = paste(names(sort(casos_emprego)), "-", pct_emprego),
    main = "Gráfico 4: Quantidade de respondentes por sitação empregatícia")

# Gráfico 4 do tipo barra sitação empregatícia
graph.casos_emprego <- barplot(sort(casos_emprego), 
                              main = "Gráfico 4: Quantidade de respondentes por sitação empregatícia",
                              xlab = "Sitação Empregatícia", 
                              ylab = "Respondentes",
                              col = "orange",
                              ylim = c(0,max(sort(casos_emprego)) + 100))
text(x = graph.casos_emprego, y = sort(casos_emprego), label = unname(sort(casos_emprego)), cex=1, pos=3)
axis(1, at=graph.casos_emprego, labels=paste("(", pct_emprego, ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)

# Dados do Gráfico 5
casos_tipo_ies<- table(dbf.xlsx$tipo_ies, exclude = NULL)
sort(casos_tipo_ies)
# Privada Pública 
#      10      77 

pct_tipo_ies <- paste(round(unname(sort(casos_tipo_ies)) / sum(unname(sort(casos_tipo_ies))) * 100), "%")
pct_tipo_ies
# [1] "11 %" "89 %"

#Gráfico 5: Quantidade de respondentes por tipo de mantenedora de instituição de ensino
# Gráfico tipo "pizza"
pie(sort(casos_tipo_ies),
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("red", "orange", "blue"),
    labels = paste(names(sort(casos_tipo_ies)), "-", pct_tipo_ies),
    main = "Gráfico 5: Respondentes por tipo de mantenedora")

# Dados do Gráfico 5
casos_estado <- table(dbf.xlsx$estado_reside, exclude = NULL)
casos_estado
# AM PR SP 
#  1  1 85 

pct_estado <- paste(round(unname(casos_estado) / sum(unname(casos_estado)) * 100), "%")
pct_estado  
# "1 %"  "1 %"  "98 %"

names(pct_estado) <-c("Amazonas", "Paraná", "São Paulo")

#Gráfico 5: Quantidade de respondentes por estado
# Gráfico tipo "pizza"
pie(casos_estado,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "green", "black"),
    labels = paste(names(pct_estado), "-", pct_estado),
    main = "Gráfico 5: Quantidade de respondentes por estado")

# Gráfico 6: Quantidade de respondentes por Instituição de Ensino Superior
# Dados do Gráfico 6
casos_ies <- table(dbf.xlsx$ies, exclude = NULL)
casos_ies
#       ETEC      FATEC        FGV        FIJ        FMJ      IMESB        ITE     SEBRAE       UFRJ     UFSCar      UNESP     UNIARA UNISAGRADO     UNOPAR        USP 
#          1          1          2          2          2          1          1          1          1          2         68          1          2          1          1 

pct_ies <- round(unname(casos_ies) / sum(unname(casos_ies)) * 100)
pct_ies
sem_unesp_ies = 100 - pct_ies[11]
sem_unesp_ies = paste(sem_unesp_ies, "%")
sem_unesp_ies
pct_ies <- paste(pct_ies, "%")
pct_ies  
#[1] "1 %"  "1 %"  "2 %"  "2 %"  "2 %"  "1 %"  "1 %"  "1 %"  "1 %"  "2 %"  "78 %" "1 %"  "2 %"  "1 %"  "1 %" 
 
# Rótulos muito grandes e dispersos. Dificulta a exibição nos gráficos!
# Redução proposital no tamanho dos textos
names(casos_ies) <- c("ETEC", "FATEC", "FGV", "FJaú", "FMJund", "IMESB", " ITE", 
                      "SEBRAE", "UFRJ", "UFSCar", "UNESP", "UNIARA", "UNISAG", 
                      "UNOPAR", "USP")
                          
# Gráfico 6 do tipo barra Instituição de Ensino Superior
graph.casos_ies <- barplot(casos_ies,
                               horiz = F,
                               main = "Gráfico 6: Quantidade de respondentes por Instituição de Ensino Superior",
                               xlab = "IES", 
                               ylab = "Respondentes",
                               col = "orange",
                               ylim = c(0,max(casos_ies) + 10)
)
text(x = graph.casos_ies, y = casos_ies, label = unname(casos_ies), cex=1, pos=3)
axis(1, at=graph.casos_ies, labels=paste("(", pct_ies , ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)

# Dados do Gráfico 7
casos_vacinado <- table(dbf.xlsx$vacinado, exclude = NULL)
casos_vacinado
#          Duas doses ou dose única Duas doses ou dose única e reforço                     Com a 1a. dose         N/A ou Não quero responder 
#                                27                                 53                                  1                                  6 

names(casos_vacinado) <- c("Duas doses ou dose única", 
                           "Duas doses ou dose única e reforço",
                           "Com a 1a. dose",
                           "N/A ou Não quero responder")
pct_vacinado <- paste(round(unname(casos_vacinado) / 
                              sum(unname(casos_vacinado)) * 100), "%")
pct_vacinado
#[1] "31 %" "61 %" "1 %"  "7 %"

total_vacina = round(sum(unname(casos_vacinado[1:2]))/sum(unname(casos_vacinado)) * 100, 1)
total_vacina
# [1] 92
#Gráfico 7: Está vacinado
# Gráfico tipo "pizza"
pie(casos_vacinado,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray",  "red", "blue","orange", "black", "white", "green"),
    labels = paste(names(casos_vacinado), "-", pct_vacinado),
    main = "Gráfico 7: Vacinados contra Covid-19")

# Dados do Gráfico 7
casos_nivel <- table(dbf.xlsx$nivel_ensino, exclude = NULL)
casos_nivel
# Doutorado     Ensino Técnico Especialização/MBA          Graduação           Mestrado      Pós-doutorado 
#        20                  1                  1                 46                 18                  1 

# Determinando o maior número de casos de nível de ensino
n <- length(casos_nivel)
n
#[1] 6
sort(casos_nivel,partial=n-0)[n-0] # maior quantidade de nível de ensino
#Graduação 
#       46 
names(sort(sort(casos_nivel,partial=n-0)[n-0]))
#[1] "Graduação"

pct_nivel <- paste(round(unname(casos_nivel) / sum(unname(casos_nivel)) * 100), "%")
pct_nivel  
#[1] "23 %" "1 %"  "1 %"  "53 %" "21 %" "1 %"

# Gráfico 7 do tipo barra respondentes por nível de graduação
graph.casos_nivel <- barplot(casos_nivel,
                           horiz = F,
                           main = "Gráfico 7: Quantidade de respondentes por Graduação",
                           xlab = "IES", 
                           ylab = "Respondentes",
                           col = "orange",
                           ylim = c(0,max(casos_nivel) + 10)
)
text(x = graph.casos_ies, y = casos_nivel, label = unname(casos_nivel), cex=1, pos=3)
axis(1, at=graph.casos_ies, labels=paste("(", pct_nivel , ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)

# Dados do Gráfico 8
casos_ensino<- table(dbf.xlsx$nivel_ensino, exclude = NULL)
casos_ensino
# Doutorado     Ensino Técnico Especialização/MBA          Graduação           Mestrado      Pós-doutorado 
#        20                  1                  1                 46                 18                  1  

pct_ensino <- paste(round(unname(casos_ensino) / sum(unname(casos_ensino)) * 100), "%")
pct_ensino  
#[1] "23 %" "1 %"  "1 %"  "53 %" "21 %" "1 %" 

#Gráfico 8: Quantidade de respondentes nível de ensino
# Gráfico tipo "pizza"
pie(casos_ensino,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "green", "black", "blue"),
    labels = paste(names(casos_ensino), "-", pct_ensino),
    main = "Gráfico 5: Quantidade de respondentes por graduação")


# Dados do Gráfico 9: Procedência do aluno
# Aluno é da cidade da IES ou fora?
casos_local_estudante <- sort(table(dbf.xlsx$local_estudante, exclude = NULL))
casos_local_estudante
#       Local Outra cidade 
#          30           57
names(casos_local_estudante) = c("Cidade da IES", "Outra cidade")
n = length(casos_local_estudante)
n
#[1] 2

pct_casos_local_estudante <- paste(round(unname(casos_local_estudante) / 
                                    sum(unname(casos_local_estudante)) * 100), "%")
pct_casos_local_estudante
#[[1] "34 %" "66 %"

names(casos_local_estudante)
#[1] "Cidade da IES" "Outra cidade"
unname(casos_local_estudante)
#[1] 30 57
unname(casos_local_estudante)[n]
#[1] 57
pct_casos_local_estudante[n]
#[1] "66 %"

#Gráfico 10: Aluno reside com quem
# Aluno é da cidade da IES ou fora?
# Gráfico tipo "pizza"
pie(casos_local_estudante,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("orange", "blue", "red","black"),
    labels = paste(names(casos_local_estudante), "-", pct_casos_local_estudante),
    main = "Gráfico 9: Procedência do aluno")

# Dados do Gráfico 10 Aluno reside com quem?
casos_aluno_mora_com <- sort(table(dbf.xlsx$morando_com, exclude = NULL))
casos_aluno_mora_com
#Colega de quarto/República                 Sozinho(a)                    Família 
#                         4                         14                         69
n = length(casos_aluno_mora_com)
n
#[1] 3
pct_aluno_mora_com <- paste(round(unname(casos_aluno_mora_com) / 
                                    sum(unname(casos_aluno_mora_com)) * 100), "%")

pct_aluno_mora_com
#[1] "5 %"  "16 %" "79 %"

#Gráfico 10: Aluno reside com quem?
# Gráfico tipo "pizza"
pie(casos_aluno_mora_com,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("orange", "blue", "red","black"),
    labels = paste(names(casos_aluno_mora_com), "-", pct_aluno_mora_com),
    main = "Gráfico 9: Aluno mora com/em")

# Dados do Gráfico 11
casos_fechar_ies <- sort(table(dbf.xlsx$decisao_fechar, exclude = NULL))
pct_fechar_ies <- paste(round(unname(casos_fechar_ies) / 
                                sum(unname(casos_fechar_ies)) * 100), "%")
pct_fechar_ies
# [1] "13 %" "14 %" "74 %"

n = length(casos_fechar_ies)
n
# [1] 3

#Gráfico 11: Fechar IES e utilizar ferramentas online
# Gráfico tipo "pizza"
pie(casos_fechar_ies,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red","blue", "gray","orange", "black", "white", "green"),
    labels = paste(names(casos_fechar_ies), "-", pct_fechar_ies),
    main = "Gráfico 11: Decisão de fechar IES e utilizar ferramentas online")

# Dados do Gráfico 12
casos_migrou_virtual <- sort(table(dbf.xlsx$migrou_virtual, exclude = NULL))
casos_migrou_virtual
# Não Sim 
#   1  86
pct_migrou_virtual <- paste(round(unname(casos_migrou_virtual) / 
                                    sum(unname(casos_migrou_virtual)) * 100), "%")
pct_migrou_virtual
# [1] "1 %"  "99 %"
n <- length(casos_migrou_virtual)
n
# [1] 2

# Dados do Gráfico 12
casos_acesso_infra_ies <- sort(table(dbf.xlsx$acesso_infra_ies, exclude = NULL))
casos_acesso_infra_ies
#            N/A ou Não sabe                    Melhorou Ficou mais ou menos o mesmo 
#                         10                          11                          30 
#                     Piorou 
#                         36 

pct_acesso_infra_ies <- paste(round(unname(casos_acesso_infra_ies) / 
                                      sum(unname(casos_acesso_infra_ies)) * 100), "%")
pct_acesso_infra_ies
#[1] "11 %" "13 %" "34 %" "41 %"

n <- length(casos_acesso_infra_ies)
n
# [1] 4

#Gráfico 12: Acesso à infra-estrutura da IES
# Gráfico tipo "pizza"
pie(casos_acesso_infra_ies,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red", "black", "white", "green"),
    labels = paste(names(casos_acesso_infra_ies), "-", pct_acesso_infra_ies),
    main = "Gráfico 12: Acesso à infra-estrutura da IES")

# Dados do Gráfico 13
casos_acesso_professores <- sort(table(dbf.xlsx$acesso_professores, exclude = NULL))
casos_acesso_professores
#          N/A ou Não sabe                  Melhorou Foi mais ou menos o mesmo 
#                        4                        14                        28 
#                   Piorou 
#                       41

pct_acesso_professores <- paste(round(unname(casos_acesso_professores) / 
                                        sum(unname(casos_acesso_professores)) * 100), "%")
pct_acesso_professores
# [1] "5 %"  "16 %" "32 %" "47 %"

n <- length(casos_acesso_professores)
n
#[1] 4

#Gráfico 13: Acesso aos professores na pandemia
# Gráfico tipo "pizza"
pie(casos_acesso_professores,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red", "black", "white", "green"),
    labels = paste(names(casos_acesso_professores), "-", pct_acesso_professores),
    main = "Gráfico 13: Acesso aos professores na pandemia")

# Dados do Gráfico 14
casos_aulas <- sort(table(dbf.xlsx$aulas_durante_pandemia, exclude = NULL))
casos_aulas
#            N/A ou Não sabe                  Melhoraram                    Pioraram 
#                          4                           7                          37 
#Foram mais ou menos o mesmo 
#                         39 
pct_aulas <- paste(round(unname(casos_aulas) / 
                           sum(unname(casos_aulas)) * 100), "%")
pct_aulas
#[1] "5 %"  "8 %"  "43 %" "45 %"

n <- length(casos_aulas)
n
#[1] 4

#Gráfico 14: Forma de ministrar as aulas
# Gráfico tipo "pizza"
pie(casos_aulas,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red", "black", "white", "green"),
    labels = paste(names(casos_aulas), "-", pct_aulas),
    main = "Gráfico 14: Modo de lecionar durante a pandemia")

# Dados do Gráfico 15
casos_ies_reinicio <- sort(table(dbf.xlsx$ies_reinicio, exclude = NULL))
casos_ies_reinicio
#  N/A ou Não sabe 
#                                                          10 
#        Não, ainda não retornou nenhuma atividade presencial 
#                                                          21 
#Em parte (apenas algumas atividades  presenciais retornaram) 
#                                                          24 
#              Sim (retornou todas as atividades presenciais) 
#                                                          32
#names(casos_ies_reinicio) <- c("Em parte", "N/A ou Não sabe", "Não retornou", "Retornou todas atividades")

pct_ies_reinicio <- paste(round(unname(casos_ies_reinicio) / 
                                  sum(unname(casos_ies_reinicio)) * 100), "%")
pct_ies_reinicio
#[1] "11 %" "24 %" "28 %" "37 %"
n <- length(casos_ies_reinicio)
n
#[1] 4

#Gráfico 15: IES reiniciou atividades presenciais
# Gráfico tipo "pizza"
pie(casos_ies_reinicio,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray",  "red", "blue","orange", "black", "white", "green"),
    labels = paste(names(casos_ies_reinicio), "-", pct_ies_reinicio),
    main = "Gráfico 15: IES reiniciou atividades presenciais")

# Dados do Gráfico 16
casos_acesso_internet <- sort(table(dbf.xlsx$acesso_internet, exclude = NULL))
casos_acesso_internet
#Muito melhor do que antes           N/A ou Não sabe      Muito pior que antes            Pior que antes 
#                        1                         1                         4                        10 
#Melhor do que antes         O mesmo que antes 
#                 17                        54 

pct_acesso_internet <- paste(round(unname(casos_acesso_internet) / 
                                     sum(unname(casos_acesso_internet)) * 100), "%")
pct_acesso_internet
#[1] "1 %"  "1 %"  "5 %"  "11 %" "20 %" "62 %"
n <- length(casos_acesso_internet)
n
#[1] 6

#Gráfico 16: Acesso aos serviços de internet
# Gráfico tipo "pizza"
pie(casos_acesso_internet,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("white", "orange", "blue", "red","black", "gray"),
    labels = paste(names(casos_acesso_internet), "-", pct_acesso_internet),
    main = "Gráfico 16: Acesso aos serviços de internet")

# Dados do Gráfico 17
casos_performance <- sort(table(dbf.xlsx$desempenho_escolar, exclude = NULL))
casos_performance
#          N/A ou Não sabe                  Aumentou                  Diminuiu Foi mais ou menos o mesmo 
#                        2                        23                        27                        35
pct_performance <- paste(round(unname(casos_performance) / 
                                 sum(unname(casos_performance)) * 100), "%")
pct_performance
#[1] "2 %"  "26 %" "31 %" "40 %"
n <- length(casos_performance)
n
#[1] 4

#Gráfico 17: Desempenho escolar
# Gráfico tipo "pizza"
pie(casos_performance,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray",  "red", "blue","orange", "black", "white", "green"),
    labels = paste(names(casos_performance), "-", pct_performance),
    main = "Gráfico 17: Desempenho escolar na pandemia")

# Dados do Gráfico 18
casos_aluno_vivenciou <- sort(table(dbf.xlsx$vivenciou, exclude = NULL))
names(casos_aluno_vivenciou) <- c("Discriminação por desconhecidos", "Ajuda/assistência de desconhecidos", 
                                  "Alterações nas condições de vida", "Não se aplica",
                                  "Dificuldade se deslocar/viajar")
casos_aluno_vivenciou
#   Discriminação por desconhecidos Ajuda/assistência de desconhecidos   Alterações nas condições de vida 
#                                 1                                  6                                 16 
#                     Não se aplica     Dificuldade se deslocar/viajar 
#                                31                                 33

pct_aluno_vivenciou <- paste(round(unname(casos_aluno_vivenciou) / 
                                     sum(unname(casos_aluno_vivenciou)) * 100), "%")

pct_aluno_vivenciou
#[1] "1 %"  "7 %"  "18 %" "36 %" "38 %"
n = length(casos_aluno_vivenciou)
n
#[1] 5
#Gráfico 18: Dificuldades vividas pelo aluno
# Gráfico tipo "pizza"
pie(casos_aluno_vivenciou,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red","black"),
    labels = paste(names(casos_aluno_vivenciou), "-", pct_aluno_vivenciou),
    main = "Gráfico 18: Dificuldades vividas pelo aluno")

# Gráfico tipo "barra"
graph.casos_aluno_vivenciou <- barplot(casos_aluno_vivenciou, 
                               main = "Gráfico 18: Dificuldades vividas pelo aluno",
                               xlab = "Dificuldades", 
                               ylab = "Respondentes",
                               horiz = F,
                               col = "orange",
                               ylim = c(0,max(casos_aluno_vivenciou) + 50))
text(x = graph.casos_aluno_vivenciou, y = unname(casos_aluno_vivenciou), label = names(casos_aluno_vivenciou), cex=1, pos=3)
axis(1, at=graph.casos_aluno_vivenciou, labels=paste("(", pct_aluno_vivenciou, ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)

# Dados do Gráfico 19
casos_despesas <- sort(table(dbf.xlsx$despesas, exclude = NULL))
casos_despesas
#   N/A ou Não quero responder                    Diminuiram Foram mais ou menos os mesmos                    Aumentaram 
#                            1                            22                            27                            37
pct_despesas <- paste(round(unname(casos_despesas) / 
                              sum(unname(casos_despesas)) * 100), "%")
pct_despesas
#[1] "43 %" "25 %" "31 %" "1 %" 

n = length(casos_despesas)
n

#Gráfico 19: Despesas durante a pandemia
# Gráfico tipo "pizza"
pie(casos_despesas,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red","gray", "blue","orange", "black", "white", "green"),
    labels = paste(names(casos_despesas), "-", pct_despesas),
    main = "Gráfico 19: Despesas durante a pandemia")

# Dados do Gráfico 21
casos_renda <- table(dbf.xlsx$renda_financeira, exclude = NULL)
pct_renda <- paste(round(unname(casos_renda) / 
                           sum(unname(casos_renda)) * 100), "%")

n=length(casos_renda)
#Gráfico 21: Renda durante a pandemia
# Gráfico tipo "pizza"
pie(casos_renda,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray", "red", "blue","orange", "black", "white", "green"),
    labels = paste(names(casos_renda), "-", pct_renda),
    main = "Gráfico 21: Renda durante a pandemia")


# Dados do Gráfico 21
casos_ajuda <- sort(table(dbf.xlsx$ajuda_financeira, exclude = NULL))
casos_ajuda
#Sim Não 
# 13  74
pct_ajuda <- paste(round(unname(casos_ajuda) / 
                           sum(unname(casos_ajuda)) * 100), "%")
pct_ajuda
#[1] "15 %" "85 %"
n = length(casos_ajuda)
n
#[1] 2
#Gráfico 21: Recebeu auxílio financeiro da IES
# Gráfico tipo "pizza"
pie(casos_ajuda,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red","blue", "gray","orange", "black", "white", "green"),
    labels = paste(names(casos_ajuda), "-", pct_ajuda),
    main = "Gráfico 21: Recebeu auxílio financeiro da IES")

# Dados do Gráfico 22
casos_endividamento <- sort(table(dbf.xlsx$nivel_endividamento, exclude = NULL))
casos_endividamento
#N/A ou Não sabe                    Diminuiram                    Aumentaram Estão mais ou menos as mesmas 
#              4                            11                            16                            56
pct_endividamento <- paste(round(unname(casos_endividamento) / 
                                   sum(unname(casos_endividamento)) * 100), "%")
pct_endividamento
#[1] "5 %"  "13 %" "18 %" "64 %"
n = length(casos_endividamento)
n
#[1] 4
#Gráfico 22: Dívidas durante pandemia
# Gráfico tipo "pizza"
pie(casos_endividamento,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red","blue", "gray","orange", "black", "white", "green"),
    labels = paste(names(casos_endividamento), "-", pct_endividamento),
    main = "Gráfico 22: Dívidas durante a pandemia")

## Gráfico 23 
# Descobrindo as ocorrências
if(!("stringr") %in% installed.packages()) install.packages("stringr")
library(stringr)
perfil_despesas <- dbf.xlsx$despesas_cresceram # selecionando todos os casos da variável despesas cresceram
relacionadas_saude <- length(na.omit(str_match(perfil_despesas, "Relacionadas com saúde")))
viagens_deslocamentos <- length(na.omit(str_match(perfil_despesas, "Viagens/deslocamentos")))
transporte_urbano <- length(na.omit(str_match(perfil_despesas, "Transporte urbano")))
aluguel <- length(na.omit(str_match(perfil_despesas, "Aluguel")))
internet <- length(na.omit(str_match(perfil_despesas, "Internet")))
alimentacao <- length(na.omit(str_match(perfil_despesas, "Alimentação")))  
outras <- length(na.omit(str_match(perfil_despesas, "Outras")))
casos_perfil_despesas <- c(relacionadas_saude, viagens_deslocamentos, transporte_urbano, aluguel, internet, alimentacao, outras)
casos_perfil_despesas
#[1] 29 42 32 32 35 66 27
names(casos_perfil_despesas) <- c("Relac. saúde", "Deslocamento", "Transp. urbano", 
                                  "Aluguel", "Internet", "Alimentação", "Outras")
casos_perfil_despesas
casos_perfil_despesas <- sort(casos_perfil_despesas)
pct_perfil_despesas <- paste0(round(unname(casos_perfil_despesas) / sum(unname(casos_perfil_despesas)) * 100,0), "%")
pct_perfil_despesas
#[1] "10%" "11%" "12%" "12%" "13%" "16%" "25%"
n = length(casos_perfil_despesas)
n
#[1] 7
# Gráfico do tipo barras
graph.perfil.despesas <- barplot(casos_perfil_despesas, 
                                 main = "Gráfico 23: Despesas que cresceram na pandemia",
                                 xlab = "Despesas", 
                                 ylab = "Quantidade",
                                 col = "orange",
                                 ylim = c(0,max(casos_perfil_despesas) + 30))
text(x = graph.perfil.despesas, y = casos_perfil_despesas, label = unname(casos_perfil_despesas), cex=1, pos=3)
axis(1, at=graph.perfil.despesas, labels=paste("(", pct_perfil_despesas, ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)      


# Gráfico 24: situacao_durante-pandemia
# Nuvem de palavras

#Load the packages
if(!"wordcloud" %in% installed.packages()) install.packages("wordcloud")
library(wordcloud)
if(!"wordcloud2" %in% installed.packages()) install.packages("wordcloud2")
library(wordcloud2)
if(!"RColorBrewer" %in% installed.packages()) install.packages("RColorBrewer")
library(RColorBrewer)
if(!"tm" %in% installed.packages()) install.packages("tm")
library(tm)

#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dbf.xlsx$situação_durante_pandemia)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
# Para utilizar o comando "pipe" (%>%) ou operador "tee pipe" (%T>%) , pode-se "carregar" o pacote magrittr
if(!"magrittr" %in% installed.packages()) install.packages("magrittr")
library(magrittr)
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matriz de termos de documento
# Uma matriz de termos de documento é uma matriz matemática que descreve a frequência 
# dos termos que ocorrem em uma coleção de documentos.
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, # menor "min.freq", maior/precisão, menor número/palavras     
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))
mtext("Gráfico 24: Manifestação sobre a sua situação durante a pandemia", side=3, line = 3)
print("Principais ocorrências de palavras em situação durante a pandemia")
for (k in 1:length (which(pmax(df$freq) > 2))) {
  print(paste(df$word[k], df$freq[k],sep = " = " ))
}

#Gráfico 25: Qualidade de vida
#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dbf.xlsx$qualidade_de_vida)
# Criando um corpus  
docs <- Corpus(VectorSource(texto))
# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))
# Criar uma matrix de termos de documento
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, # maior "min.freq", maior/precisão, menor número/palavras     
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))
mtext("Gráfico 25: Manifestação sobre a sua qualidade de vida", side=3, line = 3)
print("Principais ocorrências de palavras em qualidade de vida")
for (k in 1:length (which(pmax(df$freq) > 2))) {
  print(paste(df$word[k], df$freq[k],sep = " = " ))
}

#Gráfico 26: IES fez de positivo na pandemia
#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dbf.xlsx$ies_positivo)
# Criando um corpus  
docs <- Corpus(VectorSource(texto))
# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))
# Criar uma matrix de termos de documento
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, # maior "min.freq", maior/precisão, menor número/palavras     
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))
mtext("Gráfico 26: O que a IES fez de positivo na pandemia", side=3, line = 3)
print("Principais ocorrências de palavras do que a IES fez de positivo na pandemia")
for (k in 1:length (which(pmax(df$freq) > 2))) {
  print(paste(df$word[k], df$freq[k],sep = " = " ))
}

#Gráfico 27: IES poderia melhorar na pandemia
#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dbf.xlsx$ies_melhorar)
# Criando um corpus  
docs <- Corpus(VectorSource(texto))
# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))
# Criar uma matrix de termos de documento
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
# Gerar a núvem de palavras
#set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, # maior "min.freq", maior/precisão, menor número/palavras     
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))
mtext("Gráfico 27: O que a IES poderia melhorar na pandemia", side=3, line = 3)
print("Principais ocorrências de palavras do que a IES poderia melhorar na pandemia")
for (k in 1:length (which(pmax(df$freq) > 2))) {
  print(paste(df$word[k], df$freq[k],sep = " = " ))
}

#Gráfico 28: IES poderia melhor ajudar na pandemia
#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dbf.xlsx$ies_ajudar)
# Criando um corpus  
docs <- Corpus(VectorSource(texto))
# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))
# Criar uma matrix de termos de documento
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, # maior "min.freq", maior/precisão, menor número/palavras     
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))
mtext("Gráfico 28: Em que a IES poderia melhor ajudar na pandemia", side=3, line = 3)
print("Principais ocorrências de palavras do que a IES poderia melhora ajudar na pandemia")
for (k in 1:length (which(pmax(df$freq) > 2))) {
  print(paste(df$word[k], df$freq[k],sep = " = " ))
}


# Dados do Gráfico 29
casos_prosseguir_estudos <- table(dbf.xlsx$capacidade_prosseguir_estudos, exclude = NULL)
pct_prosseguir_estudos <- paste(round(unname(casos_prosseguir_estudos) / 
                                        sum(unname(casos_prosseguir_estudos)) * 100), "%")
#Gráfico 30: Capacidade prosseguir estudos
# Gráfico tipo "pizza"
pie(casos_prosseguir_estudos,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red","black", "white", "green"),
    labels = paste(names(casos_prosseguir_estudos), "-", pct_prosseguir_estudos),
    main = "Gráfico 29: Capacidade para prosseguir ou concluir estudos")


# Dados do Gráfico 30
casos_socializacao <- table(dbf.xlsx$capacidade_socializacao, exclude = NULL)
pct_socializacao <- paste(round(unname(casos_socializacao) / 
                                  sum(unname(casos_socializacao)) * 100), "%")
#Gráfico 30: Capacidade de socialização
# Gráfico tipo "pizza"
pie(casos_socializacao,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red","black", "white", "green"),
    labels = paste(names(casos_socializacao), "-", pct_socializacao),
    main = "Gráfico 30: Capacidade de socialização")

# Dados do Gráfico 31
casos_bem_estar <- table(dbf.xlsx$bem_estar_psicologico, exclude = NULL)
pct_bem_estar <- paste(round(unname(casos_bem_estar) / 
                               sum(unname(casos_bem_estar)) * 100), "%")
#Gráfico 31: Bem-estar psicológico
# Gráfico tipo "pizza"
pie(casos_bem_estar,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "white", "black", "red",  "green"),
    labels = paste(names(casos_bem_estar), "-", pct_bem_estar),
    main = "Gráfico 31: Bem-estar psicológico")

# Dados do Gráfico 32
casos_espaco_fisico <- table(dbf.xlsx$espaco_físico, exclude = NULL)
pct_espaco_fisico <- paste(round(unname(casos_espaco_fisico) / 
                                   sum(unname(casos_espaco_fisico)) * 100), "%")
#Gráfico 32: Espaço físico utilizado para atividades escolares
# Gráfico tipo "pizza"
pie(casos_espaco_fisico,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red", "black", "white", "green"),
    labels = paste(names(casos_espaco_fisico), "-", pct_espaco_fisico),
    main = "Gráfico 32: Espaço físico utilizado para atividades escolares")

# Dados do Gráfico 33
casos_disposicao <- table(dbf.xlsx$disposicao_atividades, exclude = NULL)
pct_disposicao <- paste(round(unname(casos_disposicao) / 
                                sum(unname(casos_disposicao)) * 100), "%")
#Gráfico 33: Disposição para participar das atividades escolares
# Gráfico tipo "pizza"
pie(casos_disposicao,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray",  "red", "blue","orange", "black", "white", "green"),
    labels = paste(names(casos_disposicao), "-", pct_disposicao),
    main = "Gráfico 33: Disposição para participar das atividades escolares")


# Dados do Gráfico 34
casos_ansiedade <- table(dbf.xlsx$nivel_ansiedade, exclude = NULL)
pct_aniedade <- paste(round(unname(casos_ansiedade) / 
                              sum(unname(casos_ansiedade)) * 100), "%")
#Gráfico 34: Nível de ansiedade para o futuro
# Gráfico tipo "pizza"
pie(casos_ansiedade,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("white","blue", "gray","orange", "black", "red", "green"),
    labels = paste(names(casos_ansiedade), "-", pct_aniedade),
    main = "Gráfico 34:  Nível de ansiedade quanto ao futuro")


# Dados do Gráfico 35
casos_ansiedade_planejamento <- table(dbf.xlsx$ansiedade_planejamento, exclude = NULL)
pct_ansiedade_planejamento <- paste(round(unname(casos_ansiedade_planejamento) / 
                                            sum(unname(casos_ansiedade_planejamento)) * 100), "%")
#Gráfico 35: Nível de ansiedade para planejamento pessoal
# Gráfico tipo "pizza"
pie(casos_ansiedade_planejamento,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("black","blue", "gray","orange", "red", "white", "green"),
    labels = paste(names(casos_ansiedade_planejamento), "-", pct_ansiedade_planejamento),
    main = "Gráfico 35: Nível de ansiedade para planejamento pessoal")

# Dados do Gráfico 36
casos_ansiedade_longo <- table(dbf.xlsx$ansiedade_longo_prazo, exclude = NULL)
pct_ansiedade_longo <- paste(round(unname(casos_ansiedade_longo) / 
                                     sum(unname(casos_ansiedade_longo)) * 100), "%")
#Gráfico 36: Nível de ansiedade para planejamento a longo prazo
# Gráfico tipo "pizza"
pie(casos_ansiedade_longo,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("black","blue", "gray","orange", "red", "white", "green"),
    labels = paste(names(casos_ansiedade_longo), "-", pct_ansiedade_longo),
    main = "Gráfico 36: Nível de ansiedade para planejamento a longo prazo")


#Gráfico 37: Dificuldades financeiras durante a pandemia
#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dbf.xlsx$dificuldades_financeiras)
# Criando um corpus  
docs <- Corpus(VectorSource(texto))
# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))
# Criar uma matrix de termos de documento
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, # maior "min.freq", maior/precisão, menor número/palavras     
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))
mtext("Gráfico 37: Dificuldades financeiras durante a pandemia", side=3, line = 3)
print("Principais ocorrências de palavras das dificuldades financeiras durante a pandemia")
for (k in 1:length (which(pmax(df$freq) > 2))) {
  print(paste(df$word[k], df$freq[k],sep = " = " ))
}


#Gráfico 38: Dificuldades acadêmicas durante a pandelia
#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dbf.xlsx$dificuldades_academicas)
# Criando um corpus  
docs <- Corpus(VectorSource(texto))
# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))
# Criar uma matrix de termos de documento
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, # maior "min.freq", maior/precisão, menor número/palavras     
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))
mtext("Gráfico 38: Dificuldades acadêmicas durante a pandemia", side=3, line = 3)
print("Principais ocorrências de palavras das dificuldades acadêmicas durante a pandemia")
for (k in 1:length (which(pmax(df$freq) > 2))) {
  print(paste(df$word[k], df$freq[k],sep = " = " ))
}

#Gráfico 39: Comentários finais
#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dbf.xlsx$detalhes_finais)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, # maior "min.freq", maior/precisão, menor número/palavras     
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))
mtext("Gráfico 39: Comentários finais", side=3, line = 3)
print("Principais ocorrências de palavras nos comentários finais")
for (k in 1:length (which(pmax(df$freq) > 2))) {
  print(paste(df$word[k], df$freq[k],sep = " = " ))
}

# Dados do Gráfico 40
casos_acesso_saude <- table(dbf.xlsx$acesso_servicos_saude, exclude = NULL)
pct_acesso_saude <- paste(round(unname(casos_acesso_saude) / 
                                  sum(unname(casos_acesso_saude)) * 100), "%")
#Gráfico 40: Acesso aos serviços de saude
# Gráfico tipo "pizza"
pie(casos_acesso_saude,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red","black"),
    labels = paste(names(casos_acesso_saude), "-", pct_acesso_saude),
    main = "Gráfico 40: Acesso aos serviços de saude")


