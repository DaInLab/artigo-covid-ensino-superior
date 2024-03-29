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
  }}

casos_idade_concat <- table(idade_concat$faixa_etaria)
casos_idade_concat
#de 17 a 21 anos de 22 a 26 anos de 27 a 31 anos de 32 a 36 anos de 37 a 41 anos de 42 a 46 anos de 47 a 51 anos 
#             11              14               3               3               7               5               4 
#de 52 a 56 anos de 57 a 61 anos 
#              4               1

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
#Casado(a)   Divorciado(a)/Separado(a)                 Solteiro(a) União Estável/Vivendo junto 
#       20                           3                          55                           8 
# Viúvo(a) 
#        1 
 
pct_conjugal <- paste(round(unname(casos_conjugal) / sum(unname(casos_conjugal)) * 100), "%")
pct_conjugal  
#[1] "23 %" "3 %"  "63 %" "9 %"  "1 %" 

#Gráfico 3: Quantidade de respondentes por sutuação conjugal
# Gráfico tipo "pizza"
pie(casos_conjugal,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "green", "black"),
    labels = paste(names(casos_conjugal), "-", pct_conjugal),
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

pct_emprego <- paste(round(unname(casos_emprego) / sum(unname(casos_emprego)) * 100), "%")
pct_emprego
#[1] "1 %"  "5 %"  "14 %" "20 %" "5 %"  "47 %" "8 %"  "1 %"  

#Gráfico 4: Quantidade de respondentes por situação empregatícia
# Gráfico tipo "pizza"
pie(casos_emprego,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "green", "black"),
    labels = paste(names(casos_emprego), "-", pct_emprego),
    main = "Gráfico 4: Quantidade de respondentes por sitação empregatícia")

# Gráfico 4 do tipo barra sitação empregatícia
graph.casos_emprego <- barplot(casos_emprego, 
                              main = "Gráfico 4: Quantidade de respondentes por sitação empregatícia",
                              xlab = "Sitação Empregatícia", 
                              ylab = "Respondentes",
                              col = "orange",
                              ylim = c(0,max(casos_emprego) + 100))
text(x = graph.casos_emprego, y = casos_emprego, label = unname(casos_emprego), cex=1, pos=3)
axis(1, at=graph.casos_emprego, labels=paste("(", pct_emprego, ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)

# Dados do Gráfico 5
casos_tipo_ies<- table(dbf.xlsx$tipo_ies, exclude = NULL)
casos_tipo_ies
# Privada Pública 
#      10      77 

pct_tipo_ies <- paste(round(unname(casos_tipo_ies) / sum(unname(casos_tipo_ies)) * 100), "%")
pct_tipo_ies
# [1] "11 %" "89 %"

#Gráfico 5: Quantidade de respondentes por tipo de mantenedora de instituição de ensino
# Gráfico tipo "pizza"
pie(casos_tipo_ies,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("red", "orange", "blue"),
    labels = paste(names(casos_tipo_ies), "-", pct_tipo_ies),
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
names(casos_vacinado) <- c("Duas doses ou dose única", 
                           "Duas doses ou dose única e reforço",
                           "Com a 1a. dose",
                           "N/A ou Não quero responder")
pct_vacinado <- paste(round(unname(casos_vacinado) / 
                              sum(unname(casos_vacinado)) * 100), "%")
pct_vacinado
#[1] "31 %" "61 %" "1 %"  "7 %"

#Gráfico 7: Está vacinado
# Gráfico tipo "pizza"
pie(casos_vacinado,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray",  "red", "blue","orange", "black", "white", "green"),
    labels = paste(names(casos_vacinado), "-", pct_vacinado),
    main = "Gráfico 7: Vacinação contra Covid-19")

# Dados do Gráfico 7
casos_nivel <- table(dbf.xlsx$nivel_ensino, exclude = NULL)
casos_nivel
# Doutorado     Ensino Técnico Especialização/MBA          Graduação           Mestrado      Pós-doutorado 
#        20                  1                  1                 46                 18                  1 

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
