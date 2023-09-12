# - ETAPA INICIAL
#--- Importação e preparação dos dados
# versão inicial em 05/01/2023.
# versão do projeto "Impacto da Covid 19 nos Estudantes de ENsino Superior" no GitHub


# Instalando pacotes
if (!("ggplot2") %in% installed.packages()) install.packages("ggplot2")
library(ggplot2)

if (!("dplyr") %in% installed.packages()) install.packages("dplyr")
library(dplyr)

if(!("stringr") %in% installed.packages()) install.packages("stringr")
library(stringr)

if (!("readxl") %in% installed.packages()) install.packages("readxl")
library(readxl)

# Lendo o arquivo em .xlsx
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

## Gráfico 1 Quantidade de Respondentes por Faixa Etaria
idade_concat <- data.frame(idade=dbf.xlsx$idade, faixa_etaria="")

for (k in 1:nrow(idade_concat)) {
  if(is.na(idade_concat$idade[k])) {idade_concat$faixa_etaria[k] <- "Não Respondeu" 
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
#de 17 a 21 anos de 22 a 26 anos de 27 a 31 anos de 32 a 36 anos de 37 a 41 anos de 42 a 46 anos de 47 a 51 anos de 52 a 56 anos de 57 a 61 anos 
#             23              25               8               5               8               5               6               4               2 
#             ND 
#              1

df_idade <- as.data.frame(casos_idade_concat) 
df_idade

df_idade[1] <- c('Age range: 17-21 years',
                 'Age range: 22-26 years',
                 'Age range: 27-31 years',
                 'Age range: 32-36 years',
                 'Age range: 37-41 years',
                 'Age range: 42-46 years',
                 'Age range: 47-51 years',
                 'Age range: 52-56 years',
                 'Age range: 57-61 years',
                 'Non-response')


ggplot(df_idade, aes(x=Var1, y=Freq))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=Freq),nudge_y=0.9)+
  labs(x='', y='Number of Respondents')+
  ggtitle('')+
  theme_minimal()+
  coord_flip()

# Gráfico 2: Quantidade de respondentes por sexo
# Gráfico tipo "Barras"
df_genero <- dbf.csv %>% 
  count(genero) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_genero[1] <- c('Non-response','Female','LGBTQ+', 'Male'); 

df_genero

ggplot(df_genero, aes(x=reorder(genero, -n), y=n))+
  geom_col(width=.6, fill= c('gray','pink', 'orange', 'blue'), col='black')+
  geom_text(aes(label=paste(n,' (', perc, '%)')),nudge_y=3.5)+
  labs(x='', y='Number of Respondents', title='')+
  theme_minimal()
  
#Gráfico 3: Quantidade de respondentes por situação conjugal
df_conjugal <- dbf.csv %>% 
  count(situacao_conjugal) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_conjugal[1,1] <- 'Nao Respondeu'
df_conjugal[1] <- c('Non-response',
                    'Married',
                    'Divorced',
                    'Single',
                    'Living together',
                    'Widowed')

df_conjugal

ggplot(df_conjugal, aes(x=reorder(situacao_conjugal,-n), y=n))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n,'(', perc, '%)')),nudge_y=4.1, size = 2.3)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()+
  coord_flip()

#Gráfico 4: Quantidade de respondentes por situação empregatícia
df_emprego <- dbf.csv %>% 
  count(situacao_empregaticia) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_emprego[1,1] <- 'Nao Respondeu'

# Empregado : Servidora publica, Autonomo, Estagiario, Bolsista
# Desempregado : 
# Nao respondeu :
# Aposentado

# Empregado : Employed
# Desempregado : Unemployed
# Nao respondeu : Non-response
# Aposentado : Retired


df_emprego

ggplot(df_emprego, aes(x=reorder(situacao_empregaticia, -n), y=n))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n,' (', perc, '%)')),nudge_y=2.5, size = 2.7)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()+
  coord_flip()

#Gráfico 5: Quantidade de respondentes por tipo de mantenedora de instituição de ensino
# Gráfico tipo "barras"
df_tipo_ies <- dbf.csv %>% 
  count(tipo_ies) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_tipo_ies[1,1] <- 'Nao Respondeu'

df_tipo_ies
df_tipo_ies[1] <- c('Non-response',
                    'Private Institutions',
                    'Public Institutions')

ggplot(df_tipo_ies, aes(x=reorder(tipo_ies, -perc), y=perc))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n,' (', perc, '%)')),nudge_y=6)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()

#Gráfico 6: Quantidade de respondentes por estado
# Gráfico tipo "barras"
df_estado <- dbf.csv %>% 
  count(estado_reside) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_estado[1] <- c('Nao Respondeu','Amazonas','Paraná','São Paulo')

ggplot(df_estado, aes(x=estado_reside, y=n))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n,' (', perc, '%)')),nudge_y=6)+
  labs(x='', y='Qtde.Respondentes')+
  ggtitle('Gráfico 6: Quantidade de respondentes por Estado')+
  theme_minimal()

# Gráfico 7: Quantidade de respondentes por Instituição de Ensino Superior
df_ies <- dbf.csv  %>% 
  mutate(ies=gsub(" ","",ies)) %>% 
  count(ies) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_ies[1,1] <- 'Nao Respondeu'

df_ies

ggplot(df_ies, aes(x=ies, y=n))+
  geom_col(width=.6, fill=alpha('blue',3), col='black')+
  geom_text(aes(label=paste(n,' (', perc, '%)')),nudge_y=4, size=3)+
  labs(x='Instituições de Ensino', y='Qtde.Respondentes')+
  ggtitle('Gráfico 7: Quantidade de respondentes por Instituição de Ensino Superior')+
  theme_minimal()+
  coord_flip()

# Gráfico 8: Vacinados Covid-19
df_vacina <- dbf.csv  %>% 
  count(vacinado) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_vacina[1] <- c("Não Respondeu",
                  "Duas doses ou Única", 
                  "Duas doses ou Única e reforço",
                  "Com a 1a. dose")

# Vacinados : "Duas doses ou Única", 
              "Duas doses ou Única e reforço",
              "Com a 1a. dose"
# Nao Respondeu : Non-response              
              
# Vacinados : Vaccinated
#               

df_vacina

ggplot(df_vacina, aes(x=vacinado, y=perc))+
  geom_col(width=.6, fill=alpha('darkred',4), col='black')+
  geom_text(aes(label=paste(n,' (', perc, '%)')),nudge_y=3.8)+
  labs(x='', y='Qtde. Respondentes')+
  theme_minimal()

# Gráfico 9 do tipo barra respondentes por nível de graduação
df_nivel_ensino <- dbf.csv  %>% 
  count(nivel_ensino) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_nivel_ensino[1,1] <- 'Nao Respondeu'

df_nivel_ensino[1] <- c('Non-response',
                        'PHD',
                        'Technical Education',
                        'Specialization/MBA',
                        'Bachelors',
                        'Masters',
                        'Post-doctorate')

df_nivel_ensino

ggplot(df_nivel_ensino, aes(x=nivel_ensino, y=n))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n,' (', perc, '%)')),nudge_y=2.9, size=2.7)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()+
  coord_flip()


# Gráfico 10 do tipo barra respondentes por Procedencia Aluno
df_local_estudante <- dbf.csv  %>% 
  count(local_estudante) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_local_estudante[1] <- c("Non-response",
                           "Local of University", 
                           "Another Local")

df_local_estudante

ggplot(df_local_estudante, aes(x=reorder(local_estudante, -n), y=n))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, ' (', perc, '%)')),nudge_y=4)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()


# Gráfico 11 do tipo barra respondentes : aluno mora com quem
df_morando_com <- dbf.csv  %>% 
  count(morando_com) %>% 
  mutate(perc=round(n/sum(n)*100,1))


df_morando_com[1] <- c('Non-response',
                       'Roommate',
                       'Family',
                       'Alone')
df_morando_com

ggplot(df_morando_com, aes(x=reorder(morando_com, -perc), y=perc))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(perc,"%")),nudge_y=6)+
  labs(x = '', y= 'Number of Respondents')+
  theme_minimal()

# Gráfico 12 do tipo barra respondentes : IES Decidiu Fechar
df_decisao_fechar <- dbf.csv  %>% 
  count(decisao_fechar) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_decisao_fechar[1] <- c('Non-response',
                          'Prudently',
                          'Slowly',
                          'Quickly')

df_decisao_fechar

ggplot(df_decisao_fechar, aes(x=reorder(decisao_fechar, -perc), y=perc))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, ' (', perc, '%)')),nudge_y=4)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()

# Gráfico 13 do tipo barra respondentes : IES Migrou Virtual
df_migrou_virtual <- dbf.csv  %>% 
  count(migrou_virtual) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_migrou_virtual[1] <- c('Non-response',
                          'No',
                          'Yes')

df_migrou_virtual

ggplot(df_migrou_virtual, aes(x=reorder(migrou_virtual, -perc), y=perc))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, ' (', perc, '%)')),nudge_y=4)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()

# Gráfico 14 Acesso à infra-estrutura da IES
df_acesso_infra_ies <- dbf.csv  %>% 
  count(acesso_infra_ies) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_acesso_infra_ies[1] <- c('Non-response',
                            'Unchanged',
                            'Increased',
                            'Don\'t Know',
                            'Declined')

df_acesso_infra_ies

ggplot(df_acesso_infra_ies, aes(x=reorder(acesso_infra_ies, -perc), y=perc))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(perc,"%")),nudge_y=2)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()

# Gráfico 15 Acesso aos Professores na pandemia
df_acesso_professores <- dbf.csv  %>% 
  count(acesso_professores) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_acesso_professores[1] <- c('Non-response',
                            'Unchanged',
                            'Improved',
                            'Don t know',
                            'Worsened')

ggplot(df_acesso_professores, aes(x=reorder(acesso_professores, -perc), y=perc))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(perc,"%")),nudge_y=3)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()

# Gráfico 16 Modo de lecionar as aulas durante a pandemia
df_aulas_durante_pandemia <- dbf.csv  %>% 
  count(aulas_durante_pandemia) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_aulas_durante_pandemia[1] <- c('Não Respondeu',
                              'Unchanged',
                              'Increased',
                              'Don\'t know',
                              'Worsened')

df_aulas_durante_pandemia

ggplot(df_aulas_durante_pandemia, aes(x=reorder(aulas_durante_pandemia, -perc), y=perc))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(perc,"%")),nudge_y=2)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()


# Gráfico 17: IES reiniciou atividades presenciais
df_ies_reinicio <- dbf.csv  %>% 
  count(ies_reinicio)   %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_ies_reinicio[1] <- c('Non-response',
                        'Partially',
                        'Don\'t know',
                        'Not Returned',
                        'Returned')

df_ies_reinicio

ggplot(df_ies_reinicio, aes(x=reorder(ies_reinicio, -perc), y=perc))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(perc,"%")),nudge_y=2)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()


# Gráfico 18: Acesso aos serviços de internet
df_acesso_internet <- dbf.csv  %>% 
  count(acesso_internet) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_acesso_internet[1] <- c('Não Respondeu',
                        'Melhorou',
                        'Melhorou Muito',
                        'Piorou',
                        'Não Sabe',
                        'Não Mudou',
                        'Piorou muito')

# Melhorou : Melhorou e melhorou muito (improved)
# Piorou : Piorou e piorou muito (worsened)
# Nao sabe : (dont know)
# Nao Mudou : (Unchanged)

df_acesso_internet

ggplot(df_acesso_internet, aes(x=reorder(acesso_internet, -perc), y=perc))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(perc,"%")),nudge_y=6)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()


# Gráfico 19: Desempenho escolar na pandemia
df_desempenho_escolar <- dbf.csv  %>% 
  count(desempenho_escolar) %>% 
  mutate(perc=round(n/sum(n)*100,0))

df_desempenho_escolar[1] <- c('Non-Response',
                           'increased',
                           'Declined',
                           'Unchanged',
                           'Don\'t Know')

df_desempenho_escolar

ggplot(df_desempenho_escolar, aes(x=reorder(desempenho_escolar, -perc), y=perc))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(perc,"%")),nudge_y=2)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()

#Gráfico 20: Dificuldades vividas pelo aluno
df_vivenciou <- dbf.csv  %>% 
  count(vivenciou) %>% 
  mutate(perc=round(n/sum(n)*100,0))

df_vivenciou[1] <- c('Não Respondeu',
                              'Ajuda ou Assistência',
                              'Condições de Vida',
                              'Deslocamento',
                              'Discriminação',
                              'Não se lembra')

df_vivenciou

ggplot(df_vivenciou, aes(x=reorder(vivenciou,-perc), y=perc))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, '(', perc, '%)')),nudge_y=4)+
  labs(x='', y='%')+
  theme_minimal()+
  coord_flip()


# Gráfico 21: Despesas durante a pandemia
df_despesas <- dbf.csv  %>% 
  count(despesas) %>% 
  mutate(perc=round(n/sum(n)*100,0))

df_despesas[1] <- c('Non-Response',
                     'Increased',
                     'Decreased',
                     'Unchanged',
                     'Don\'t Know')

df_despesas

ggplot(df_despesas, aes(x=reorder(despesas,-perc), y=perc))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, '(', perc, '%)')),nudge_y=5)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()+
  coord_flip()

#Gráfico 22: REnda durante a pandemia
df_renda_financeira <- dbf.csv  %>% 
  count(renda_financeira) %>% 
  mutate(perc=round(n/sum(n)*100,0))

df_renda_financeira[1] <- c('Non-Response',
                            'Increased',
                            'Decreased',
                            'Unchanged',
                            'Don\'t Know')

df_renda_financeira

ggplot(df_renda_financeira, aes(x=reorder(renda_financeira, -perc), y=perc))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, '(', perc, '%)')),nudge_y=4)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()+
  coord_flip()

#Gráfico 23: Recebeu auxílio financeiro da IES
df_ajuda_financeira <- dbf.csv  %>% 
  count(ajuda_financeira) %>% 
  mutate(perc=round(n/sum(n)*100,0))

df_ajuda_financeira[1] <- c('Non-response',
                            'No',
                            'Yes')

df_ajuda_financeira

ggplot(df_ajuda_financeira, aes(x=reorder(ajuda_financeira, -perc), y=perc))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, '(', perc, '%)')),nudge_y=7)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()

#Gráfico 24: Endividamento durante pandemia
df_nivel_endividamento <- dbf.csv  %>% 
  count(nivel_endividamento) %>% 
  mutate(perc=round(n/sum(n)*100,0))

df_nivel_endividamento[1] <- c('Non-response',
                              'Increased',
                              'Decreased',
                              'Unchanged',
                              'Don\'t Know')

df_nivel_endividamento

ggplot(df_nivel_endividamento, aes(x=reorder(nivel_endividamento, -perc), y=perc))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, '(', perc, '%)')),nudge_y=3)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()


# Gráfico 25: Despesas que cresceram na pandemia 
# Descobrindo as ocorrências
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
names(casos_perfil_despesas) <- c("Relacionadas a Saúde", "Deslocamento", "Transporte urbano", 
                                  "Aluguel", "Internet", "Alimentação", "Outras")
casos_perfil_despesas
casos_perfil_despesas <- sort(casos_perfil_despesas)
pct_perfil_despesas <- paste0(round(unname(casos_perfil_despesas) / sum(unname(casos_perfil_despesas)) * 100,0), "%")
pct_perfil_despesas
#[1] "10%" "11%" "12%" "12%" "13%" "16%" "25%"
n = length(casos_perfil_despesas)
n
#[1] 7

df_despesas_cresceram <- data.frame(despesas= c("Healthcare", 
                                                "Urban Transportation", 
                                                "Travel", 
                                                "Rent", 
                                                "Internet", 
                                                "Food", 
                                                "Other"),
                                              n=c(relacionadas_saude, 
                                                  viagens_deslocamentos, 
                                                  transporte_urbano, 
                                                  aluguel, 
                                                  internet, 
                                                  alimentacao, 
                                                  outras))

# Percentual
df_despesas_cresceram <- df_despesas_cresceram %>% mutate(perc=round(n/sum(n)*100,1))
df_despesas_cresceram

ggplot(df_despesas_cresceram, aes(x=reorder(despesas, -perc), y=perc))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, '(', perc, '%)')),nudge_y=3.5)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()+
  coord_flip()


# Gráfico 26: situacao_durante-pandemia
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
mtext("Gráfico 26: Manifestação sobre a sua situação durante a pandemia", side=3, line = 3)
print("Principais ocorrências de palavras em situação durante a pandemia")
for (k in 1:length (which(pmax(df$freq) > 2))) {
  print(paste(df$word[k], df$freq[k],sep = " = " ))
}

#Gráfico 27: Qualidade de vida
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
mtext("Gráfico 27: Manifestação sobre a sua qualidade de vida", side=3, line = 3)
print("Principais ocorrências de palavras em qualidade de vida")
for (k in 1:length (which(pmax(df$freq) > 2))) {
  print(paste(df$word[k], df$freq[k],sep = " = " ))
}

#Gráfico 28: IES fez de positivo na pandemia
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
mtext("Gráfico 28: O que a IES fez de positivo na pandemia", side=3, line = 3)
print("Principais ocorrências de palavras do que a IES fez de positivo na pandemia")
for (k in 1:length (which(pmax(df$freq) > 2))) {
  print(paste(df$word[k], df$freq[k],sep = " = " ))
}

#Gráfico 29: IES poderia melhorar na pandemia
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
mtext("Gráfico 29: O que a IES poderia melhorar na pandemia", side=3, line = 3)
print("Principais ocorrências de palavras do que a IES poderia melhorar na pandemia")
for (k in 1:length (which(pmax(df$freq) > 2))) {
  print(paste(df$word[k], df$freq[k],sep = " = " ))
}

#Gráfico 30: IES poderia melhor ajudar na pandemia
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
mtext("Gráfico 30: Em que a IES poderia melhor ajudar na pandemia", side=3, line = 3)
print("Principais ocorrências de palavras do que a IES poderia melhora ajudar na pandemia")
for (k in 1:length (which(pmax(df$freq) > 2))) {
  print(paste(df$word[k], df$freq[k],sep = " = " ))
}


#Gráfico 31: Capacidade para prosseguir ou concluir estudos
df_capacidade_prosseguir_estudos <- dbf.csv  %>% 
  count(capacidade_prosseguir_estudos) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_capacidade_prosseguir_estudos[1] <- c('Não Respondeu',
                               'Melhorou',
                               'Melhorou Muito',
                               'Piorou Muito',
                               'Não sabe',
                               'Não Mudou',
                               'Piorou')

df_capacidade_prosseguir_estudos

ggplot(df_capacidade_prosseguir_estudos, aes(x=capacidade_prosseguir_estudos, y=perc))+
  geom_col(width=.6, fill=alpha('darkblue',3), col='black')+
  geom_text(aes(label=paste(perc,"%")),nudge_y=2)+
  labs(x='', y='%')+
  ggtitle('Gráfico 31: Capacidade para prosseguir ou concluir estudos')+
  theme_minimal()+
  coord_flip()

# Gráfico 32: Capacidade de socialização
df_capacidade_socializacao <- dbf.csv  %>% 
  count(capacidade_socializacao) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_capacidade_socializacao[1] <- c('Não Respondeu',
                                   'Melhorou',
                                   'Piorou Muito',
                                   'Não Mudou',
                                   'Piorou')

df_capacidade_socializacao

ggplot(df_capacidade_socializacao, aes(x=capacidade_socializacao, y=perc))+
  geom_col(width=.6, fill=alpha('orange',3), col='black')+
  geom_text(aes(label=paste(perc,"%")),nudge_y=2)+
  labs(x='', y='%')+
  ggtitle('Gráfico 32: Capacidade de socialização')+
  theme_minimal()+
  coord_flip()

#Gráfico 33 Bem-estar psicológico
df_bem_estar_psicologico <- dbf.csv  %>% 
  count(bem_estar_psicologico) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_bem_estar_psicologico[1] <- c('Não Respondeu',
                                 'Melhorou',
                                 'Melhorou Muito',
                                 'Piorou Muito',
                                 'Não Sabe',
                                 'Não Mudou',
                                 'Piorou')

df_bem_estar_psicologico

ggplot(df_bem_estar_psicologico, aes(x=bem_estar_psicologico, y=perc))+
  geom_col(width=.6, fill=alpha('purple',3), col='black')+
  geom_text(aes(label=paste(perc,"%")),nudge_y=2)+
  labs(x='', y='%')+
  ggtitle('Gráfico 33: Bem-estar psicológico')+
  theme_minimal()+
  coord_flip()

# Gráfico 34: Espaço físico utilizado para atividades escolares
df_espaco_fisico <- dbf.csv  %>% 
  count(espaco_físico) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_espaco_fisico[1] <- c('Não Respondeu',
                         'Não Mudou',
                         'Melhorou',
                         'Não Sabe',
                         'Piorou')

df_espaco_fisico

ggplot(df_espaco_fisico, aes(x=espaco_físico, y=perc))+
  geom_col(width=.6, fill=alpha('tomato',3), col='black')+
  geom_text(aes(label=paste(perc,"%")),nudge_y=2)+
  labs(x='', y='%')+
  ggtitle('Gráfico 34: Espaço físico utilizado para atividades escolares')+
  theme_minimal()+
  coord_flip()

# Dados do Gráfico 35
df_disposicao_atividades <- dbf.csv  %>% 
  count(disposicao_atividades) %>% 
  mutate(perc=round(n/sum(n)*100,1))

df_disposicao_atividades[1] <- c('Não Respondeu',
                         'Aumentou',
                         'Diminuiu',
                         'Não Mudou',
                         'Não Sabe')

df_disposicao_atividades

ggplot(df_disposicao_atividades, aes(x=disposicao_atividades, y=perc))+
  geom_col(width=.6, fill=alpha('yellow',3), col='black')+
  geom_text(aes(label=paste(perc,"%")),nudge_y=3)+
  labs(x='', y='%')+
  ggtitle('Gráfico 35: Disposição para participar das atividades escolares')+
  theme_minimal()+
  coord_flip()


# Gráfico 36: Nível de ansiedade para o futuro
df_nivel_ansiedade <- dbf.csv  %>% 
  count(nivel_ansiedade)%>%
  mutate(perc=round(n/sum(n)*100,1))

df_nivel_ansiedade[1,1] <- 'Não Respondeu'

df_nivel_ansiedade

ggplot(df_nivel_ansiedade, aes(x=nivel_ansiedade, y=perc))+
  geom_col(width=.6, fill=alpha('white',3), col='black')+
  geom_text(aes(label=paste(perc,"%")),nudge_y=3)+
  labs(x='', y='%')+
  ggtitle('Gráfico 36: Nível de ansiedade para o futuro')+
  theme_minimal()+
  coord_flip()


# Gráfico 37: Nível de ansiedade para planejamento pessoal
df_ansiedade_planejamento <- dbf.csv  %>% 
  count(ansiedade_planejamento) %>%
  mutate(perc=round(n/sum(n)*100,1))

df_ansiedade_planejamento[1,1] <- 'Não Respondeu'

df_ansiedade_planejamento

ggplot(df_ansiedade_planejamento, aes(x=ansiedade_planejamento, y=perc))+
  geom_col(width=.6, fill=alpha('orange',3), col='black')+
  geom_text(aes(label=paste(perc,"%")),nudge_y=3)+
  labs(x='', y='%')+
  ggtitle('Gráfico 37: Nível de ansiedade para planejamento pessoal')+
  theme_minimal()+
  coord_flip()

# Gráfico 38: Nível de ansiedade para planejamento a longo prazo
df_ansiedade_longo_prazo <- dbf.csv  %>% 
  count(ansiedade_longo_prazo) %>%
  mutate(perc=round(n/sum(n)*100,1))

df_ansiedade_longo_prazo[1,1] <- 'Não Respondeu'
                             
df_ansiedade_longo_prazo

ggplot(df_ansiedade_longo_prazo, aes(x=ansiedade_longo_prazo, y=perc))+
  geom_col(width=.6, fill=alpha('gray',3), col='black')+
  geom_text(aes(label=paste(perc,"%")),nudge_y=3)+
  labs(x='', y='%')+
  ggtitle('Gráfico 38: Nível de ansiedade para planejamento a longo prazo')+
  theme_minimal()+
  coord_flip()


#Gráfico 39: Dificuldades financeiras durante a pandemia
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
mtext("Gráfico 39: Dificuldades financeiras durante a pandemia", side=3, line = 3)
print("Principais ocorrências de palavras das dificuldades financeiras durante a pandemia")
for (k in 1:length (which(pmax(df$freq) > 2))) {
  print(paste(df$word[k], df$freq[k],sep = " = " ))
}


#Gráfico 40: Dificuldades acadêmicas durante a pandelia
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
mtext("Gráfico 40: Dificuldades acadêmicas durante a pandemia", side=3, line = 3)
print("Principais ocorrências de palavras das dificuldades acadêmicas durante a pandemia")
for (k in 1:length (which(pmax(df$freq) > 2))) {
  print(paste(df$word[k], df$freq[k],sep = " = " ))
}

#Gráfico 41: Comentários finais
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
mtext("Gráfico 41: Comentários finais", side=3, line = 3)
print("Principais ocorrências de palavras nos comentários finais")
for (k in 1:length (which(pmax(df$freq) > 2))) {
  print(paste(df$word[k], df$freq[k],sep = " = " ))
}

# Gráfico 42: Acesso aos serviços de saude
df_acesso_servicos_saude <- dbf.csv  %>% 
  count(acesso_servicos_saude) %>%
  mutate(perc=round(n/sum(n)*100,1))

df_acesso_servicos_saude[1] <- c('Não Respondeu',
                                 'Melhorou',
                                 'Piorou Muito',
                                 'Não Sabe',
                                 'Não Mudou',
                                 'Piorou')

df_acesso_servicos_saude

ggplot(df_acesso_servicos_saude, aes(x=acesso_servicos_saude, y=perc))+
  geom_col(width=.6, fill=alpha('green',3), col='black')+
  geom_text(aes(label=paste(perc,"%")),nudge_y=3)+
  labs(x='', y='%')+
  ggtitle('Gráfico 42: Acesso aos serviços de saude')+
  theme_minimal()+
  coord_flip()
