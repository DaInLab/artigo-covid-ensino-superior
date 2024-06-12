# Gráficos editados para o artigo do I SINCID

# Gráfico 1 - tipo barra respondentes : IES Decidiu Fechar
df_decisao_fechar <- dbf.csv  %>% 
  count(decisao_fechar) %>% 
  mutate(perc=round(n/sum(n)*100,0))

df_decisao_fechar[1] <- c('Não respondeu',
                          'De forma prudente',
                          'Lentamente',
                          'Rapidamente')

df_decisao_fechar

g1 <- ggplot(df_decisao_fechar, aes(x=reorder(decisao_fechar, -n), y=n))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, ' (', perc, '%)')),nudge_y=6)+
  labs(x='', y='Número de Respondentes')+
  theme_minimal()
g1
# Salvando o gráfico
#png(filename = "./graficos-I-SINCID/figura1.png", width = 480, height = 480, units = "px", pointsize = 12)
#    quality = 75,)
#g1
#dev.off()

# Gráfico 2 Acesso à infra-estrutura da IES
df_acesso_infra_ies <- dbf.csv  %>% 
  count(acesso_infra_ies) %>% 
  mutate(perc=round(n/sum(n)*100,0))

df_acesso_infra_ies[1] <- c('Não Respondeu',
                            'Não Mudou',
                            'Melhorou',
                            'Não Sabe',
                            'Piorou')

df_acesso_infra_ies

g2 <- ggplot(df_acesso_infra_ies, aes(x=reorder(acesso_infra_ies, -n), y=n))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, ' (', perc, '%)')),nudge_y=2)+
  labs(x='', y='Número de Respondentes')+
  theme_minimal()
g2
# Salvando o gráfico
#png(filename = "./graficos-I-SINCID/figura2.png")
#g2
#dev.off()

# Gráfico 3 - Acesso aos Professores na pandemia
df_acesso_professores <- dbf.csv  %>% 
  count(acesso_professores) %>% 
  mutate(perc=round(n/sum(n)*100,0))

df_acesso_professores[1] <- c('Não Respondeu',
                              'Não Mudou',
                              'Melhorou',
                              'Não Sabe',
                              'Piorou')

g3 <- ggplot(df_acesso_professores, aes(x=reorder(acesso_professores, -n), y=n))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, ' (', perc, '%)')),nudge_y=3)+
  labs(x='', y='Número de Respondentes')+
  theme_minimal()
g3
# Salvando o gráfico
#png(filename = "./graficos-I-SINCID/figura3.png")
#g3
#dev.off()

# Gráfico 4 - Modo de lecionar as aulas durante a pandemia
df_aulas_durante_pandemia <- dbf.csv  %>% 
  count(aulas_durante_pandemia) %>% 
  mutate(perc=round(n/sum(n)*100,0))

df_aulas_durante_pandemia[1] <- c('Não Respondeu',
                                  'Não Mudou',
                                  'Melhorou',
                                  'Não Sabe',
                                  'Piorou')

df_aulas_durante_pandemia

g4 <- ggplot(df_aulas_durante_pandemia, aes(x=reorder(aulas_durante_pandemia, -n), y=n))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, ' (', perc, '%)')),nudge_y=2)+
  labs(x='', y='Número de Respondentes')+
  theme_minimal()
g4
# Salvando o gráfico
#png(filename = "./graficos-I-SINCID/figura4.png")
#g4
#dev.off()

# Gráfico 5 - Acesso aos serviços de internet
df_acesso_internet <- dbf.csv  

df_acesso_internet$acesso_internet[df_acesso_internet$acesso_internet %in% c("Melhor do que antes", "Muito melhor do que antes")] <- "Melhorou"
df_acesso_internet$acesso_internet[df_acesso_internet$acesso_internet %in% c("Muito pior que antes", "Pior que antes")] <- "Piorou"
df_acesso_internet$acesso_internet[df_acesso_internet$acesso_internet %in% c("O mesmo que antes")] <- "Não mudou"
df_acesso_internet$acesso_internet[df_acesso_internet$acesso_internet %in% c("N/A ou Não sabe")] <- "Não Sabe"

df_acesso_internet <- df_acesso_internet%>% 
  count(acesso_internet) %>% 
  mutate(perc=round(n/sum(n)*100,0))

df_acesso_internet

df_acesso_internet$acesso_internet[1] <- 'Não Respondeu'


# Melhorou : Melhorou e melhorou muito (improved)
# Piorou : Piorou e piorou muito (worsened)
# Nao sabe : (dont know)
# Nao Mudou : (Unchanged)


g5 <- ggplot(df_acesso_internet, aes(x=reorder(acesso_internet, -perc), y=n))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, '(', perc, '%)')),nudge_y=4)+
  labs(x='', y='Número de Respondentes')+
  theme_minimal()
g5
# Salvando o gráfico
#png(filename = "./graficos-I-SINCID/figura5.png")
#g5
#dev.off()

# Gráfico 6 - Desempenho escolar na pandemia
df_desempenho_escolar <- dbf.csv  %>% 
  count(desempenho_escolar) %>% 
  mutate(perc=round(n/sum(n)*100,0))

df_desempenho_escolar[1] <- c('Não Respondeu',
                              'Aumentou',
                              'Diminuiu',
                              'Não Mudou',
                              'Não Sabe')

df_desempenho_escolar

g6 <- ggplot(df_desempenho_escolar, aes(x=reorder(desempenho_escolar, -n), y=n))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, '(', perc, '%)')),nudge_y=2)+
  labs(x='', y='Number of Respondents')+
  theme_minimal()
g6
# Salvando o gráfico
#png(filename = "./graficos-I-SINCID/figura6.png")
#g6
#dev.off()

# Gráfico 7 - Despesas durante a pandemia
df_despesas <- dbf.csv  %>% 
  count(despesas) %>% 
  mutate(perc=round(n/sum(n)*100,0))

df_despesas[1] <- c('Nào respondeu',
                    'Aumentou',
                    'Diminuiu',
                    'Não mudou',
                    'Não sabe')

df_despesas

g7 <- ggplot(df_despesas, aes(x=reorder(despesas,-perc), y=n))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, '(', perc, '%)')),nudge_y=6)+
  labs(x='', y='Número de Respondentes')+
  scale_y_continuous(limits=c(0, 45))+
  theme_minimal()+
  coord_flip()
g7
# Salvando o gráfico
#png(filename = "./graficos-I-SINCID/figura7.png")
#g7
#dev.off()

# Gráfico 8 - Recebeu auxílio financeiro da IES
df_ajuda_financeira <- dbf.csv  %>% 
  count(ajuda_financeira) %>% 
  mutate(perc=round(n/sum(n)*100,0))

df_ajuda_financeira[1] <- c('Não respondeu',
                            'Não',
                            'Sim')

df_ajuda_financeira

g8 <- ggplot(df_ajuda_financeira, aes(x=reorder(ajuda_financeira, -n), y=n))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, '(', perc, '%)')),nudge_y=7)+
  labs(x='', y='Número de Respondentes')+
  theme_minimal()
g8
# Salvando o gráfico
#png(filename = "./graficos-I-SINCID/figura8.png")
#g8
#dev.off()

# Gráfico 9 - # Gráfico 25: Despesas que cresceram na pandemia 
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

df_despesas_cresceram <- data.frame(despesas= c("Saúde", 
                                                "Transporte urbano", 
                                                "Viagens", 
                                                "Aluguel", 
                                                "Internet", 
                                                "Alimentação", 
                                                "Outras"),
                                    n=c(relacionadas_saude,
                                        transporte_urbano,
                                        viagens_deslocamentos,
                                        aluguel,
                                        internet,
                                        alimentacao,
                                        outras))

# Percentual
df_despesas_cresceram <- df_despesas_cresceram %>% mutate(perc=round(n/sum(n)*100,0))
df_despesas_cresceram

g9 <- ggplot(df_despesas_cresceram, aes(x=reorder(despesas, -perc), y=n))+
  geom_col(width=.6, fill=alpha('lightblue',3), col='black')+
  geom_text(aes(label=paste(n, '(', perc, '%)')),nudge_y=10)+
  labs(x='', y='Número de Respondentes')+
  scale_y_continuous(limits=c(0, 80))+
  theme_minimal()+
  coord_flip()
g9
# Salvando o gráfico
png(filename = "./graficos-I-SINCID/figura9.png")
g9
dev.off()
