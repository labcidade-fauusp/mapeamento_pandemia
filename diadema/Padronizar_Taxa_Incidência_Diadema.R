###Carregando as bibliotecas
pacman::p_load(
  dplyr,
  reshape2,
  tidyverse,           
  janitor,              
  epitools)

##########################Analise temporal de notificacao de covid
not <- read.csv("./dados/diadema_covid_2020_2022.csv",
                    sep = ";",header=T)
not$CD_SETOR_2010 <-as.character(not$CD_SETOR_2010)
not$dt_notif <- strptime(not$DATA.NOTIFICAÇÃO,format="%d/%m/%Y")#Transformando a data
not$MES <- format(cov_obi$dt_notif, "%Y-%m")

not <- not %>% group_by(FX.ETÁRIA, GÊNERO, CD_SETOR_2010,MES) %>% tally()  
colnames (not) <- c ("faixa","sexo", "codigo","mes","notifica") 
ubs <- read.table("/Volumes/DiscoD/fapesp/dados/corresp_sc_ubs.csv",sep = ',',
                  header=T)
ubs$CD_GEOCODI <-as.character(ubs$CD_GEOCODI)
colnames (ubs) <- c ("codigo", "nome","UBS") 
not_ubs <- left_join(not, ubs, by=c('codigo'))
not_ubs <- not_ubs %>% group_by(nome,faixa,sexo,mes) %>%
  summarise(notifica = sum(notifica))
write.table(not_ubs, "cov_not_ubs.csv", sep=";",
            row.names = FALSE, dec = ".")
view(not_ubs)

####Juntando dados de óbito, pop por AA e pop padrao
pop_dia <- read.csv("/Volumes/DiscoD/fapesp/dados/pop_dia_FE_ubs.csv",
                    sep = ";",header=T)
dia_mes <- read.csv("/Volumes/DiscoD/fapesp/dados/filtrotemp.csv",
                    sep = ";",header=T)
dia_mes <- dia_mes %>% mutate(nome = recode(nome,
                                            "CASAGRANDE" = "CASA GRANDE",
                                            "MARIATEREZA" = "MARIA TEREZA",
                                            "NOVACONQUISTA" = "NOVA CONQUISTA",
                                            "SAOJOSE" = "SAO JOSE"))
diadema <- diadema %>% mutate(sexo = recode(sexo, "M" = "MASCULINO","F" = "FEMININO"))
padrao <- pop_dia %>% group_by(sexo,faixa)  %>% summarise(pop = sum(populacao))
not_obi_ubs <- left_join(not_obi_ubs, padrao, by=c("faixa", "sexo"))
dia_mespop<- left_join(dia_mes,pop_dia,
                       by=c("nome"))
cov_noti_pd <- left_join(dia_mespop,cov_obi_ubs,
                        by=c("nome","faixa", "sexo","mes"))
padrao <- pop_dia %>% group_by(sexo,faixa)  %>% summarise(pop = sum(populacao))
cov_noti_pd <- left_join(cov_noti_pd, padrao, by=c("faixa", "sexo"))
cov_noti_pd <- mutate_at(cov_noti_pd, c("notifica"),
                        ~replace(., is.na(.), 0))
view (dia_mespop)

####################Taxa padronizada por ubs temporal
not_obi_ubs_pd <- not_obi_ubs %>% group_by(nome,mes) %>%
  summarise(age_adjust = list(ageadjust.direct(count = notifica,  
                                               pop = populacao,          
                                               rate = NULL,                
                                               stdpop = pop,            
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))

not_not_ubs_pd <- mutate (not_not_ubs_pd, tx_bruta = crude.rate * 10000,
                           tx_padro = adj.rate * 10000)
view(not_not_ubs_pd)

####Grafico de linha
not_ubs <- ggplot(data = subset(not_obi_ubs_pd, !is.na(mes)), 
                   aes(x = mes, group=nome, y = tx_padro,color = nome)) +  
  geom_line(na.rm = TRUE) +
  labs(x = '', y = 'óbito', 
       title = 'Incidencia mensal por 10.000 habitantes') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

not_ubs









