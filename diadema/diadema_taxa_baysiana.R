##########Taxas baysianas padronizadas por sexo e idade
#####Carregando pacotes
pacman::p_load(dplyr, reshape2, tidyverse, janitor, epitools, spdep,sf)

########Padronizando as taxas
diadema <- read.csv("./Diadema/diadema_covid_2020_2022.csv",sep = ";",header=T)
dia.obi <- filter(diadema, RECUPERADOS == 'OBITO')

#Filtro - Transformando a data
dia.obi$dt_notif <- strptime(dia.obi$DATA.NOTIFICAÇÃO,format="%d/%m/%Y")
filtro <-data.frame(Date = seq(as.Date("2021-03-01"), as.Date("2022-02-28"), by="day"))
dia.obi <- dia.obi %>% filter(dt_notif %in% filtro$Date)
dia.obi <- dia.obi %>% group_by(FX.ETÁRIA, GÊNERO, CD_SETOR_2010) %>% tally()
colnames (dia.obi) <- c ("faixa","sexo", "codigo","obito") 

####Juntando dados de óbito, pop por setor censitario e pop padrao
### 508 setores censitario com populacao, somente 504 com pop por faixa etaria
## 14 * 508 = 7112
pop_dia <- read.csv("./Diadema/pop_diadema_FE_SC.csv", sep = ";",header=T)
pop.padrao <- read.csv("./Diadema/pop_dia_FE_ubs.csv", sep = ";",header=T)
pop.padrao <- pop.padrao %>% group_by(sexo,faixa) %>%
  summarise(pop = sum(populacao))
pop.sc.pd <- left_join(pop.padrao, pop_dia, by=c("faixa", "sexo"))
pop.sc.pd <- mutate_at(pop.sc.pd, c("populacao"), ~replace(., is.na(.), 0))
pop.sc.pd.obi <- left_join(pop.sc.pd, dia.obi, by=c("faixa", "sexo","codigo"))
pop.sc.pd.obi <- mutate_at(pop.sc.pd.obi, c("obito"), ~replace(., is.na(.), 0))

####################Taxa padronizada por ubs espacial
dia.tx.padro <- pop.sc.pd.obi %>% group_by(codigo) %>%
  summarise(age_adjust = list(ageadjust.direct(count = obito,  
                                               pop = populacao,          
                                               rate = NULL,                
                                               stdpop = pop,            
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))
dia.tx.padro <- mutate (dia.tx.padro, txbr_obi = crude.rate * 10000,
                            txpd_obi = adj.rate * 10000)
### Ha setores censitarios sem populacao
#351380105000499, 351380105000350, 351380105000145
###Ha setores com obitos mais sem populacao
#351380105000356, 351380105000203 (1)
###Ha setores que possuem taxa bruta, mas nao padronizada
#possui populacao e obitos 
#351380105000494 (1 obito), 351380105000272 (0),351380105000269 (1)
#351380105000359 (0)

### 4 - Taxa bayesiana empirica 
dia.map <- st_read("./Shape/sc_diadema_2010.shp") # lendo shape
plot(st_geometry(dia.map))

# taxa bruta e padronizada
###Populacao por setor censitario
dia.pop.st <- pop_dia %>% group_by(codigo) %>% summarise(pop = sum(populacao))

colnames (dia.pop.st) <- c ("codigo","pop") 
dia.pop.st <- mutate_at(dia.pop.st, c("pop"), ~replace(., is.na(.), 0))
colSums(dia.pop.st[2]) #######Total da pop diadema 370955
###Total oficial 386.039

##Unindo populacao por setor censitario com as taxas bruta e padronizada
dia.obi.pd <- left_join(dia.pop.st, dia.tx.padro, by=c("codigo"))
####Calculando total de obitos padronizados
dia.obi.pd$ob.pd <- dia.obi.pd$adj.rate * dia.obi.pd$pop

###Agrupando obito por setor censitario
dia.ob.br <- dia.obi %>% group_by(codigo) %>% summarise(ob.br = sum(obito))
dia.ob.pd.br <- left_join(dia.obi.pd, dia.ob.br, by=c("codigo"))
dia.ob.pd.br <- dia.ob.pd.br[,-c(3:6)]
colnames(dia.ob.pd.br) <- c("CD_GEOCODI","pop","txbr_ob","txpd_ob","ob_pd","ob_br") 
dia.ob.pd.br$CD_GEOCODI<-as.character(dia.ob.pd.br$CD_GEOCODI)
dia_map <- left_join( dia.map, dia.ob.pd.br, by=c("CD_GEOCODI"))

# taxa bayesiana global # EBest(n, x, family="poisson")
# n: casos
# x: populacao
#######substituir NA por zero
dia_map <- mutate_at(dia_map, c("ob_pd"), ~replace(., is.na(.), 0)) 
dia_map <- mutate_at(dia_map, c("ob_br"), ~replace(., is.na(.), 0)) 
dia_map <- dia_map %>% filter(dia_map$pop > 0)

#######Calculando taxa baysiana global - obito
dia.tx.gl <- EBest(dia_map$ob_pd, dia_map$pop, family="poisson")
class(dia.tx.gl)

# juntando a taxa bayesiana global com o shape
dia_map <- cbind(dia_map,dia.tx.gl$estmm)

# taxa bayesiana por 100000 hab
dia_map$txbayg <- (dia_map$dia.tx.gl.estmm)*10000

# todas as taxas foram em direcao a media
plot(dia_map["txpd_ob"])
plot(dia_map["txbr_ob"])
plot(dia_map["txbayg"])

### 4.2 Taxa bayesiana local ##############obito
# zero.policy: default NULL, use global option value; if TRUE assign zero to 
#the lagged value of zones without neighbours, if FALSE assign NA
# geoda=T: para obter mesmo resultado q GeoDa
# nb: matriz de vizinhanca - contiguidade # queen
dia.q <- poly2nb(as(dia_map, "Spatial"),queen=TRUE) 
print(dia.q)
str(dia.q)

# taxa bayesiana local com o shape
# uso matriz de viz e nao de pesos
diabay.lc.p <- EBlocal(dia_map$ob_pd, dia_map$pop, dia.q) 

str(diabay.lc.p)
# Unindo ao mapa
dia_map <- cbind(dia_map, diabay.lc.p$est)
str(dia_map)

# taxa bayesiana local por 100000 hab
dia_map$txbayl_pd <- (dia_map$diabay.lc.p.est)*10000
str(dia_map)
plot(dia_map["txpd_ob"])
plot(dia_map["txbr_ob"])
plot(dia_map["txbayl_pd"])

dia_sc_tx_21 <- dia_map[,-c(1,3:14,18:20,22)]
colnames(dia_sc_tx_21) <- c("CD_GEOCODI","pop","br_ob21","pd_ob21","bayg_ob21","geometry","bayl_ob21") 

# exportando o shape

st_write(dia_sc_tx_21,'./Shape/dia_sc_tx21.shp')

####################################Moran com as taxas padronizadas##obito
dia.moran <- st_read("./Shape/dia_sc_tx.shp") # lendo shape
plot(st_geometry(dia.moran))

###Matriz de contiguidade e peso
dia.q <- poly2nb(as(dia.moran, "Spatial"),queen=TRUE) 
dia.moran.pes <- nb2listw(dia.q) 

###Teste do moran é o Moran Global?
dia.moran <- mutate_at(dia.moran, c("txpd_ob"), ~replace(., is.na(.), 0)) 
moran.test(dia.moran$txpd_ob,dia.moran.pes, zero.policy=NULL)

#############Moran local - utiliza matriz de peso
dia.ml <- localmoran(dia.moran$txpd_ob,listw = dia.moran.pes,alternative="two.sided")
class(dia.ml)
dia.ml
head(dia.ml)

# ordenando a matriz ##Para analisar p<0.05, mas prq?# 3 areas com p < 0.05?
dia.ml[order(dia.ml[,"Pr(z != E(Ii))"]),] 

# juntando o objeto 'sf' com a matriz
dia.ml.map <- cbind(dia.moran,dia.ml)

class(dia.ml.map)
str(dia.ml.map)

# criando a  coluna 'moran.local' e dizendo q seus valores sao nulos
dia.ml.map$moran.local <- 0

# classificando o moran local segundo as categorias 1 (alto-alto); 2 (baixo-baixo); 4 (baixo-alto) e 3 (alto-baixo)
str(dia.ml.map)

# media da variavel taxa bruta de obito por covid
mean(dia.ml.map$txpd_ob) # 44.02
summary(dia.ml.map)

### usar tb os valores da coluna Z.Ii q rerpresenta a media dos vizinhos
# atribuindo os valores corretos

head(dia.ml.map) ## para copiar nome da coluna 'Pr.z....E.Ii..'
dia.ml.map$moran.local <- ifelse(dia.ml.map$Pr.z....E.Ii..>=0.05 , 0, ifelse(dia.ml.map$txbr_ob>44.02 & dia.ml.map$Ii > 0, 1, ifelse (dia.ml.map$txbr_ob<44.02 & dia.ml.map$Ii>0,2,ifelse(dia.ml.map$txbr_ob<44.02 & dia.ml.map$Ii<0,4,3))))
# mostrando o resultado
str(dia.ml.map)
dia.ml.map$moran.local
###o que mostra esse print?
print(dia.ml.map[c(16,22,25,26,28)],n=82)

# mapeando
plot(dia.ml.map["moran.local"])

# exportando o shape
st_write(dia.ml.map,"./Shape/dia.ml.pd.ob.shp")

###############################################################################
###############################################################################
##########################################################Notificacao
##########Taxas baysianas padronizadas por sexo e idade
#####Carregando pacotes
pacman::p_load(dplyr, reshape2, tidyverse, janitor, epitools,
               spdep,sf)

########Padronizando as taxas
diadema <- read.csv("./Diadema/diadema_covid_2020_2022.csv",sep = ";",header=T)

#Transformando a data
diadema$dt_notif <- strptime(diadema$DATA.NOTIFICAÇÃO,format="%d/%m/%Y")
filtro <- data.frame(Date = seq(as.Date("2021-03-01"), as.Date("2022-02-28"), by="day"))
dia.int <- diadema %>% filter(dt_notif %in% filtro$Date)

dia.int <- dia.int %>% group_by(FX.ETÁRIA, GÊNERO, CD_SETOR_2010) %>% tally()  
colnames (dia.int) <- c ("faixa","sexo", "codigo","interna") 

####Juntando dados de óbito, pop por setor censitario e pop padrao
### 508 setores censitario com populacao, somente 504 com pop por faixa etaria
## 14 * 508 = 7112
pop_dia <- read.csv("./Diadema/pop_diadema_FE_SC.csv", sep = ";",header=T)
pop.padrao <- read.csv("./Diadema/pop_dia_FE_ubs.csv", sep = ";",header=T)
pop.padrao <- pop.padrao %>% group_by(sexo,faixa) %>%
  summarise(pop = sum(populacao))
pop.sc.pd <- left_join(pop.padrao, pop_dia, by=c("faixa", "sexo"))
pop.sc.pd <- mutate_at(pop.sc.pd, c("populacao"), ~replace(., is.na(.), 0))
pop.sc.pd.int <- left_join(pop.sc.pd, dia.int, by=c("faixa", "sexo","codigo"))
pop.sc.pd.int <- mutate_at(pop.sc.pd.int, c("interna"), ~replace(., is.na(.), 0))

####################Taxa padronizada por ubs espacial
dia.tx.padro <- pop.sc.pd.int %>% group_by(codigo) %>%
  summarise(age_adjust = list(ageadjust.direct(count = interna,  
                                               pop = populacao,          
                                               rate = NULL,                
                                               stdpop = pop,            
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))
dia.tx.padro <- mutate (dia.tx.padro, txbr_int = crude.rate * 10000,
                        txpd_int = adj.rate * 10000)
### Ha setores censitarios sem populacao
#351380105000499, 351380105000350, 351380105000145
###Ha setores com obitos mais sem populacao
#351380105000356, 351380105000203 (1)
###Ha setores que possuem taxa bruta, mas nao padronizada
#possui populacao e obitos 
#351380105000494 (1 obito), 351380105000272 (0),351380105000269 (1)
#351380105000359 (0)

### 4 - Taxa bayesiana empirica 
dia.map <- st_read("./Shape/sc_diadema_2010.shp") # lendo shape
plot(st_geometry(dia.map))

# taxa bruta e padronizada
###Populacao por setor censitario
dia.pop.st <- pop_dia %>% group_by(codigo) %>% summarise(pop = sum(populacao))
colnames (dia.pop.st) <- c ("codigo","pop") 
dia.pop.st <- mutate_at(dia.pop.st, c("pop"), ~replace(., is.na(.), 0))
colSums(dia.pop.st[2]) #######Total da pop diadema 370955
###Total oficial 386.039

##Unindo populacao por setor censitario com as taxas bruta e padronizada
dia.int.pd <- left_join(dia.pop.st, dia.tx.padro, by=c("codigo"))
####Calculando total de obitos padronizados
dia.int.pd$ob.pd <- dia.int.pd$adj.rate * dia.int.pd$pop

###Agrupando internacao por setor censitario
dia.int.br <- dia.int %>% group_by(codigo) %>% summarise(int.br = sum(interna))
dia.int.pd.br <- left_join(dia.int.pd, dia.int.br, by=c("codigo"))
dia.int.pd.br <- dia.int.pd.br[,-c(3:6)]
colnames(dia.int.pd.br) <- c("CD_GEOCODI","pop","txbr_int","txpd_int","int_pd","int_br") 
dia.int.pd.br$CD_GEOCODI<-as.character(dia.int.pd.br$CD_GEOCODI)
dia_map <- left_join( dia.map, dia.int.pd.br, by=c("CD_GEOCODI"))

# taxa bayesiana global # EBest(n, x, family="poisson")
# n: casos
# x: populacao
#######substituir NA por zero
dia_map <- mutate_at(dia_map, c("int_pd"), ~replace(., is.na(.), 0)) 
dia_map <- mutate_at(dia_map, c("int_br"), ~replace(., is.na(.), 0)) 
dia_map <- dia_map %>% filter(dia_map$pop > 0)
### Matriz queen
dia_map <- dia_map[-c(202,268),]###Retirando Inf
dia.q <- poly2nb(as(dia_map, "Spatial"),queen=TRUE) 
print(dia.q)
str(dia.q)

# taxa bayesiana local com o shape
diabay.lc.p <- EBlocal(dia_map$int_pd, dia_map$pop, dia.q) 
str(diabay.lc.p)
# Unindo ao mapa
dia_map <- cbind(dia_map, diabay.lc.p$est)
str(dia_map)

# taxa bayesiana local por 100000 hab
dia_map$txbInt_pd <- (dia_map$diabay.lc.p.est)*10000
plot(dia_map["txbInt_pd"])

dia_sc_tx_int_21 <- dia_map[,-c(1,3:14,18:20)]

# exportando o shape
colnames(dia_sc_tx_int_21) <- c("CD_GEOCODI","pop","br_int21","pd_int21","geometry","bayl_int21") 

st_write(dia_sc_tx_int_21,'./Shape/dia_sc_tx_int_21.shp')

################################Moran com as taxas padronizadas## Notificacao
dia.moran <- st_read("./Shape/dia_sc_tx_int.shp") # lendo shape

###Matriz de contiguidade e peso
dia.q <- poly2nb(as(dia.moran, "Spatial"),queen=TRUE) 
dia.moran.pes <- nb2listw(dia.q) 

###Teste do moran é o Moran Global?
dia.moran <- mutate_at(dia.moran, c("txpd_int"), ~replace(., is.na(.), 0)) 
moran.test(dia.moran$txpd_int,dia.moran.pes, zero.policy=NULL)

#############Moran local - utiliza matriz de peso
dia.ml <- localmoran(dia.moran$txpd_int, listw = dia.moran.pes,alternative="two.sided")

# ordenando a matriz ##Para analisar p<0.05, mas prq?# 3 areas com p < 0.05?
dia.ml[order(dia.ml[,"Pr(z != E(Ii))"]),] 

# juntando o objeto 'sf' com a matriz
dia.ml.map <- cbind(dia.moran,dia.ml)

# criando a  coluna 'moran.local' e dizendo q seus valores sao nulos
dia.ml.map$moran.local <- 0

# media da variavel taxa bruta de obito por covid
mean(dia.ml.map$txpd_int) # 1312.821
summary(dia.ml.map)

### usar tb os valores da coluna Z.Ii q rerpresenta a media dos vizinhos
# atribuindo os valores corretos

head(dia.ml.map) ## para copiar nome da coluna 'Pr.z....E.Ii..'
dia.ml.map$moran.local <- ifelse(dia.ml.map$Pr.z....E.Ii..>=0.05 , 0,ifelse(dia.ml.map$txbr_int>1312.821 & dia.ml.map$Ii > 0, 1,ifelse (dia.ml.map$txbr_int<1312.821 & dia.ml.map$Ii>0,2,ifelse(dia.ml.map$txbr_int<1312.821 & dia.ml.map$Ii<0,4,3))))
# mostrando o resultado
str(dia.ml.map)
dia.ml.map$moran.local
###o que mostra esse print?
print(dia.ml.map[c(16,22,25,26,28)],n=82)

# mapeando
plot(dia.ml.map["moran.local"])

# exportando o shape
st_write(dia.ml.map,"./Shape/dia.ml.pd.int.shp")








