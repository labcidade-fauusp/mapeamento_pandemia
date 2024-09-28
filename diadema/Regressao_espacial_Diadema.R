###########Regressao Espacial
# carregar pacotes
library(lattice)
#library(rgdal)
library(spdep) ##Moran
library(carData)
library(car) # VIF
library(ggplot2)
library(caret)
library(gstat)
library(sf) ##Ver mapas
library(nlme)

## 1 - leitura do banco de dados 
diadema <- read.csv('./dados/Dia_Tx_Indicadores_SemOutlier.csv', sep=",",dec = '.')

########################################################Analise exploratoria
# funcao desenvolvida por Zuur et al para ajudar na an explor 
# ficar junto do script

source("HighstatLibV9.R") 

## 1.1 Outliers na variavel resposta.

boxplot(diadema$txpd_ob)
dotchart(diadema$txpd_ob)

# comparando os dois graficos
#valores apontados como outliers pelo boxplot nao sao tao extremas qto pareceriam qdo olhamos o dotplot

## 1.2 Outliers nas variaveis independentes

MyVar <- c("PPI_perc","RENDA_capi","PES_DOM","X2_Densidade")
Mydotplot(diadema[,MyVar]) # usando a funcao 'HighstatLibV9.R'
boxplot(diadema$X2_Densidade) 
boxplot(diadema$PPI_perc) 
diadema[order(diadema$PES_DOM,decreasing=T),]
###Outliers
# Qdo explicao mais provavel eh erro de medida - valores devem ser excluidos
# A sua presenca pode dominar a analise
# PES_DOM, RENDA_capi, PPI_perc - tem valores mais alto mesmo! Nao eh erro de digit.
##Desidade - eh preciso verificar a confiabilidade
# Opcao 1: fazer analise sem ele, mas considera-lo na apresentacao dos resultados (val pred, por ex.)
# Opcao 2: transformar a covariavel usando, por exemplo, raiz quadrada (o q foi feito no artigo)

# criando a covar RZ e lg

diadema$RZ_RENDA_capi <- sqrt(diadema$RENDA_capi)
diadema$lg_PES_DOM <- log(diadema$PES_DOM)
diadema$lg_densidade <- log(diadema$X2_Densidade)
diadema <- diadema[-c(202),]
MyVar <- c("PPI_perc","RZ_RENDA_capi","lg_PES_DOM","lg_densidade")
Mydotplot(diadema[,MyVar]) # usando a funcao 'HighstatLibV9.R'
boxplot(diadema$RZ_RENDA_capi) 
boxplot(diadema$lg_densidade) 
diadema[order(diadema$PES_DOM,decreasing=T),]

# 3. Normalidade

hist(diadema$txpd_ob)
qqnorm(diadema$txpd_ob)

# 4. Excesso de zeros

# Altas proporcoes de zero na var dependente
# producao de estimativas de parametros e de erros padroes enviesados
# considerar os Modelos Inflados de Zero (ZIM - Zero Inflated Models)

sum(diadema$txpd_ob == 0) / nrow(diadema) * 100 # 17% de valores iguais a zero

# 5. Colinearidade entre as variaveis independentes

# refere-se ao prob de existencia de correlacao entre as var. indep.
# Se o prob for ignorado - produz modelos ruins
# exemplo - Tabela 1 do art de Zuur et al - pag 9

str(diadema)
pairs(diadema[,c(13,7,15,16,17)])#3 - txbr_ob, 4 - txpd_ob, 7 - PPI_perc, 13 - txbay_l, 15 - RZ_RENDA_capi, 16 - lg_PES_DOM, 17 - lg_densidade
cor.dia <- cor(diadema[,c(13,7,15,16,17)]) # criterio de corte - coef de correl >= 0.5 a 0.8
cor.dia

# VIF - variance inflation factor (pag 9, primeira coluna do artigo)
# criterio VIF < 3
# Remoção da colinearidade - utilizando o VIF

corvif(diadema[,MyVar]) ##ok - todas covar com VIF < 3

# 6. Relacao em var dependente (Y) e var indep (X)

# Plotagem da var dependente versus cada uma da var indep
# A ausencia de padroes claros entre X e cada uma da var indep nao significa q nao haja relacao entre elas.
# Signfica apenas q nao um ha claro relacionamento bivariado entre X e Y
# Modelagem com multiplas var explan pode proprocionar ainda um bom ajuste
### pode ajudar na decisao na escolha entre covariaveis colineares - entre duas ou mais, escolher a que tem "melhor" relacao com  variael dependente

# plotando Y em relacao a cada um dos Xs - usando o script 'HighstatLib.V9.R'

################### 9. As obs da var depend sao independentes espacialmente
MyX  <- c("lg_densidade","PPI_perc","RZ_RENDA_capi","lg_PES_DOM")
Myxyplot(diadema, MyX, "txbay_l", MyYlab = "Tx Bayesiana")

dia.espacial <- st_read("./Shape/Dia_taxas_indicadores_semOutlier_covariaveis_transformadas.shp")

###matriz de vizinhanca - contiguidade # queen
dia.q <- poly2nb(as(dia.espacial, "Spatial"),queen=TRUE) 

###Transformando em pesos
dia.moran.pes <- nb2listw(dia.q) 
summary(dia.moran.pes)

# moran da variavel dependente
moran.test(dia.espacial$txbay_l, dia.moran.pes,zero.policy=TRUE)

################################# mod de regress linear multipla 

dia.mod <- lm(txbay_l ~ RZ_RENDA_c + lg_densida, dia.espacial) 
summary(dia.mod) #sumario do modelo


# Analise dos residuos do modelo de regressao da taxa bayesiana

# Normalidade dos res
plot(dia.mod, 2) # QQ-plot - avaliar normalidade dos residuos
ks.test(dia.mod$residuals, mean(dia.mod$residuals), sd(dia.mod$residuals),
        alternative=c("greater")) # teste para a normalidade

# Autocorrelacao dos residuos

# construcao da matriz de viz - contiguidade
dia.espacial.mv <- poly2nb(dia.espacial) # matriz de viz com 1s e 0s
summary(dia.espacial.mv)
str(dia.espacial.mv)
print(dia.espacial.mv)
# matriz de pesos
dia.espacial.mp <- nb2listw(dia.espacial.mv, style="W")
summary(dia.espacial.mp,zero.policy=T)
print(dia.espacial.mp,zero.policy=T)
str(dia.espacial.mp)

# Calc do I de Moran dos residuos do modelo

lm.morantest(dia.mod, dia.espacial.mp, zero.policy = T)
####################### residuos com dep espacial I = 0.33, p < 0.001



############################################################################################################################################################################################################################################


# pressuposto basico de muitas tec estat - obs da var Y devem ser indep uma das outras
# Avaliar se var dep tem dep espacial

dia.espacial <- st_read("./Shape/Dia_taxas_indicadores_semOutlier_covariaveis_transformadas.shp")
MyX  <- c("PPI_perc","RZ_RENDA_capi","lg_PES_DOM","lg_densidade")
Myxyplot(diadema, MyX, "txbr_ob", MyYlab = "Tx Bruta")
###Variavel renda parece estar relacionada com a taxa padronizada

###matriz de vizinhanca - contiguidade # queen
dia.q <- poly2nb(as(dia.espacial, "Spatial"),queen=TRUE) 

###Transformando em pesos
dia.moran.pes <- nb2listw(dia.q) 
summary(dia.moran.pes)

# moran da variavel dependente
moran.test(dia.espacial$txbr_ob, dia.moran.pes,zero.policy=TRUE)
# na verdade, o q ira nos interessar eh se os resduos das modelagens tem ou nao depend espacial.

write.csv(diadema,file="./dados/dia_covariaveis_transformadas.csv")

############################ mod de regress linear multipla da tx padronizada

dia.espacial <- st_read("./Shape/Dia_taxas_indicadores_semOutlier_covariaveis_transformadas.shp")

dia.mod <- lm(txpd_ob ~ PPI_perc+ RZ_RENDA_c+lg_PES_DOM+lg_densida, dia.espacial) 
summary(dia.mod) #sumario do modelo

# Analise dos residuos do modelo de regressao da taxa padronizada

# Normalidade dos res
plot(dia.mod, 2) # QQ-plot - avaliar normalidade dos residuos
ks.test(dia.mod$residuals, mean(dia.mod$residuals), sd(dia.mod$residuals),
        alternative=c("greater")) # teste para a normalidade

# Autocorrelacao dos residuos

# construcao da matriz de viz - contiguidade
dia.espacial.mv <- poly2nb(dia.espacial) # matriz de viz com 1s e 0s
summary(dia.espacial.mv)
str(dia.espacial.mv)
print(dia.espacial.mv)
# matriz de pesos
dia.espacial.mp <- nb2listw(dia.espacial.mv, style="W")
summary(dia.espacial.mp,zero.policy=T)
print(dia.espacial.mp,zero.policy=T)
str(dia.espacial.mp)

# Calc do I de Moran dos residuos do modelo

lm.morantest(dia.mod, dia.espacial.mp, zero.policy = T)
# residuos sem dep espacial I = 0.018, p = 0.2238
###O p valor do exemplo foi significante. Eh valido o teste acima?
# Residuos vs var indep - Homocedasticidade
# plot dos riduos vs valores preditos

plot(dia.mod, 1) # outliers???
D0<-resid(dia.mod) # colocando os res em um vetor E0

# plot dos residuos vs var indepent q entrarm no modelo

# PPI_perc
par(mfrow = c(1,1))
plot(x = dia.espacial$PPI_perc, y = D0, xlab = "% PPI", ylab = "Residuals",cex.lab = 1.5)
abline(h = 0, lty = 2)
#Ok

# RZ_RENDA_c
par(mfrow = c(1,1))
plot(x = dia.espacial$RZ_RENDA_c, y = D0, xlab = "Renda per capita (raiz quadrada)", ylab = "Residuals",cex.lab = 1.5)
abline(h = 0, lty = 2)
#Ok

# lg_PES_DOM
par(mfrow = c(1,1))
plot(x = dia.espacial$lg_PES_DOM, y = D0, xlab = "Pessoa por domicilio (logaritmo)", ylab = "Residuals",cex.lab = 1.5)
abline(h = 0, lty = 2)
#Ok

# lg_densida
par(mfrow = c(1,1))
plot(x = dia.espacial$lg_densida, y = D0, xlab = "Densidade populacional (logaritmo)", ylab = "Residuals",cex.lab = 1.5)
abline(h = 0, lty = 2)
#Ok??

source('HighstatLibv9.R')
library(car)

############Olhando colineraridade entre as variáveis
dia.espacial.nesp <- as.data.frame(dia.espacial)
DiaVar <- c('PPI_perc','RZ_RENDA_c','lg_PES_DOM','lg_densida')
corvif(dia.espacial.nesp[,DiaVar])


