# Dados de Repasses para o PNAE 2013
library(readxl)
library(dplyr)

# ANO = 2013
repasses_br_13=read_excel("C:/Users/Yasmin/Documents/NEPA-TCC/Repasses/repasses_sp_campinas.xlsx", sheet=5)
View(repasses_br_13)
options(OutDec = ",")
nrow(repasses_br_13)
colnames(repasses_br_13)<-c("Geocod","UF","Perc_af")
head(repasses_br_13)

### Ajeitando as Classes
repasses_br_13$Geocod = as.character(repasses_br_13$Geocod)
repasses_br_13$Perc_af = as.numeric(repasses_br_13$Perc_af)
class(repasses_br_13$Perc_af)

### Trazer valores negativos para 0
repasses_br_13_001=repasses_br_13 %>%
  mutate_if(is.numeric, function(x) {x[x < 0] <- 0; x})
View(repasses_br_13_001)

### Atribuir 1 para valores maiores que 1 
#### Considerando a baixa representatividade de valores maiores que 1 e a fim de facilitar a visualização gráfica
#### Considerando tb que quem adquire mais que o recurso adquire todo o recurso
repasses13_maiorqur1=repasses_br_13%>%filter(Perc_af>1)
head(repasses13_maiorqur1)
nrow(repasses13_maiorqur1)
repasses_br_13_001$Perc_af[repasses_br_13_001$Perc_af>1]=1

### Imputação simples para dados faltantes (NA)=mean
repasses_br_13_na=repasses_br_13_001%>%filter(is.na(Perc_af))
nrow(repasses_br_13_na) ## pouquissimos dados faltantes (0,6%), o que justifica a imputação simples pela média

#  Adicionar o valor da média
repasses_br_13_notna=repasses_br_13_001%>%filter(!is.na(Perc_af))
mean(repasses_br_13_notna$Perc_af) ##0,227 -> valor que vamos imputar

repasses_br_13_001 <- mutate_at(repasses_br_13_001, c("Perc_af"), ~replace(., is.na(.), 0.227))


### Transformar decimais em percentual
repasses_br_13_002=repasses_br_13_001%>%mutate(100*Perc_af)
colnames(repasses_br_13_002)
repasses_br_13_002=repasses_br_13_002%>%rename("Perc_AF_100"="100 * Perc_af")
View(repasses_br_13_002)

### Estatísticas Descritivas
summary(repasses_br_13_002$Perc_AF_100)
sd(repasses_br_13_002$Perc_AF_100)

### Municípios que não adquiriram
mun_0=repasses_br_13_002%>%filter(Perc_AF_100==0)
nrow(mun_0) ## 1139 (21,5%)

mun_10=repasses_br_13_002%>%filter(Perc_AF_100<10)
nrow(mun_10) ## 1899 (34%)

mun_30=repasses_br_13_002%>%filter(Perc_AF_100<30)
nrow(mun_30) ## 3670 (66,8%)

mun_50=repasses_br_13_002%>%filter(Perc_AF_100>50)
nrow(mun_50) ## 507 (9,1%)


### Histograma
hist(repasses_br_13_002$Perc_AF_100,col="darkblue", border="black", prob=FALSE, xlab="% Aquisições da AF - 2013",
     ylab="Frequência", main="");
# indicar mediana e média
abline(v = c(median(repasses_br_13_002$Perc_AF_100), mean(repasses_br_13_002$Perc_AF_100)),
       col = c("blue", "red", "orange"),
       lwd = c(2,2),
       lty=c(2,2));
# legenda
legend(x="topright", #posicao da legenda
       c("Mediana","Media"), #nomes da legenda
       col=c("blue","red"), #cores
       lty=c(2,2), #estilo da linha
       lwd=c(2,2)) #grossura das linhas

### Plotar mapa temático!
# install.packages("ggplot2")
#install.packages("sf")
# install.packages("dplyr")
#install.packages("rio")
# install.packages("readr")
#install.packages("ggpubr")
#install.packages("Rcpp")
library(geobr)
library(ggplot2)
library(sf)
library(dplyr)
library(rio)
library(readr)
library(ggpubr)
library(Rcpp)

# Dados do mapa do brasil
metadata<-download_metadata() # para ver codigos
head(metadata)

all_mun_br_13 <- read_municipality(year=2013) #all
colnames(all_mun_br_13)

# Dados dos repasses
colnames(repasses_br_13_002)
class(repasses_br_13_002$Geocod)
class(all_mun_br_13$code_muni)
all_mun_br_13$code_muni=as.character(all_mun_br_13$code_muni)

save(all_mun_br_13, file = "all_mun_br_13.Rda")
save(repasses_br_13_002, file="repasses_br_13_002.Rda")
# Juntar dados do mapa com dados a serem trabalhados
dataset_final_br_13 = left_join(all_mun_br_13, repasses_br_13_002, by=c("code_muni"="Geocod"))
colnames(dataset_final_br_13)
View(dataset_final_br_13)
save(dataset_final_br_13, file="dataset_final_br_13.Rda")

grafico_br_13<-ggplot() +
  geom_sf(data=dataset_final_br_13, aes(fill=Perc_AF_100), color= NA, size=.15)+
  labs(title="% Aquisições da AF - 2013",
       caption='', size=8)+
  scale_fill_distiller(palette = "YlGnBu",direction = 1, limits=c(0.0, 100),
                       name="")+
  theme_minimal(base_size = 10, base_family = "")

grafico_br_13

### Boxplot total e segmentado por região...
colnames(dataset_final_br_13) ## problema: a base de 2013 não tem infos de região.. vou fazer um merge com 2019

## Obter regiões a partir do all_mun_br_19
regions=all_mun_br_19%>%select(1,7)
head(regions)
regions=as.data.frame(regions)
regions=regions%>%select(1,2)

## Left_join entre dataset e regions
dataset_final_br_13_001=merge(dataset_final_br_13, regions, by="code_muni")
nrow(dataset_final_br_13)
nrow(dataset_final_br_13_001)
head(dataset_final_br_13_001)

## total
boxplot(dataset_final_br_13$Perc_AF_100, col="blue", ylab="% Aquisições da AF", main=
          "", border="black", notch=T, pch=13)

## por região
dataset_final_br_13=na.omit(dataset_final_br_13_001)
ggplot(data=dataset_final_br_13_001, mapping = aes(x=name_region, y=Perc_AF_100))+
  geom_boxplot(col="black", fill="blue")+
  stat_summary(aes(linetype = "mediana"),
               geom = "errorbar",
               fun.min = median, fun.max = median,
               width = .66, size = 1)+
  stat_summary(aes(shape = "média"),
               geom = "point", 
               fun = base::mean) +
  ylab("% Aquisições da AF - 2013")+
  xlab("Região")+ggtitle("")+
  theme_classic()+
  theme(legend.position = "top")

### Para facilitar a análise do boxplot... estats. descritivas por região
colnames(dataset_final_br_13)
unique(dataset_final_br_13$name_region)
# Sul
percent_sul=dataset_final_br_13%>%filter(name_region=="Sul")
summary(percent_sul$Perc_AF_100) ## mediana 32,4 média 34,6
summary(percent_sul19$Perc_AF_100) ## mediana 57,5 e 60,5
## Aumento percentual: 25,9 pp

# Centro Oeste
percent_co13=dataset_final_br_13%>%filter(name_region=="Centro Oeste")
summary(percent_co13$Perc_AF_100) ## mediana 12,5 média 18,2
summary(percent_co$Perc_AF_100) ## mediana 30,2 média 30,7
## Aumento percentual: 12,5 pp

# Nordeste
percent_ne13=dataset_final_br_13%>%filter(name_region=="Nordeste")
summary(percent_ne13$Perc_AF_100) ## mediana 15,7 média 17,8
## 2019 mediana 34,8 média 37,6
## aumento percentual: 19,8 pp

# Norte
percent_norte13=dataset_final_br_13%>%filter(name_region=="Norte")
summary(percent_norte13$Perc_AF_100) ## mediana 12,85 média 17,71
## 2019 mediana 32,8 média 35,6
## aumento percentual: 17,9 p.p

# Sudeste
percent_se13=dataset_final_br_13%>%filter(name_region=="Sudeste")
summary(percent_se13$Perc_AF_100) ## mediana 20,8 média 22,2
## 2019 mediana 37,8 média 40,1 
## aumento percentual:18 p.p

### SALVAR EM SHP: shapefile do IBGE
library(rgdal)
shp <- readOGR(dsn=path.expand("C:/Users/Yasmin/Documents/NEPA-TCC/Bases de dados FNDE - SIGPC/AquisicoesAF-TCC/brmun_shp"), layer="BR_Municipios_2021")
class(shp)
head(shp)
colnames(repasses_br_13_002)

## Juntar base de repasses com shapefile
shp_sp_af=merge(shp,repasses_br_13_002, by.x = "CD_MUN", by.y = "Geocod")
nrow(repasses_br_13_002)
nrow(shp_sp_af)

### Tratamento dos dados
proj4string(shp_sp_af) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

Encoding(shp_sp_af$NM_MUN) <- "UTF-8"

### Salvar
class(shp_sp_af)
writeOGR(shp_sp_af, dsn = "repasses13_003.shp", layer="repasses13_003", driver="ESRI Shapefile")
