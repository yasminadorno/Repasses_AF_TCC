# Dados de Repasses para o PNAE 2019
library(readxl)
library(dplyr)

# ANO = 2019
repasses_br_19=read_excel("C:/Users/Yasmin/Documents/NEPA-TCC/Repasses/repasses_sp_campinas.xlsx", sheet=7)
View(repasses_br_19)
options(OutDec = ",")
options(digits = 4)
nrow(repasses_br_19)
colnames(repasses_br_19)<-c("Geocod","UF","Perc_af")
repasses_br_19= repasses_br_19%>%select(1,2,3)
head(repasses_br_19)

### Ajeitando as Classes
repasses_br_19$Geocod = as.character(repasses_br_19$Geocod)
repasses_br_19$Perc_af = as.numeric(repasses_br_19$Perc_af)
class(repasses_br_19$Perc_af)

### Trazer valores negativos para 0
repasses_br_19_001=repasses_br_19 %>%
  mutate_if(is.numeric, function(x) {x[x < 0] <- 0; x})
View(repasses_br_19_001)

### Atribuir 1 para valores maiores que 1 
#### Considerando a baixa representatividade de valores maiores que 1 e a fim de facilitar a visualização gráfica
#### Considerando tb que quem adquire mais que o recurso adquire todo o recurso
repasses_br_19_001$Perc_af[repasses_br_19_001$Perc_af>1]=1

### Imputação simples para dados faltantes (NA)=mean
repasses_br_19_na=repasses_br_19_001%>%filter(is.na(Perc_af))
nrow(repasses_br_19_na) ## pouquissimos dados faltantes (3%), o que justifica a imputação simples pela média

#  Adicionar o valor da média
repasses_br_19_notna=repasses_br_19_001%>%filter(!is.na(Perc_af))
mean(repasses_br_19_notna$Perc_af) ##0,43 -> valor que vamos imputar

repasses_br_19_001 <- mutate_at(repasses_br_19_001, c("Perc_af"), ~replace(., is.na(.), 0.43))


### Transformar decimais em percentual
repasses_br_19_002=repasses_br_19_001%>%mutate(100*Perc_af)
colnames(repasses_br_19_002)
repasses_br_19_002=repasses_br_19_002%>%rename("Perc_AF_100"="100 * Perc_af")
View(repasses_br_19_002)

### Estatísticas Descritivas
summary(repasses_br_19_002$Perc_AF_100)
sd(repasses_br_19_002$Perc_AF_100)

### Municípios que não adquiriram
mun_0=repasses_br_19_002%>%filter(Perc_AF_100==0)
nrow(mun_0) ## 453 (8,1%)

mun_30=repasses_br_19_002%>%filter(Perc_AF_100<30)
nrow(mun_30) ## 1754 (31,5%)

mun_50=repasses_br_19_002%>%filter(Perc_AF_100>50)
nrow(mun_50) ## 1825 ()

### Histograma
hist(repasses_br_19_002$Perc_AF_100,col="darkblue", border="black", prob=T, xlab="% Aquisições da AF - 2019",
     ylab="Frequência", main="");
# indicar mediana e média
abline(v = c(median(repasses_br_19_002$Perc_AF_100), mean(repasses_br_19_002$Perc_AF_100)),
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

all_mun_br_19 <- read_municipality(year=2019) #all
colnames(all_mun_br_19)

# Dados dos repasses
colnames(repasses_br_19_002)
class(repasses_br_19_002$Geocod)
class(all_mun_br_19$code_muni)
all_mun_br_19$code_muni=as.character(all_mun_br_19$code_muni)

save(all_mun_br_19, file = "all_mun_br_19.Rda")
save(repasses_br_19_002, file="repasses_br_19_002.Rda")
# Juntar dados do mapa com dados a serem trabalhados
dataset_final_br_19 = left_join(all_mun_br_19, repasses_br_19_002, by=c("code_muni"="Geocod"))
colnames(dataset_final_br_19)
View(dataset_final_br_19)
nrow(dataset_final_br_19)
save(dataset_final_br_19, file="dataset_final_br_19.Rda")

grafico_br_19<-ggplot() +
  geom_sf(data=dataset_final_br_19, aes(fill=Perc_AF_100), color= NA, size=.15)+
  labs(title="% Aquisições AF - 2019",
       caption='', size=8)+
  scale_fill_distiller(palette = "YlGnBu",direction = 1, limits=c(0.0, 100),
                       name="")+
  theme_minimal(base_size = 10, base_family = "")

grafico_br_19


### Boxplot total e segmentado por região...
colnames(dataset_final_br_19)

## total
boxplot(dataset_final_br_19$Perc_AF_100, col="blue", ylab="% Aquisições da AF", main=
          "", border="black", notch=T, pch=19)

## por região
dataset_final_br_19=na.omit(dataset_final_br_19)
ggplot(data=dataset_final_br_19, mapping = aes(x=name_region, y=Perc_AF_100))+
  geom_boxplot(col="black", fill="blue")+
  stat_summary(aes(linetype = "mediana"),
               geom = "errorbar",
               fun.min = median, fun.max = median,
               width = .66, size = 1)+
  stat_summary(aes(shape = "média"),
               geom = "point", 
               fun = base::mean) +
  ylab("% Aquisições da AF - 2019")+
  xlab("Região")+ggtitle("")+
  theme_classic()+
  theme(legend.position = "top")

### Para facilitar a análise do boxplot... estats. descritivas por região
colnames(dataset_final_br_19)
unique(dataset_final_br_19$name_region)
# Sul
percent_sul19=dataset_final_br_19%>%filter(name_region=="Sul")
summary(percent_sul19$Perc_AF_100) ## mediana 57,5 média 60,5

# Centro Oeste
percent_co=dataset_final_br_19%>%filter(name_region=="Centro Oeste")
summary(percent_co$Perc_AF_100) ## mediana 30,2 média 30,8

# Nordeste
percent_ne=dataset_final_br_19%>%filter(name_region=="Nordeste")
summary(percent_ne$Perc_AF_100) ## mediana 34,8 média 37,6

# Norte
percent_norte=dataset_final_br_19%>%filter(name_region=="Norte")
summary(percent_norte$Perc_AF_100) ## mediana 32,8 média 35,6

# Sudeste
percent_se=dataset_final_br_19%>%filter(name_region=="Sudeste")
summary(percent_se$Perc_AF_100) ## mediana 37,8 média 40,1

### SALVAR EM SHP: shapefile do IBGE
library(rgdal)
shp <- readOGR(dsn=path.expand("C:/Users/Yasmin/Documents/NEPA-TCC/Bases de dados FNDE - SIGPC/AquisicoesAF-TCC/brmun_shp"), layer="BR_Municipios_2021")
class(shp)
head(shp)
colnames(repasses_br_19_002)

## Juntar base de repasses com shapefile
shp_sp_af=merge(shp,repasses_br_19_002, by.x = "CD_MUN", by.y = "Geocod")
colnames(repasses_br_19_002)
colnames(shp_sp_af)

### Tratamento dos dados
proj4string(shp_sp_af) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

Encoding(shp_sp_af$NM_MUN) <- "UTF-8"

### Salvar
class(shp_sp_af)
writeOGR(shp_sp_af, dsn = "repasses19_003.shp", layer="repasses19_003", driver="ESRI Shapefile")
