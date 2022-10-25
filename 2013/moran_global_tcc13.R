# ÍNDICE DE MORAN 2013

### Pacotes exigidos
library(spdep)

### Dados para análise espacial
colnames(dataset_final_br_13)

### Matriz de vizinhança
vizinhanca=poly2nb(dataset_final_br_13, queen=TRUE)
View(vizinhanca)
card.vizinhanca=as.data.frame(card(vizinhanca))
View(card.vizinhanca)

dataset_final_br_13$vizinhanca=card.vizinhanca
View(dataset_final_br_13)
nrow(dataset_final_br_13) ##??

mun_juntos=subset(dataset_final_br_13, dataset_final_br_13$vizinhanca!=0)
mun_juntos=mun_juntos%>%filter(!is.na(Perc_AF_100))
vizinhanca2=poly2nb(mun_juntos)

### Matriz de vizinhança normalizada
normal_viz= nb2listw(vizinhanca2, style = "W")
View(normal_viz)

### Índice Global de Moran
moran.test(mun_juntos$Perc_AF_100, listw = normal_viz)
##significativo!!! (PVALOR= 0,00000000....2 muito pequeno)
## Índice Global = 0,193 > 0, correlação positiva