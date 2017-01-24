library(rgdal)
library(maptools)
library(spdep)
library(rgeos)
library(rgdal)
library(data.table)
library(dplyr)

## diretórios
dir.fonte <- "X:\\Bases NEAD\\Fontes Externas"
dir_cartografias <- file.path(dir.fonte,"Cartografias")

# endereçando arquivos
dir_mun <- (file.path(dir_cartografias,
                      "Limites Territoriais",
                      "2014",
                      "Municipal"))

dir_uf  <- (file.path(dir_cartografias, 
                      "Limites Territoriais",
                      "2014",
                      "Estadual"))

# carregando malha municipal
mun_bra2014 <- readOGR(dsn = dir_mun,layer = "brasil_mun2014")
names(mun_bra2014)

# carregando malha estadual
# uf_bra2014 <- readOGR(dsn = dir_uf,layer = "brasil_est2014")
# names(uf_bra2014)

# simplificando geometria para acelerar produção de mapas
shp_mun <- SpatialPolygonsDataFrame(gSimplify(mun_bra2014,
                                              tol = 0.005,
                                              topologyPreserve = T),
                                    mun_bra2014@data)


# colunas do shp municipal
setnames(shp_mun@data, old  = "CD_GEOCMU", new =  "codigo_mun7")


# criando matriz de vizinhança
mnb <- poly2nb(mun_bra2014,mun_bra2014$CD_GEOCMU)
mnb.mat <- nb2mat(mnb,zero.policy=T,style = "B")

# ajuste na matriz de vizinhos
mnb.mat <- 1-mnb.mat
colnames(mnb.mat) <- row.names(mnb.mat)
diag(mnb.mat) <- 0


# checando o tamanho da matriz de vizinhança
mnb.mat %>% object.size() %>% format(units = "Mb")

# transformando de matriz para data.table
mnb.dt <- as.data.table(mnb.mat)
mnb.dt[, mun_base := row.names(mnb.mat)]

# transpondo vizinhos de colunas para linhas
mnb.dt_t <- melt(data = mnb.dt,
                 idvars = "mun_base",
                 variable.name = "mun_vizinho",
                 value.name = "ind_vizinho")

# setando a chave no município "base"
setkey(mnb.dt, mun_base)

# filtrando onde é 0 (ou seja, é vizinho)
mun_viz <- subset(mnb.dt_t,ind_vizinho == 0)

# medindo o tamanho da base filtrada
mun_viz %>% object.size() %>% format(units = "Mb")

#
## SALVANDO INDICADORES ----
#

save(list = c("shp_mun","mun_viz"),
     file = "caderninhos_municipios/poligonos_viz.rda")
