## pacotes e funções auxiliares----
library(data.table)
library(dplyr)


## carregando indicadores ----
load("indicadores_municipais.rda")

## carregando shapefiles e vizinhanças geográficas ----
load("poligonos_viz.rda")

# inserindo nomes dos vizinhos
setnames(mun_viz,"mun_vizinho","codigo_mun7")
teste <- (dtb %>% select(codigo_mun7,nome_mun,sigla_uf))[mun_viz,on = "codigo_mun7"]
setnames(teste,c("mun_base","codigo_mun7"),c("codigo_mun7","codigo_mun7_viz"))

# consolidando nomes dos vizinhos
teste[,nome_mun_viz := paste0(nome_mun,"-",sigla_uf)]
mun_viz_cons <- teste[,list(codigo_mun7_viz = paste0(codigo_mun7_viz,collapse = "; \n"),
                            nome_mun_viz = paste0(nome_mun_viz,collapse = "; \n")),
                      by = c("codigo_mun7")]

# inserindo nomes dos vizinhos na dtb
dtb_viz <- mun_viz_cons[dtb,on = "codigo_mun7"]
