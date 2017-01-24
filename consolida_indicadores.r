library(tcltk)
library(taRifx)
library(dplyr)
library(plyr)
library(data.table)
library(googlesheets)


## diretórios
dir.fonte <- "C:\\Users\\wesley.jesus\\Documents"
dir.cons  <- file.path(dir.fonte,"Bases Consolidadas")

## url da planilha de indicadores
url_planilha <- "https://docs.google.com/spreadsheets/d/1EAAlkpZ91kUaDl-6c3LY_lfchX2NfWjiahmji25MBtU/pub?output=csv"
url_planilha <- gs_url(url_planilha)

## baixando planilha de indicadores (worksheet 3)
indicadores <- gs_read(url_planilha,ws = 3,verbose = F)
indicadores <- as.data.table(indicadores)
setnames(indicadores,tolower(names(indicadores)))

## filtrando apenas onde existe uma TAB definida
indicadores <- subset(indicadores,!is.na(tabela))

## carregar cada TAB
for(tab in indicadores[,tabela %>% unique]){
  cat("carregando tabela",tab,"\n")
  
  # endereçando o arquivo
  arquivo <- file.path(dir.cons,
                       paste0(tab,".csv"))
  
  df <- try(fread(arquivo),silent = T)
  if(!inherits(df,"try-error")){
    assign(tab,df)
  }
  rm(df)
}

## vetores que começam com "TAB"
bases <- ls(pattern = "TAB")

## mergeando tabelas carregadas
rm(indicadores_municipais)
for(df in bases){
  cat("tratando tabela",df,"\n")
  # filtrando a variável de interesse
  variavel <- indicadores[tabela == df,coluna]
  
  # pegando tabela e mergeando, caso objeto 'indicadores_anuais' exista
  tab_df <- get(df) %>% select(codigo_mun7, ano, one_of(variavel))
  
  # ano e municipio como character
  tab_df[,`:=`(ano = as.character(ano),
               codigo_mun7 = as.character(codigo_mun7))]
  
  if(df == "TAB_0104"){
    anoi <- min(tab_df$ano);anof <- max(tab_df$ano)
    tab_split <- split(tab_df,f = tab_df["ano"])
    tab_temp <- subset(tab_df,ano == anoi)
    for(a in (anoi + 1):anof){
      if(a %in% tab_df$ano){
        tab_temp <- rbind.fill(tab_temp,tab_split[[as.character(a)]])
      }else{
        prov <- subset(tab_temp,ano == a - 1)
        prov$ano = a
        tab_temp <- rbind.fill(tab_temp,prov)
      }
    }
    tab_df <- tab_temp
  }
  if(exists("indicadores_municipais")){
    indicadores_municipais <- merge(indicadores_municipais,
                                tab_df,
                                by = c("codigo_mun7","ano"),
                                all = T)
  }else{
    indicadores_municipais <- tab_df
  }
}

## limpando:
# municípios com menos de 7 dígitos
# dados posteriores a 1999 (por enquanto)
# tirando quem tem nome NA ou código começando em '999'
indicadores_municipais <- indicadores_municipais %>% subset(ano >= 1999 & nchar(codigo_mun7) == 7)
indicadores_municipais <- indicadores_municipais %>% subset(!is.na(nome_mun))
indicadores_municipais <- indicadores_municipais %>% subset(substr(codigo_mun7,1,1) != '9')

# ordenando por município e ano
setorder(indicadores_municipais,codigo_mun7,ano)

# pegando algumas informações da história do município
indicadores_municipais[,ano_surg := min(ano),by = codigo_mun7]
indicadores_municipais[,ano_surg := ifelse(ano_surg == 1999,"1999 ou antes",ano_surg)]
dtb <- indicadores_municipais[,lapply(.SD,tail,1),
                              .SDcols = c("nome_mun","nome_micro","nome_meso","sigla_uf"),
                              by = c("codigo_mun7","ano_surg")]

# salvando painel anual
save(list = c("indicadores_municipais","indicadores","dtb"),
     file = "caderninhos_municipios/indicadores_municipais.rda")
