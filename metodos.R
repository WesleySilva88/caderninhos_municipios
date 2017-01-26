library(stringr)

# função 'arrumar_registro'
#' Tira caracteres não numéricos e imputa '0's à esquerda de um código identificador (cpf, cnpj, etc)
#' Usa a função 'str_pad' do pacote 'stringr'
#' @param x vetor com os códigos identificadores (cpf, cnpj, etc.)
#' @param digitos número de dígitos
#'
#' @return vetor com os códigos sem caracteres não numéricos, com 0's imputados à esquerda para completar
#' o número de dígitos especificados em 'digitos' 
#' @export
#'
#' @examples
arrumar_registro <- function(x,digitos){
  # passo 1: tirar caracteres não numéricos
  y <- gsub("[[:alpha:]]|[[:punct:]]|[[:space:]]","",x)
  
  # passo 2: completar com '0' à esquerda se for menor que 'digitos'
  z <- str_pad(y,digitos, pad = '0')
  
  z
}

#' função 'limpar_valores'
#' 'Limpa' caracteres especiais (como R$) de valores moretários/numéricos, retirando divisor de
#' milhar, fornecido pelo usuário, tratando divisor decimal e convertendo para numérico. Também
#' prevê a inserção de expressão regular informando representação de valor negativo.
#'
#' @param x vetor com os valores monetários e/ou numéricos de qualquer natureza que se quer ajustar
#' @param divmilhar caracter que separa os milhares. Se não houver um, manter como NULL
#' @param dec caracter que separa os decimais. Se não houver um, manter como NULL
#' @param negativo expressão regular que indica a representação de números negativos. 
#' Se não houver, manter NULL
#'
#' @return retorna um vetor numérico após a limpeza e conversão para numérico
#' @export
#'
#' @examples arrumar_valores(c("(R$ 222.395,350)","R$ 195.32","(R$ 20135.8)"), dec = ",",divmilhar = ".",negativo = "^\\(.*\\)$")
limpar_valores <- function(x, 
                            divmilhar = NULL,
                            dec = NULL,
                            negativo = NULL) {
  # passo 1: retirar divisor de milhares, se houver
  if(!is.null(divmilhar)) x <- gsub(divmilhar,"",x, fixed = T)
  
  # passo 2: substituir caracter de decimal por ".", se houver
  if(!is.null(dec)) x <- gsub(dec, ".", x, fixed = T)
  
  # passo 3: detectar expressão regular onde ocorre negativo
  if(!is.null(negativo)) x <- ifelse(grepl(negativo,x),paste0("-",x),x)
  
  # passo 3: tirar caracteres não numéricos
  y <- gsub("[[:alpha:]]|\\$|\\(|\\)|[[:space:]]", "", x)
  
  # passo 4: retornar em formatu numérico
  as.numeric(y)
}

# função 'verifica_cpf'
#' verifica se cpf é válido
#' Usa a função 'strsplit' e str_pad
#' @param x número CPF sem caracteres não numéricos (".","-", etc.)
#' 
#' @return retorna TRUE se o dígito verificador do CPF é válido.
#' @export
#'
#' @examples
verifica_cpf <- function(x){
  x <- str_pad(x,11,pad='0')
  vec_sep <- as.numeric(unlist(strsplit(x,"")))
  verif_dig <- vec_sep[10:11]
  vec_sep <- vec_sep[1:9]
  
  # passo 1: primeiro digito
  y <- sum(vec_sep*(10:2))
  
  #resto
  y <-  y %% 11
  y <- ifelse(y %in% 0:1,0,11-y)
  
  cpf_ok <- y == verif_dig[1]
  
  # passo 2: segundo digito
  vec_sep <- c(vec_sep,y)
  
  z <- sum(vec_sep*(11:2))
  #resto
  z <- z %% 11
  z <- ifelse(z %in% 0:1,0,11-z)
  
  cpf_ok <- cpf_ok & z == verif_dig[2]
  
  return(cpf_ok)
}

#' Extração interativa de metadados 
#' Função facilita a criação de código para produção de metadados, que depois deve ser colado no script
#' @param data dataframe/datatable cujas colunas se pretende descrever
#'
#' @return
#' @export
#'
#' @examples
extrair_metadados <- function(data){
  x <- unlist(
    lapply(
      names(data),
      function(x) {
        readline(
          prompt = paste0(
            x,
            ": "
          )
        )
      }
    )
  )
  names(x) <- names(data)
  printar <- paste0(names(x)," = '",x,"'")
  cat(printar, sep = ", \n")
  x
}

#' Title estruturar_string
#' Retorna um vetor de novos nomes estruturados, sem repetição, minúsculos, sem caracter especial e com "_" no lugar de "."
#'
#' @param x vetor de strings
#' @param encoding encogin da base, sendo na maiorria 'utf8', 'latin1' OU 'Windows-1252
#'
#' @return vetor com novos nomes padronizados
#' @export
#'
#' @examples
estruturar_string <- function(x, encoding = "utf8") {
  x %>%
    iconv(from = encoding, to = "ASCII//TRANSLIT") %>% # tira caracteres de a acentuação
    tolower %>% ## tudo em minúsculo
    gsub(pattern = "[[:space:]]{1,}", replacement = "_") # substituindo 1 ou mais espaços por "_"
}

#' Title estruturar_nomes
#' Retorna um vetor de novos nomes estruturados, sem repetição, minúsculos, sem caracter especial e com "_" no lugar de "."
#'
#' @param data data.frame ou data.table 
#' @param encoding encogin da base, sendo na maiorria 'utf8', 'latin1' OU 'Windows-1252
#'
#' @return vetor com novos nomes padronizados
#' @export
#'
#' @examples
estruturar_nomes <- function(data, encoding = "utf8"){
  colunas_validas <- data %>%
    names %>%
    estruturar_string(encoding) %>% # pré-estrutura tirando acento, ".", espaço, etc.
    make.names(unique = T) %>% # repetidos são concatenados a números
    gsub(pattern = "\\.", replacement = "_") %>% # substituindo "." por "_"
    gsub(pattern = "_{2,}",replacement = "_") # tirando "_" múltilpos
  
  ## retorna colunas estruturadas
  colunas_validas
}


#' Title last_nao_na
#' Complementa vazios de um vetor pegando a última informação não NA
#' @param x vetor de números e/ou valores
#'
#' @return vetor complementado
#' @export
#'
#' @examples
last_nao_na <- function(x){
  y = x
  for(i in 2:length(x)){
    if(is.na(x[i])) y[i] <- y[i-1]
  }
  y
}


#' Combinações parciais
#' função prepara todas as combinações de 'm' elementos de um vetor, em forma de dataframe
#' @param x vetor de elementos a serem combinados
#' @param m número de elementos de 'x' para criar combinações
#'
#' @return
#' @export
#'
#' @examples
combn_parc <- function(x,m){
  x = (t(combn(x, m)))
  split(x,seq(NROW(x)))
}

## 
#' Combinações marginais
#' função junta todas as combinações parciais, de todos os tamanhos de um vetor x
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
comb_margins <- function(x){
  xx = lapply(seq_along(x),FUN = combn_parc,x = x)
  xy = unlist(xx,recursive = F)
  names(xy) <- NULL
  xy
}

length_distinct <- function(x){
  length(unique(x))
}


encontrar_constantes <- function(data,keyvars,valuevars){
  stopifnot(is.data.table(data), length(keyvars) > 0L,length(valuevars) >= 1L)
  
  # cria as combinações possíveis em 'margens'
  cmar <- comb_margins(keyvars)
  
  data <- setorderv(data,keyvars)
  
  # criando totais e subtotais
  ti <- Sys.time()
  aggrs <- rbindlist(
    c(
      # subtotais por categoria
      lapply(cmar,
             function(g){
               data %>% 
                 # select(one_of(g), one_of(valuevars)) %>% 
                 # unique %>% 
                 .[,lapply(.SD, function(x) max(length_distinct(x))),
                   by = c(g)] %>% 
                 .[, lapply(.SD,max, na.rm = T), .SDcols = valuevars] %>% 
                 .[, grupo := paste(g, collapse = "; ")]
             }
      )
    ),
    use.names = TRUE, fill = TRUE
  ) %>% 
    melt(id.vars = "grupo",
         measure.vars = valuevars,
         variable.name = "variavel",
         value.name = "ndiff") %>%
    .[,mindiff := min(ndiff),by = variavel] %>% 
    subset(ndiff == mindiff) %>%
    .[,lapply(.SD,head,1), by = variavel, .SDcols = c("grupo","ndiff")]
  (tf <- difftime(Sys.time(),ti))
  return(aggrs)
}



#' Title função 'formatar_num': acrescenta big.mark = "." e decimal.mark = ",", de acordo com o número de dígitos escolhido.
#' função 'formatar_num': acrescenta big.mark = "." e decimal.mark = ",", de acordo com o número de dígitos escolhido.
#' @param x vetor ou número
#' @param digitos número de dígitos que irão aparecer
#'
#' @return
#' @export
#'
#' @examples
formatar_num <- function(x,digitos){
  formatC(x,
          big.mark = ".",
          decimal.mark = ",",
          format = "f",
          digits = digitos)
}

