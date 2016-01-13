###################################################
## Las rutinas en este script, tienen como finalidad
## automatizar el proceso de validación de datos.
## Se revisan distintos tipos de datos como fechas,
## entidades federativas y coordenadas.
###################################################


###################################################
## Librerías utilizadas
###################################################
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(xlsx))
suppressPackageStartupMessages(library(stringdist))
###################################################

###################################################
##---------------------------------
## Funciones
##---------------------------------
###################################################

###################################################
############### Sección 1: FECHAS #################
###################################################
## Código que identifica, revisa y corrige fechas
###################################################
###################################################
##---------------------------------
## date_pre_proc
##---------------------------------
## Algo de estructura para evitar clasificación incorrecta.
###################################################
date_pre_proc <- function(date){
    new_date <- str_split(date,"[[:punct:]]")[[1]]
    new_date[str_length(new_date) < 2] <- paste0("0",new_date[str_length(new_date) < 2])
    new_date <- new_date[order(str_length(new_date), decreasing = TRUE)]
    ## Obtener día
    index <- which(new_date %in% seq(12,31,1))
    day <- new_date[index]
    new_date <- new_date[-index]
    new_date[3] <- day
    ##
    paste(new_date,collapse = "-")
}
###################################################
##---------------------------------
## most_simil
##---------------------------------
###################################################
most_simil <- function(char1, char2){
    ## Identifica similaridad entre caracteres usando diversas métricas
    ## OSA (Restricted Damerau-Levenshtein): inserciones, eliminaciones,
    ## reemplazos y transposiciones pero cada subcadena sólo puede ser
    ## editada una vez.
    ## LV (Levenshtein): inserciones, eliminaciones y reemplazos.
    ## DL (FULL Damerau-Levenshtein): inserciones, eliminaciones,
    ## reemplazos y transposiciones (menor o igual a Levenshtein).
    ## LCS: subcadena de caracteres más larga en común
    max_str_length <- max(str_length(char1), str_length(char2))
    max_unique_length <- max(
        length(unique(str_split(char1,"")[[1]])),
        length(unique(str_split(char2,"")[[1]]))
    )
    ## Elección de una q razonable: en fechas 3
    ## PENDIENTE
    ## hagamos que todas las métricas estén entre 0,1
    res <-     list(
        osa       = 1 - stringdist(char1, char2, method = "osa")/max_str_length,
        lv        = 1 - stringdist(char1, char2, method = "lv")/max_str_length,
        dl        = 1 - stringdist(char1, char2, method = "dl")/max_str_length,
        lcs       = 1 - stringdist(char1, char2, method = "lcs")/max_str_length,
        ## Es necesario usar el máximo con 0
        ## ya que cuando alguna de las cadenas es
        ## vacía, se va a -Inf.
        cosine    = max(1 - stringdist(char1, char2, method = "cosine",q = 3),0),
        jaccard   = max(1 - stringdist(char1, char2, method = "jaccard",q = 3),0)
    )
    max(unlist(res))
}

###################################################
##---------------------------------
## most_simil_mult
##---------------------------------
###################################################
most_simil_mult <- function(char, bag){
    char <- date_pre_proc(char)
    ## Primera Iteración, los que tienen los elementos más parecidos
    simil <- laply(bag, function(t)t <- (1-stringdist(char, t, method = "jaccard", q = 1)))
    res <-
    list(simil = max(simil),
         char  = unique(bag[which(simil == max(simil))]))
    ## Segunda Iteración, combinación de dos más parecidos
    ##simil <- laply(res[[2]], function(t)t <- (1-stringdist(char, t, method = "jaccard", q = 1)))
    ##res <-
    ##list(simil = max(simil),
      ##   char  = unique(res[[2]][which(simil == max(simil))]))
    ## Tercera Iteración, diversas métricas
    simil <- laply(res[[2]], function(t)t <- most_simil(char, t))
    res <-
    list(simil = max(simil),
         char  = unique(res[[2]][which(simil == max(simil))]))
    ## Devolvemos resultados
    res
}

###################################################
##---------------------------------
## prob.date
##---------------------------------
###################################################
prob.date <- function(col, date_bag = date_base){
    ## Recorre e inspecciona todas las entradas de col.
    ## y determina si estas cumplen con un patrón estandar de fecha.
    ## IN
    ## col: la columna que se quiere inspeccionar.
    ## OUT
    ## el porcentaje de entradas que pudieran ser fechas.
    ##---------------------------------
    ## El patrón 1 comprende las fechas que contienen sólo dígitos.
    ## pattern1    <-
    ##    '(((0?[1-9]{1}|[1,2]{1}[0-9]{1}|3{1}[0-1]{1})|(0?[1-9]{1}|1{1}[0-2])|(1{1}[0-9]{4}|20{1}(0{1}[0-9]{1}|1{1}[0-5]{1})))[[:punct:]]?){1,3}'
    ## '(((0{1}[1-9]{1}|[1,2]{1}[0-9]{1}|3{1}[0-1]{1})|(0{1}[1-9]{1}|1{1}[0-2])|(1{1}[0-9]{4}|20{1}(0{1}[0-9]{1}|1{1}[0-5]{1})))[[:punct:]]?){1,3}'
    ## El patrón 2 comprende las fechas que contienen dígitos y caractéres.
    ## pattern2    <-
    ##    '([0-9]{1,}[[:punct:]]?([A-Z]|[a-z]){3,9}[[:punct:]]?[0-9]{1,})'
    ## El patrón 3 comprende todos los patrones de fechas anteriores almazenadas.
    ## Cuando la bolsa de fechas crezca demasiado,
    ## será necesario utilizar LSH (locality sensitve hashing)
     pattern3    <- date_bag
    ## En caso de que las fechas tengan hora,
    ## obtenemos la primera columna exclusivamente...??????
    ## col.match   <- ldply(str_split(col," "),function(t)t<- t[[1]][1])
    ## Calculamos el número de observaciones que cumplen con el patrón 1
    ## true_match1 <- na.omit(str_length(str_match(col,pattern1)[,1]) ==
    ##                          str_length(col.match[,1]))
    ## Calculamos el número de observaciones que cumplen con el patrón 2
    ##true_match2 <- na.omit(str_length(str_match(col,pattern2)[,1]) ==
    ##                          str_length(col.match[,1]))
    ## Calculamos el número de observaciones que cumplen con el patrón 3
    if(!is.null(date_bag)){
    similitud   <- llply(col.match, function(t)t <- most_simil_mult(t, date_bag))
        true_match3 <- laply(similitud, function(t)t <- similitud[[1]])
    }else{
        true_match3 <- 0
    }
    ## Regrar máximo porcentaje de valores que cumplen con algún patroń.
    max(
        c(
            sum(true_match1)/length(col),
            sum(true_match2)/length(col),
            mean(true_match3)
          )
        )
}

###################################################
##---------------------------------
## ident.date
##---------------------------------
###################################################
ident.date <- function(df, thresh = .7){
    ## Aplica prob.date a todas las columnas de ident.date y determina si la
    ## proporción de entradas que cumplen con el patrón es mayor que thresh
    ## IN
    ## df: data.frame de la que se quieren verificar las columnas
    ## OUT
    ## variable booleana que identifica a las columnas que sobrepasan thresh.
    res <- c()
    if(!is.null(dim(df))){
        res <- apply(df, 2,function(t) t <- prob.date(t)) > thresh
    }else{
        res <- laply(df,function(t) t <- prob.date(t)) > thresh
    }
    res
}

###################################################
##---------------------------------
## Pruebas
##---------------------------------
###################################################
## Generamos base de fechas
###################################################
date_base <- seq(as.Date("1980-01-01"), today(), "days")
###################################################

data <- read.csv("data.txt",
                stringsAsFactors = FALSE)
data <- data[-1,]
test <- ident.date(data)
