###################################################
#####################FECHAS########################
###################################################
## Código que identifica, revisa y corrige fechas
###################################################
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(xlsx))
suppressPackageStartupMessages(library(stringdist))
###################################################
##---------------------------------
## Funciones
##---------------------------------
###################################################
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
    ## Elección de una q razonable: en fechas 3
    ## PENDIENTE
    ## hagamos que todas las métricas estén entre 0,1
    res <-     list(
        osa       = 1 - stringdist(char1, char2, method = "osa")/max_str_length,
        lv        = 1 - stringdist(char1, char2, method = "lv")/max_str_length,
        dl        = 1 - stringdist(char1, char2, method = "dl")/max_str_length,
        lcs       = 1 - stringdist(char1, char2, method = "lcs")/max_str_length,
        ## Es necesario usar el máximo con 0 ya que cuando alguna de las cadenas es
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
    simil <- laply(bag, function(t)t <- most_simil(char, t))
    list(simil = max(simil), char = which(simil == max(simil)))
}
###################################################
##---------------------------------
## prob.date
##---------------------------------
###################################################
prob.date <- function(col, date_bag = NULL){
    ## Recorre e inspecciona todas las entradas de col.
    ## y determina si estas cumplen con un patrón estandar de fecha.
    ## IN
    ## col: la columna que se quiere inspeccionar.
    ## OUT
    ## el porcentaje de entradas que pudieran ser fechas.
    ##---------------------------------
    ## El patrón 1 comprende las fechas que contienen sólo dígitos.
    pattern1    <-
        '(((0{1}[1-9]{1}|[1,2]{1}[0-9]{1}|3{1}[0-1]{1})|(0{1}[1-9]{1}|1{1}[0-2])|(1{1}[0-9]{4}|20{1}(0{1}[0-9]{1}|1{1}[0-5]{1})))[[:punct:]]?){1,3}'
    ## El patrón 2 comprende las fechas que contienen dígitos y caractéres.
    pattern2    <- '([0-9]{1,}[[:punct:]]?([A-Z]|[a-z]){3,9}[[:punct:]]?[0-9]{1,})'
    ## El patrón 3 comprende todos los patrones de fechas anteriores almazenadas.
    ## Cuando la bolsa de fechas crezca demasiado, será necesario utilizar LSH (locality sensitve hashing)
    pattern3    <- date_bag
    ## En caso de que las fechas tengan hora, obtenemos la primera columna exclusivamente...??????
    col.match   <- ldply(str_split(col," "),function(t)t<- t[[1]][1])
    ## Calculamos el número de observaciones que cumplen con el patrón 1
    true_match1 <- na.omit(str_length(str_match(col,pattern1)[,1]) == str_length(col.match[,1]))
    ## Calculamos el número de observaciones que cumplen con el patrón 2
    true_match2 <- na.omit(str_length(str_match(col,pattern2)[,1]) == str_length(col.match[,1]))
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
    apply(df, 2,function(t) t <- prob.date(t)) > thresh
}
###################################################
##---------------------------------
## Pruebas
##---------------------------------
###################################################
test <- ident.date(data)
