## capital <- function(x) {
##   s <- strsplit(x, " ")[[1]]
##   paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
##         sep="", collapse=" ")
## }

csv2tex <- function(bd){

  tipoComunicacion <- as.character(bd[11])
  
  autores <- as.character(bd[8])
  ## Algunos registros usan , para separar autores
  autores <- gsub(', ', '%%%', autores)
  autores <- gsub(',', '%%%', autores)
  autores <- gsub('\\*', '', autores)
  autorList <- strsplit(autores, '%%%')[[1]]

  nombreCompleto <- strsplit(autorList, ' ')
  nombrePila <- sapply(nombreCompleto, function(x)x[1])
  apellidos <- sapply(nombreCompleto, function(x)paste(x[-1], collapse=' '))

  inst <- as.character(bd[9])
  instList <- strsplit(inst, '%%%')[[1]]

  titulo <- as.character(bd[7])
  
  nombreFile <- gsub('[[:space:][:punct:]]', '_', titulo)
  ##elimino caracteres extraños en nombre de ficheros
  nombreFile <- iconv(nombreFile, 'UTF-8', 'ASCII//TRANSLIT')
  nombreFile <- tolower(nombreFile)
  if (tipoComunicacion == 'Taller'){
    nombreFile <- paste('talleres/', nombreFile, sep='')
  } else {
    nombreFile <- paste('resumenes/', nombreFile, sep='')
    }
  nombreTex <- paste(nombreFile, '.tex', sep='')
  nombreBib <- paste(nombreFile, '.bib', sep='')

  resumen <- as.character(bd[10])
  resumen <- gsub('%%%', '\n', resumen)

  biblio <- as.character(bd[12])
  biblio <- gsub('%%%', '\n', biblio)
  

  titulo <- paste('\\chapter{', titulo, '}', sep='')
  autor <- paste('\\chapterprecis{',
                 paste(autorList, collapse=', '),
                 '\\\\',
                 paste(instList, collapse='\\\\'),
                 '}', sep='')
  autorIdx <- paste('\\index{', paste(apellidos, nombrePila, sep=', '), '}',
                    sep='', collapse='\n')
  instIdx <- paste('\\index[inst]{',instList,'}',
                   sep='', collapse='\n')
  
  bibliografia <- paste('%\\bibliography{', nombreFile, '}', sep='')
  bibstyle <- '%\\bibliographystyle{plain}'

  txt <- paste(titulo,
               autor, autorIdx,
               instIdx,
               resumen,
               bibstyle,
               bibliografia,
               sep='\n\n', collapse='\n\n')

  writeLines(txt, nombreTex)
  writeLines(biblio, nombreBib)
}

listaPonencias <- function(bd){
  talleres <- character()
  resumenes <- character()
  for (i in 1:nrow(bd)){
    tipoComunicacion <- as.character(bd[i, 11])
    titulo <- as.character(bd[i, 7])
    nombreFile <- gsub('[[:space:][:punct:]]', '_', titulo)
    ##elimino caracteres extraños en nombre de ficheros
    nombreFile <- iconv(nombreFile, 'UTF-8', 'ASCII//TRANSLIT')
    nombreFile <- tolower(nombreFile)
    if (tipoComunicacion == 'Taller'){
      talleres <- c(talleres, paste('\\include{talleres/', nombreFile, '}', sep=''))
    } else {
      resumenes <- c(resumenes, paste('\\include{resumenes/', nombreFile, '}', sep=''))
    }
  }
  writeLines(talleres, 'talleres.tex')
  writeLines(resumenes, 'resumenes.tex')
}

listaPonenciasPorSesiones <- function(bd){
  talleres <- character()
  resumenes <- character()
  horario <- as.character(bd[, 6])
  horaTall <- character()
  horaResu <- character()
  for (i in 1:nrow(bd)){
    tipoComunicacion <- as.character(bd[i, 11])
    titulo <- as.character(bd[i, 7])
    nombreFile <- gsub('[[:space:][:punct:]]', '_', titulo)
    ##elimino caracteres extraños en nombre de ficheros
    nombreFile <- iconv(nombreFile, 'UTF-8', 'ASCII//TRANSLIT')
    nombreFile <- tolower(nombreFile)
    if (tipoComunicacion == 'Taller'){
      horaTall <- c(horaTall,horario[i])
      talleres <- c(talleres, paste('\\include{talleres/', nombreFile, '}', sep=''))
    } else {
      horaResu <- c(horaResu,horario[i])
      resumenes <- c(resumenes, paste('\\include{resumenes/', nombreFile, '}', sep=''))
    }
  }
  writeLines(talleres, 'talleres.tex')
  writeLines(resumenes, 'resumenes.tex')
  # resumenes por sesiones
  for(h in unique(horario)) {
    writeLines(talleres[horaTall==h], paste('talleres',h,'.tex',sep=""))
    writeLines(resumenes[horaResu==h], paste('resumenes',h,'.tex',sep=""))
  }
}
                    

setwd('~/R/VJornadas/')

bd <- read.csv('formulario-bd_9.csv')

# apply(bd, 1, csv2tex)
apply(bd[,-1], 1, csv2tex)  # quito 1ra. col. para que coincidan los índices en csv2tex

# listaPonencias(bd)
listaPonencias(bd[-1])  # quito 1ra. col. para que coincidan los índices en csv2tex

listaPonenciasPorSesiones(bd[-1])
