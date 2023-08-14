

api_data <- function(url){
  #https://cms.flip.datasketch.co/api/periodistas-asesinados
  respose <- GET( url )
  result <- fromJSON(content(respose, as = 'text'))
  result$fecha_agresion <- lubridate::ymd(result$fecha_agresion)
  result
}

