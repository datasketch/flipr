

api_data <- function(url){
  #https://cms.flip.datasketch.co/api/periodistas-asesinados
  respose <- GET( url )
  result <- fromJSON(content(respose, as = 'text'))
  result$fecha_agresion <- lubridate::ymd(result$fecha_agresion)
  result$anio_mes_agresion <- format(result$fecha_agresion, "%Y-%m")
  result$departamento[is.na(result$departamento)] <- "Sin información"
  result$genero[is.na(result$genero)] <- "Sin información"
  result$presunto_autor[is.na(result$presunto_autor)] <- "N/A"
  result$alerta_genero[is.na(result$alerta_genero)] <- "N/A"
  result$sucedio_en_internet[is.na(result$sucedio_en_internet)] <- "N/A"
  result$cargo[is.na(result$cargo)] <- "N/A"
  result
}

