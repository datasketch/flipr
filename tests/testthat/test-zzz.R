test_that("db", {


  # La tabla flip_casos y flip_contacto tienen los datos "en crudo".

  # La tabla agresiones es el cruce entre las tablas que menciono al inicio,
  # pero solo con casos con estado igual a Cerrado.

  # La tabla periodistas_asesinados es igual a la de agresiones pero solo con
  # los casos donde la columna tipo_agresion es igual a Asesinato.

  ##
  tables <- flip_list()
  tables

  # [1] "public.flip_contacto"               "public.flip_casos"
  # [3] "public.agresiones"                  "public.agresiones_por_departamento"
  # [5] "public.periodistas_asesinados"




})
