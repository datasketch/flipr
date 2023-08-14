
# library(tidyverse)
# library(ltgeo)
# library(dsopts)
# library(vctrs)
# library(hgchmagic)
#
# # x0 <- read_csv("persiodistas-asesinados.csv")
#
# x0 <- read_csv("persiodistas-asesinados.csv",show_col_types = FALSE) |>
#   mutate(year = lubridate::year(fecha_agresion))
# x0 <- x0 |>
#   filter(year >= 1990)
#
#
# # Map
# x <- x0 |>
#   select(departamento) |>
#   summarise(total = n(), .by = "departamento")
#
# opts <- list(
#   title = "Periodistas asesinados",
#   border_color = "#3a3a3a",
#   border_width = 1,
#   na_color = "#f0f0f0",
#   background_color = "#ffffff",
#   # na_label = "Sin&nbsp;Información"
#   na_label = "N/A",
#   tooltip_template = "<b>{total} asesinatos</b><br><i>{departamento}</i>"
# )
#
# ltgeo::lt_choropleth(x, map_name = "col_departments", var = "total",
#                      opts = opts)
#
#
# # Bar
#
# x <- x0 |>
#   select(year) |>
#   summarise(total = n(), .by = "year") |>
#   complete(year = seq(min(x0$year), max(x0$year))) |>
#   mutate(year = as.character(year))
#
# hgch_bar_CatNum(x)
#














