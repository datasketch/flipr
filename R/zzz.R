
# .onLoad <- function(libname, pkgname){
#   con <- flip_create_connection()
#   package_env <- parent.env(environment())
#   dbcooper::dbc_init(con, "flip", env = package_env)
#
# }
#
# .onUnload <- function(libpath){
#   dbcooper::dbc_clear_connections()
#
# }

