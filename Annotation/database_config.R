# File for the server specific database configuration:
# Dependend on the machine the Shiny App is run, a different configuration file is sourced.
# The configuration file contains names of databases, in which the data to be explored is stored.

# source(paste0(Sys.info()["nodename"], ".R"))
dmzDatabases <- c("database")