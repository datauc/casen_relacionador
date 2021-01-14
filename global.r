library(shiny)
library(dplyr)
library(ggplot2)
library(shinyWidgets)

casen_datos <- readRDS("casen_datos.rds")

#variables <- names(casen_datos %>% select(-comuna, -tipo))

variables <- c("Años de escolaridad" = "esc",
"Edad" = "edad",
"Ingreso total del hogar corregido" = "ytotcorh",
"Ingreso total corregido" = "ytotcor",
"Ingreso ocupación principal" = "yoprcor",
"Ingreso total per cápita del hogar corregido" = "ypc",
"Ingreso del trabajo" = "ytrabajocor",
"Ingreso del trabajo del hogar" = "ytrabajocorh",
"Ingreso autónomo per cápita " = "ypchautcor",
"Jubilación o pensión" = "y26_2c",
"Numero de personas en el hogar" = "numper",
"Número de hijos vivos" = "s4")
