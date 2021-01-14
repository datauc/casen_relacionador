library(dplyr)
library(tidyr)

#importar datos
casen <- readstata13::read.dta13("~/Casen/Casen 2017.dta") %>% as_tibble()

#filtrar sólo la RM
casen_rm <- casen %>%
  filter(region == "Región Metropolitana de Santiago")

#seleccionar variables
casen_1 <- casen_rm %>% 
  select(comuna,
         expc,                    #factor de expansión comunal
         sexo,                    #género
         esc,                     #años de escolaridad
         edad,                    #edad
         ytotcorh,                #Ingreso total del hogar corregido
         ytotcor,                 #Ingreso total corregido
         yoprcor,                 #Ingreso ocupación principal
         ypc,                     #Ingreso total per cápita del hogar corregido
         ytrabajocor,             #ingreso del trabajo
         ytrabajocorh,            #ingreso del trabajo del hogar
         ypchautcor,              #ingreso autónomo per cápita 
         y26_2c,                  #jubilación o pensión
         numper,                  #numero de personas en el hogar
         s4,                      #hijos vivos
         pco1,                    #jefe de hogar
         activ,                   #actividad
         hacinamiento,            #hacinamiento
         pobreza,                 #pobreza
         pobreza_multi_5d,        #pobreza multidimensional
         r1a,                     #nacionalidad
         r3,                      #pertenencia a pueblos originarios
         v12,                     #metros cuadrados de la casa
         indmat)                  #índice de materialidad de la vivienda

#aplicar factor de expansión
casen_2 <- tidyr::uncount(casen_1, weights = expc)

#calcular medidas de tendencia central
casen_promedios <- casen_2 %>%
  group_by(comuna) %>%
  summarize(across(2:13, ~ mean(.x, na.rm = TRUE))) %>%
  mutate(tipo = "promedio")

casen_medianas <- casen_2 %>%
  group_by(comuna) %>%
  summarize(across(2:13, ~ median(.x, na.rm = TRUE))) %>%
  mutate(tipo = "mediana")

#unir datos
casen_datos <- bind_rows(casen_promedios, casen_medianas) %>%
  arrange(comuna, tipo)

glimpse(casen_datos)

#limpiar
remove(casen, casen_rm, casen_1, casen_2)

#exportar datos
saveRDS(casen_datos, file = "casen_datos.rds")


#—----
variable_y = "ytotcorh"
variable_x = "numper"
variable_col = "comuna"
variable_size = "s4"
comunas_elegidas <- c("La Florida", "Puente Alto", "Vitacura")


tipo_elegido_x = "promedio"
tipo_elegido_y = "promedio"
tipo_elegido_size = "mediana"

aes_mapping <- aes_string(
  x = variable_x,
  y = variable_y,
  #group = datos_cep()$variable,
  col = variable_col,
  size = variable_size
)

min(casen_datos$s4)

casen_datos %>%
  tidyr::pivot_wider(names_from = tipo, values_from = 2:12) %>%
  select(comuna, 
         paste(variable_x, tipo_elegido_x, sep="_"), 
         paste(variable_y, tipo_elegido_y, sep="_"),
         variable_col,
         paste(variable_size, tipo_elegido_size, sep="_")) %>%
  #select(comuna, ends_with(tipo_elegido)) %>%
  rename_at(vars(ends_with("promedio")), list(~ stringr::str_remove(., "_promedio"))) %>%
    rename_at(vars(ends_with("mediana")), list(~ stringr::str_remove(., "_mediana"))) %>%
  #filter(tipo == tipo_elegido) %>%
  filter(comuna %in% comunas_elegidas) %>%
  ggplot(mapping = aes_mapping) +
  geom_point(col= "white") +
  geom_point(alpha=0.5) +
  ggrepel::geom_text_repel(aes(label = comuna), size= 4) +
  scale_size(range = c(3, 9)) +
  fishualize::scale_color_fish(option = "Bodianus_pulchellus", discrete = TRUE) +
  theme_light() +
  theme(legend.position = "right") +
  theme(axis.line = element_line(size=1, color = "gray40", lineend = "round"),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(size=0.5, color="gray80"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(color= "gray60", size=7),
        legend.text = element_text(color= "gray60", size=8),
        legend.title = element_text(color= "gray60", face = "bold", size=9),
        axis.title = element_text(color= "gray60", face = "bold", size=9)) +
  guides(size = guide_legend(override.aes = list(color="gray60")),
         col = guide_legend(override.aes = list(size=4)))
  
