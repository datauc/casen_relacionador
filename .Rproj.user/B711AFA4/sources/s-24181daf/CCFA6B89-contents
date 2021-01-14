library(shiny)
library(dplyr)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    readRDS("casen_datos.rds")
    
    output$selector_y <- renderText({ input$selector_y })
    output$selector_x <- renderText({ input$selector_x })
    
    variables <- names(casen_datos %>% select(-comuna, -tipo))
    
    updateSelectizeInput(session,
                         inputId = "selector_y",
                         choices = variables,
                         selected = sample(variables, 1)
    )
    
    updateSelectizeInput(session,
                         inputId = "selector_x",
                         choices = variables,
                         selected = sample(variables, 1)
    )
    
    updateSelectizeInput(session,
                         inputId = "selector_size",
                         choices = variables,
                         selected = sample(variables, 1)
    )
    
    updatePickerInput(session,
                         inputId = "selector_comunas",
                         choices = casen_datos %>% select(comuna) %>% distinct() %>% pull() %>% as.character(),
                      selected = c("La Florida", "Puente Alto", "Vitacura", "Providencia")
    )

    
    output$grafico <- renderPlot({
        
        variable_y = input$selector_y #"ytotcorh"
        variable_x = input$selector_x #"numper"
        variable_col = "comuna"
        variable_size = input$selector_size #"s4"
        comunas_elegidas <- input$selector_comunas#c("La Florida", "Puente Alto", "Vitacura")
        
        tipo_elegido_x = input$tipo_elegido_x #"promedio"
        tipo_elegido_y = input$tipo_elegido_y #"promedio"
        tipo_elegido_size = input$tipo_elegido_size #"mediana"
        
        aes_mapping <- aes_string(
            x = variable_x,
            y = variable_y,
            col = variable_col,
            size = variable_size
        )
        
        p <- casen_datos %>%
            pivot_wider(names_from = tipo, values_from = 2:12) %>%
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
            scale_size(range = c(5, 15)) +
            fishualize::scale_color_fish(option = "Bodianus_pulchellus", discrete = TRUE) +
            theme_light(base_size = 18) +
            theme(legend.position = "right") +
            theme(axis.line = element_line(size=1, color = "gray40", lineend = "round"),
                  axis.ticks = element_blank(),
                  panel.grid.major = element_line(size=0.5, color="gray80"),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  axis.text = element_text(color= "gray60", size=10),
                  legend.text = element_text(color= "gray60", size=11),
                  legend.title = element_text(color= "gray60", face = "bold", size=12),
                  axis.title = element_text(color= "gray60", face = "bold", size=12)) +
            guides(size = guide_legend(override.aes = list(color="gray60")),
                   col = guide_legend(override.aes = list(size=4)))
        
        return(p)
        
    })
    
    
}) #fin
