shinyServer(function(input, output, session) {

    output$selector_y <- renderText({ input$selector_y })
    output$selector_x <- renderText({ input$selector_x })
    
    updateSelectizeInput(session,
                         inputId = "selector_y",
                         #choices = variables,
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
                      selected = c("La Florida", "Puente Alto", "La Cisterna", "Cerrillos", "Ñuñoa", "Vitacura", "Providencia")
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
            tidyr::pivot_wider(names_from = tipo, values_from = 2:13) %>%
            select(comuna, 
                   paste(variable_x, tipo_elegido_x, sep="_"), 
                   paste(variable_y, tipo_elegido_y, sep="_"),
                   all_of(variable_col),
                   paste(variable_size, tipo_elegido_size, sep="_")) %>%
            #select(comuna, ends_with(tipo_elegido)) %>%
            rename_all(list(~ stringr::str_remove(., "_promedio"))) %>%
            rename_all(list(~ stringr::str_remove(., "_mediana"))) %>%
            #filter(tipo == tipo_elegido) %>%
            filter(comuna %in% comunas_elegidas) %>%
            ggplot(mapping = aes_mapping) +
            geom_point(col= "white") +
            geom_point(alpha=0.5) +
            ggrepel::geom_text_repel(aes(label = comuna), 
                                     size = 4, point.padding = 1,
                                     min.segment.length = 2,
                                     show.legend = FALSE) +
            scale_size(range = c(6, 20)) +
            coord_cartesian(clip = "off") +
            fishualize::scale_color_fish(option = "Bodianus_pulchellus", discrete = TRUE) +
            theme_light(base_size = 18) +
            theme(legend.position = "right") +
            theme(axis.line = element_line(size = 1, color = "gray40", lineend = "round"),
                  axis.ticks = element_blank(),
                  panel.grid.major = element_line(size=0.5, color="gray80"),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  axis.text = element_text(color= "gray60", size=11),
                  legend.text = element_text(color= "gray60", size=12),
                  legend.title = element_text(color= "gray60", face = "bold", size=13),
                  axis.title = element_text(color= "gray60", face = "bold", size=13)) +
            guides(size = guide_legend(override.aes = list(color="gray60")),
                   col = guide_legend(override.aes = list(size=4)))
        
        return(p)
        
    })
    
    
    # output$alerta_repetidas <- renderText({
    #     if(input$selector_x == input$selector_y) {
    #         alerta <- "Las variables seleccionadas no pueden repetirse"
    #     } else if (input$selector_x == input$selector_size) {
    #         alerta <- "Las variables seleccionadas no pueden repetirse"
    #     } else if (input$selector_y == input$selector_size) {
    #         alerta <- "Las variables seleccionadas no pueden repetirse"
    #     }  else {
    #         alerta <- ""
    #     }
    #     return(alerta)
    # })
    
    
}) #fin