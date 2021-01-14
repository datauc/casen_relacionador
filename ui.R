shinyUI(fluidPage(
    
    # Application title
    titlePanel("Relacionador Casen"),
    
    fluidRow(
        column(12,
               column(8,
               selectizeInput("selector_y",
                              label = "seleccione la variable para el eje y",
                              width = "100%",
                              choices = NULL
                              #choices = c("primera variable" = "1",
                              #               "segunda variable" = "2")
               ),
               ),
               column(4,
               selectizeInput("tipo_elegido_y",
                              label = "seleccione la medida del dato del eje y",
                              width = "100%",
                              choices = c("Promedios" = "promedio",
                                          "Medianas" = "mediana")
               )
               )
        ),
        
        column(12,
               column(8,
               selectizeInput("selector_x",
                              label = "seleccione la variable para el eje x",
                              width = "100%",
                              choices = NULL
                              #choices = c("primera variable" = "1",
                              #               "segunda variable" = "2")
               ),
               ),
               column(4,
               selectizeInput("tipo_elegido_x",
                              label = "seleccione la medida del dato del eje x",
                              width = "100%",
                              choices = c("Promedios" = "promedio",
                                          "Medianas" = "mediana")
               )
               )
               
        ),
        
        column(12,
               column(8,
               selectizeInput("selector_size",
                              label = "seleccione la variable para el tamaño",
                              width = "100%",
                              choices = NULL
                              #choices = c("primera variable" = "1",
                              #               "segunda variable" = "2")
               ),
               ),
               column(4,
               
               selectizeInput("tipo_elegido_size",
                              label = "seleccione la medida del tamaño",
                              width = "100%",
                              choices = c("Promedios" = "promedio",
                                          "Medianas" = "mediana")
               )
               )
               
        ),
        
        column(12,
               column(12,
               pickerInput("selector_comunas",
                           label = "seleccione las comunas que desea graficar",
                           width = "100%",
                           multiple = TRUE,
                           choices = NULL
               )
               )
               
        )
    ),
    
    # textOutput("selector_y"),
    # textOutput("selector_x"),
    
    fluidRow(    
        column(12,
               br(),
               #textOutput("alerta_repetidas"),br(),
               plotOutput("grafico", height = "640px")
        )
    )
    
))
