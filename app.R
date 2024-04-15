# Documentación del script Shiny

# Cargar las librerías necesarias
library(shiny)          # Para crear la aplicación Shiny
library(bslib)          # Para personalizar el estilo de la aplicación Shiny
library(readxl)         # Para leer archivos de Excel
library(dplyr)          # Para manipulación de datos
library(scales)         # Para formatear los datos numéricos
library(DT)             # Para tablas interactivas
library(bsicons)        # Para iconos Bootstrap
library(ggplot2)        # Para crear gráficos
library(shinythemes)    # Para utilizar temas en la aplicación Shiny
library(forcats)        # Para manipular factores

# Establecer el directorio de trabajo
#setwd("D:/Python/Shiny")

# Definir la función del servidor
server <- function(input, output) {
  
  # Leer datos de siniestralidad desde un archivo Excel
  siniestralidad_dataframe <- read_excel("datos.xlsx", sheet = "Siniestralidad") %>%
    # Seleccionar las columnas necesarias y convertir Año a numérico
    select(Año, Mes, Regimen_Financiamento, `Ingresos en Salud`, `Gasto en Salud`) %>%
    mutate(Año = as.numeric(Año))
  
  # Leer datos de prestaciones montos desde un archivo Excel
  prestaciones_montos_dataframe <- read_xlsx("datos.xlsx", 
                                             sheet = "Prestaciones_Montos") %>%
    # Seleccionar las columnas necesarias y convertir "Año de Cobertura" a numérico
    select(`Año de Cobertura`, `Grupo Descripción`, Regimen_Financiamento, `Montos Pagados`) %>%
    mutate("Año de Cobertura" = as.numeric(`Año de Cobertura`))
  
  # Leer datos de prestaciones servicios desde un archivo Excel
  prestaciones_servicio_dataframe <- read_xlsx("datos.xlsx", 
                                               sheet = "Prestaciones_Servicios") %>%
    # Seleccionar las columnas necesarias y convertir "Año de Cobertura" a numérico
    select(`Año de Cobertura`, `Grupo Descripción`, Regimen_Financiamento, `Servicios Otorgados`) %>%
    mutate("Año de Cobertura" = as.numeric(`Año de Cobertura`))
  
  # Renderizar la tabla interactiva en la interfaz Shiny
  output$mytable1 <- renderDataTable({
    datatable(siniestralidad_dataframe%>%
                # Filtrar los datos según los años y regímenes seleccionados por el usuario
                filter(between(Año,min(input$ano),max(input$ano)) &
                         Regimen_Financiamento %in% input$regimen)%>%
                # Agrupar por año y sumar los ingresos y gastos en salud
                group_by(Año)%>%
                summarise("Ingresos en Salud" = sum(`Ingresos en Salud`),
                          "Gasto en Salud" = sum(`Gasto en Salud`))%>%
                # Calcular la siniestralidad como porcentaje
                mutate(Siniestralidad = percent(`Gasto en Salud`/`Ingresos en Salud`,accuracy = 0.01))%>%
                # Formatear los valores numéricos con comas y precisión de 2 decimales
                mutate("Ingresos en Salud" = comma(`Ingresos en Salud`,accuracy = 0.01),
                       "Gasto en Salud" = comma(`Gasto en Salud`,accuracy = 0.01)))
  })
  
  # Renderizar la tabla interactiva mytable2 en la interfaz Shiny
  output$mytable2 <- renderDataTable({
    datatable(prestaciones_montos_dataframe%>%
                # Filtrar los datos según los años y regímenes seleccionados por el usuario
                filter(between(`Año de Cobertura`,min(input$ano_pm),max(input$ano_pm)) &
                         Regimen_Financiamento %in% input$regimen_pm)%>%
                # Agrupar por descripción del grupo y sumar los montos pagados
                group_by(`Grupo Descripción`)%>%
                summarise("Montos Pagados" = sum(`Montos Pagados`))%>%
                # Ordenar los datos por los montos pagados de manera descendente
                arrange(desc(`Montos Pagados`))%>%
                # Calcular la participación como porcentaje de los montos pagados
                mutate(Participación = percent(`Montos Pagados`/sum(`Montos Pagados`),accuracy = 0.01),
                       "Montos Pagados" = comma(`Montos Pagados`,accuracy=0.01)))
  })
  
  # Renderizar la tabla interactiva mytable3 en la interfaz Shiny
  output$mytable3 <- renderDataTable({
    datatable(prestaciones_servicio_dataframe%>%
                # Filtrar los datos según los años y regímenes seleccionados por el usuario
                filter(between(`Año de Cobertura`,min(input$ano_ps),max(input$ano_ps)) &
                         Regimen_Financiamento %in% input$regimen_ps)%>%
                # Agrupar por descripción del grupo y sumar los servicios otorgados
                group_by(`Grupo Descripción`)%>%
                # Ordenar los datos por los servicios otorgados de manera descendente
                summarise("Servicios Otorgados" = sum(`Servicios Otorgados`))%>%
                arrange(desc(`Servicios Otorgados`))%>%
                # Calcular la participación como porcentaje de los servicios otorgados
                mutate(Participación = percent(`Servicios Otorgados`/sum(`Servicios Otorgados`),accuracy = 0.01),
                       "Servicios Otorgados" = comma(`Servicios Otorgados`)))
  })
  
  # Renderizar el texto de siniestralidad en la interfaz Shiny
  output$siniestralidad <- renderText({
    # Calcular la siniestralidad general
    general_sinestralidad<-siniestralidad_dataframe%>%
      # Filtrar los datos según los años y regímenes seleccionados por el usuario
      filter(between(Año,min(input$ano),max(input$ano)) &
               Regimen_Financiamento %in% input$regimen)%>%
      # Sumar los ingresos y gastos en salud
      summarise("Ingresos en Salud" = sum(`Ingresos en Salud`),
                "Gasto en Salud" = sum(`Gasto en Salud`))%>%
      # Calcular la siniestralidad como porcentaje
      mutate(Siniestralidad = percent(`Gasto en Salud`/`Ingresos en Salud`,accuracy = 0.01))
  
    # Devolver el valor de siniestralidad
    general_sinestralidad$Siniestralidad
  })
  
  # Renderizar el texto de incoming en la interfaz Shiny
  output$incoming <- renderText({
    # Calcular el Ingresos en Salud
    general_sinestralidad<-siniestralidad_dataframe%>%
      filter(between(Año,min(input$ano),max(input$ano)) &
               Regimen_Financiamento %in% input$regimen)%>%
      # Sumar los Ingresos en Salud
      summarise("Ingresos en Salud" = sum(`Ingresos en Salud`))%>%
      mutate("Ingresos en Salud" = comma(`Ingresos en Salud`,
                                         accuracy = 0.01,
                                         suffix = "M",
                                         scale = 1e-6,
                                         vjust = 0))
    
    # Devolver el valor de Ingresos en Salud
    general_sinestralidad$`Ingresos en Salud`
  })
  
  # Renderizar el texto de paid en la interfaz Shiny
  output$paid <- renderText({
    # Calcular el Gasto en Salud
    general_sinestralidad<-siniestralidad_dataframe%>%
      filter(between(Año,min(input$ano),max(input$ano)) &
               Regimen_Financiamento %in% input$regimen)%>%
      # Sumar los Gasto en Salud
      summarise("Gasto en Salud" = sum(`Gasto en Salud`))%>%
      mutate("Gasto en Salud" = comma(`Gasto en Salud`,
                                      accuracy = 0.01,
                                      suffix = "M",
                                      scale = 1e-6,
                                      vjust = 0))

    # Devolver el valor de Gasto en Salud
    general_sinestralidad$`Gasto en Salud`
  })
  
  # Renderizar el texto de total_paid en la interfaz Shiny
  output$total_paid <- renderText({
    # Calcular el Montos Pagados
    total_paid<-prestaciones_montos_dataframe%>%
      filter(between(`Año de Cobertura`,min(input$ano_pm),max(input$ano_pm)) &
               Regimen_Financiamento %in% input$regimen_pm)%>%
      # Sumar los Montos Pagados
      summarise("Montos Pagados" = comma(sum(`Montos Pagados`),
                                              accuracy = 0.01,
                                              suffix = "M",
                                              scale = 1e-6,
                                              vjust = 0))
    
    # Devolver el valor de Montos Pagados
    total_paid$`Montos Pagados`
  })
  
  # Renderizar el texto de total_serv en la interfaz Shiny
  output$total_serv <- renderText({
    # Calcular el Servicios Otorgados
    total_serv<-prestaciones_servicio_dataframe%>%
      filter(between(`Año de Cobertura`,min(input$ano_ps),max(input$ano_ps)) &
               Regimen_Financiamento %in% input$regimen_ps)%>%
      # Sumar los Servicios Otorgados
      summarise("Servicios Otorgados" = comma(sum(`Servicios Otorgados`),
                                              accuracy = 0.01,
                                              suffix = "M",
                                              scale = 1e-6,
                                              vjust = 0))
    
    # Devolver la cantidad de Servicios Otorgados
    total_serv$`Servicios Otorgados`
  })
  
  # Renderizar el gráfico de ingresos en salud en la interfaz Shiny
  output$income_plot <- renderPlot({
    siniestralidad_dataframe %>%
      # Filtrar los datos según los años y regímenes seleccionados por el usuario
      filter(between(Año, min(input$ano), max(input$ano)) &
               Regimen_Financiamento %in% input$regimen) %>%
      # Agrupar por año y régimen financiero y sumar los ingresos y gastos en salud
      group_by(Año, Regimen_Financiamento) %>%
      summarise("Ingresos en Salud" = sum(`Ingresos en Salud`),
                "Gasto en Salud" = sum(`Gasto en Salud`)) %>%
      # Calcular la siniestralidad como el gasto en salud dividido por los ingresos en salud
      mutate(Siniestralidad = `Gasto en Salud` / `Ingresos en Salud`) %>%
      ungroup(Año, Regimen_Financiamento) %>%
      # Crear el gráfico de dispersión y línea con etiquetas de ingresos en salud
      ggplot(aes(x = Año, y = `Ingresos en Salud`, color = Regimen_Financiamento,
                 lty = Regimen_Financiamento, group = Regimen_Financiamento)) +
      geom_point() +
      geom_line() +
      geom_label(aes(label = comma(`Ingresos en Salud`,
                                   accuracy = 0.01,
                                   scale = 1e-6,
                                   vjust = 0,
                                   suffix = "M")),
      ) +
      labs(x = "Año", y = "Total de Ingreso del Seguro Familiar de Salud") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  })
  
  # Renderizar el gráfico de gastos en salud en la interfaz Shiny
  output$paid_plot <- renderPlot({
    siniestralidad_dataframe %>%
      # Filtrar los datos según los años y regímenes seleccionados por el usuario
      filter(between(Año, min(input$ano), max(input$ano)) &
               Regimen_Financiamento %in% input$regimen) %>%
      # Agrupar por año y régimen financiero y sumar los ingresos y gastos en salud
      group_by(Año, Regimen_Financiamento) %>%
      summarise("Ingresos en Salud" = sum(`Ingresos en Salud`),
                "Gasto en Salud" = sum(`Gasto en Salud`)) %>%
      # Calcular la siniestralidad como el gasto en salud dividido por los ingresos en salud
      mutate(Siniestralidad = `Gasto en Salud` / `Ingresos en Salud`) %>%
      ungroup(Año, Regimen_Financiamento) %>%
      # Crear el gráfico de dispersión y línea con etiquetas de gastos en salud
      ggplot(aes(x = Año, y = `Gasto en Salud`, color = Regimen_Financiamento,
                 lty = Regimen_Financiamento, group = Regimen_Financiamento)) +
      geom_point() +
      geom_line() +
      geom_label(aes(label = comma(`Gasto en Salud`,
                                   accuracy = 0.01,
                                   scale = 1e-6,
                                   vjust = 0,
                                   suffix = "M")),
      ) +
      labs(x = "Año", y = "Total de Gastos del Seguro Familiar de Salud") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  })
  
  # Renderizar el gráfico de siniestralidad en la interfaz Shiny
  output$siniestralidad_plot <- renderPlot({
    siniestralidad_dataframe %>%
      # Filtrar los datos según los años y regímenes seleccionados por el usuario
      filter(between(Año, min(input$ano), max(input$ano)) &
               Regimen_Financiamento %in% input$regimen) %>%
      # Agrupar por año y régimen financiero y sumar los ingresos y gastos en salud
      group_by(Año, Regimen_Financiamento) %>%
      summarise("Ingresos en Salud" = sum(`Ingresos en Salud`),
                "Gasto en Salud" = sum(`Gasto en Salud`)) %>%
      # Calcular la siniestralidad como el gasto en salud dividido por los ingresos en salud
      mutate(Siniestralidad = `Gasto en Salud` / `Ingresos en Salud`) %>%
      ungroup(Año, Regimen_Financiamento) %>%
      # Crear el gráfico de dispersión y línea con etiquetas de siniestralidad
      ggplot(aes(x = Año, y = Siniestralidad, color = Regimen_Financiamento,
                 lty = Regimen_Financiamento, group = Regimen_Financiamento)) +
      geom_point() +
      geom_line() +
      geom_label(aes(label = percent(Siniestralidad,
                                     accuracy = 0.01,
                                     vjust = 0))
      ) +
      labs(x = "Año", y = "Siniestralidad (%) del Seguro Familiar de Salud") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  })
  
  # Renderizar el gráfico de Servicios Otorgados en la interfaz Shiny
  output$service_plot <- renderPlot({
    prestaciones_servicio_dataframe%>%
      # Filtrar los datos según los años y regímenes seleccionados por el usuario
      filter(between(`Año de Cobertura`,min(input$ano_ps),max(input$ano_ps)) &
               Regimen_Financiamento %in% input$regimen_ps)%>%
      # Agrupar por año y régimen financiero y sumar los Servicios Otorgados
      group_by(Regimen_Financiamento,`Año de Cobertura`)%>%
      summarise("Servicios Otorgados" = sum(`Servicios Otorgados`))%>%
      ungroup()%>%
      # Calcular reordenar la variable Regimen_Financiamiento de manera decendente de los Servicios Otorgados
      mutate(Regimen_Financiamento = fct_reorder(Regimen_Financiamento,desc(`Servicios Otorgados`)))%>%
      # Crear el gráfico de barras con etiquetas de los Servicios Otorgados
      ggplot(aes(x=`Año de Cobertura`,y = `Servicios Otorgados`,fill = Regimen_Financiamento,
                 order = `Servicios Otorgados`))+
      geom_bar(stat = "identity")+
      scale_fill_brewer()+
      geom_label(aes(label=comma(`Servicios Otorgados`,
                                 vjust = 0))
      )+
      labs(x = "Año", y = "Servicios Otorgados") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  })
  
  # Renderizar el gráfico de Monto Pagado en la interfaz Shiny
  output$monto_plot <- renderPlot({
    prestaciones_montos_dataframe%>%
      # Filtrar los datos según los años y regímenes seleccionados por el usuario
      filter(between(`Año de Cobertura`,min(input$ano_pm),max(input$ano_pm)) &
               Regimen_Financiamento %in% input$regimen_pm)%>%
      # Agrupar por año y régimen financiero y sumar los Monto Pagado
      group_by(Regimen_Financiamento,`Año de Cobertura`)%>%
      summarise("Montos Pagados" = sum(`Montos Pagados`))%>%
      ungroup()%>%
      # Calcular reordenar la variable Regimen_Financiamiento de manera decendente de los Monto Pagado
      mutate(Regimen_Financiamento = fct_reorder(Regimen_Financiamento,desc(`Montos Pagados`)))%>%
      # Crear el gráfico de barras con etiquetas de los Monto Pagado
      ggplot(aes(x=`Año de Cobertura`,y = `Montos Pagados`,fill = Regimen_Financiamento,
                 order = `Montos Pagados`))+
      # Crear el gráfico de barras con etiquetas de los Monto Pagado
      geom_bar(stat = "identity")+
      scale_fill_brewer()+
      geom_label(aes(label=comma(`Montos Pagados`,
                                 accuracy = 0.01,
                                 suffix = "M",
                                 scale = 1e-6,
                                 vjust = 0))
      )+
      labs(x = "Año", y = "Montos Pagados") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  })
  
  # Renderizar la interfaz de usuario dinámica en la aplicación Shiny
  output$ui <- renderUI({
    # Crear un panel lateral con varias filas fluidas
    sidebarPanel(
      # Primera fila: Slider de año para filtrar datos por año
      fluidRow(column(width = 10,
                      sliderInput("ano", "Año:",
                                  min = min(siniestralidad_dataframe$Año),
                                  max = max(siniestralidad_dataframe$Año),
                                  value = c(max(siniestralidad_dataframe$Año) - 7, max(siniestralidad_dataframe$Año)),
                                  step = 1,
                                  sep = ""))),
      # Segunda fila: CheckboxGroupInput para seleccionar tipos de régimen de financiamiento
      fluidRow(column(width = 6,
                      checkboxGroupInput("regimen", "Tipos de Regimen de Financiamiento",
                                         choices = unique(siniestralidad_dataframe$Regimen_Financiamento),
                                         selected = unique(siniestralidad_dataframe$Regimen_Financiamento)))),
      # Tercera fila: ValueBox para mostrar la siniestralidad total
      fluidRow(column(width = 10,
                      value_box(title = tags$p("Siniestralidad Total", style = "font-size: 150%;"),
                                value = textOutput("siniestralidad"),
                                showcase = bs_icon("plus-slash-minus"),
                                theme = value_box_theme(fg = "#0047AB")))),
      # Cuarta fila: ValueBox para mostrar el ingreso total
      fluidRow(column(width = 10,
                      value_box(title = tags$p("Ingreso Total", style = "font-size: 150%;"),
                                value = textOutput("incoming"),
                                showcase = bs_icon("plus-lg"),
                                theme = value_box_theme(fg = "#0A914B")))),
      # Quinta fila: ValueBox para mostrar el gasto total
      fluidRow(column(width = 10,
                      value_box(title = tags$p("Gasto Total", style = "font-size: 150%;"),
                                value = textOutput("paid"),
                                showcase = bs_icon("dash-lg"),
                                theme = value_box_theme(fg = "#910A2B"))))
    )
  })
  
  # Renderizar la interfaz de usuario dinámica de Montos por Servicios en la aplicación Shiny
  output$ui_pm <- renderUI({
    # Crear un panel lateral con varias filas fluidas
    sidebarPanel(
      # Primera fila: Slider de año para filtrar datos por año
      fluidRow(column(width = 10,
                      sliderInput("ano_pm", "Año:",
                                  min = min(prestaciones_montos_dataframe$`Año de Cobertura`),
                                  max = max(prestaciones_montos_dataframe$`Año de Cobertura`),
                                  value = c(max(prestaciones_montos_dataframe$`Año de Cobertura`) - 7, 
                                            max(prestaciones_montos_dataframe$`Año de Cobertura`)),
                                  step = 1))),
      # Segunda fila: CheckboxGroupInput para seleccionar tipos de régimen de financiamiento
      fluidRow(column(width = 6,
                      checkboxGroupInput("regimen_pm", "Tipos de Regimen de Financiamiento",
                                         choices = unique(prestaciones_montos_dataframe$Regimen_Financiamento),
                                         selected = unique(prestaciones_montos_dataframe$Regimen_Financiamento)))),
      # Tercera fila: ValueBox para mostrar la Total Monto Pagado
      fluidRow(column(width = 10,
                      value_box(title = tags$p("Total Monto Pagado", style = "font-size: 150%;"),
                                value = textOutput("total_paid"),
                                showcase = bs_icon("cash"),
                                theme = value_box_theme(fg = "#0047AB"))))
    )
  })
  
  # Renderizar la interfaz de usuario dinámica de Servicios Otorgados en la aplicación Shiny
  output$ui_ps <- renderUI({
    # Crear un panel lateral con varias filas fluidas
    sidebarPanel(
      # Primera fila: Slider de año para filtrar datos por año
      fluidRow(column(width = 10,
                      sliderInput("ano_ps", "Año:",
                                  min = min(prestaciones_servicio_dataframe$`Año de Cobertura`),
                                  max = max(prestaciones_servicio_dataframe$`Año de Cobertura`),
                                  value = c(max(prestaciones_servicio_dataframe$`Año de Cobertura`) - 7, 
                                            max(prestaciones_servicio_dataframe$`Año de Cobertura`)),
                                  step = 1))),
      # Segunda fila: CheckboxGroupInput para seleccionar tipos de régimen de financiamiento
      fluidRow(column(width = 6,
                      checkboxGroupInput("regimen_ps", "Tipos de Regimen de Financiamiento",
                                         choices = unique(prestaciones_servicio_dataframe$Regimen_Financiamento),
                                         selected = unique(prestaciones_servicio_dataframe$Regimen_Financiamento)))),
      # Tercera fila: ValueBox para mostrar la Total Servicios Otorgados
      fluidRow(column(width = 10,
                      value_box(title = tags$p("Total Servicios Otorgados", style = "font-size: 150%;"),
                                value = textOutput("total_serv"),
                                showcase = bs_icon("123"),
                                theme = value_box_theme(fg = "#0047AB"))))
    )
  })
}

# Crear la aplicación Shiny
shinyApp(
  # Interfaz de usuario (UI)
  ui = fluidPage( theme = shinytheme("journal"),
    tabsetPanel(
      # Panel de pestañas
      tabPanel(
        " Siniestralidad",# Título de la pestaña
        fluidRow(
          # Fila fluida para la disposición de elementos
          uiOutput("ui"), # Renderizado dinámico de la interfaz de usuario
          mainPanel(h2("Siniestralidad del Seguro Familiar de Salud"), # Título principal
                    dataTableOutput("mytable1"), # Salida de tabla interactiva
                    tabsetPanel(
                      # Panel de pestañas secundario
                      tabPanel(
                        "Evolucion Anual de la Siniestralidad",# Título de la pestaña secundaria
                        h3("Evolucion Anual de la Siniestralidad por Regimen Financiamiento del Seguro Familiar de Salud"),# Título secundario
                        plotOutput("siniestralidad_plot") # Salida de gráfico
                      ),
                      tabPanel(
                        "Evolucion Anual del Ingreso",# Título de la pestaña secundaria
                        h3("Evolucion Anual del Ingreso por Regimen Financiamiento del Seguro Familiar de Salud"),# Título secundario
                        plotOutput("income_plot") # Salida de gráfico
                      ),
                      tabPanel(
                        "Evolucion Anual del Gasto",# Título de la pestaña secundaria
                        h3("Evolucion Anual del Gasto por Regimen Financiamiento del Seguro Familiar de Salud"),# Título secundario
                        plotOutput("paid_plot") # Salida de gráfico
                      )
                    ))
        )
      ),
      tabPanel(
        "Montos por Servicios Otorgados",# Título de la pestaña
        fluidRow(
          # Fila fluida para la disposición de elementos
          uiOutput("ui_pm"),# Renderizado dinámico de la interfaz de usuario
          mainPanel(h2("Montos Pagados por Servicios Otorgados del Seguro Familiar de Salud"),# Título principal
                    dataTableOutput("mytable2"),# Salida de tabla interactiva
                    h3("Evolucion Anual de los Montos Pagados por Servicios Otorgados por Regimen Financiamiento, Seguro Familiar de Salud"),# Título secundario
                    plotOutput("monto_plot"))# Salida de gráfico
        )
      ),
      tabPanel(
        "Servicios Otorgados",# Título de la pestaña
        fluidRow(
          # Fila fluida para la disposición de elementos
          uiOutput("ui_ps"),# Renderizado dinámico de la interfaz de usuario
          mainPanel(h2("Servicios Otorgados del Seguro Familiar de Salud"),# Título principal
                    dataTableOutput("mytable3"),# Salida de tabla interactiva
                    h3("Evolucion Anual de los Servicios Otorgados por Regimen Financiamiento, Seguro Familiar de Salud"),# Título secundario
                    plotOutput("service_plot"))# Salida de gráfico
        )
      )
    )
    ),
  # Función del servidor
  server = server
)
