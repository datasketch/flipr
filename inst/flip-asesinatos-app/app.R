
webshot::install_phantomjs(force = FALSE)

library(tidyverse)
library(dstools)

library(shiny)
library(hdbase)
library(hdtable)
library(dplyr)
library(purrr)
library(shinyWidgets)
library(shinyinvoer)
library(shinycustomloader)
library(shinybusy)
library(shinyjs)
library(hgchmagic)
library(reactable)
library(dsmodules)

library(ltgeo)
library(dsopts)
library(stringi)



dt_table <- function(data, ...){
  DT::datatable(data)
}

# x0 <- jsonlite::read_json("https://cms.flip.datasketch.co/api/periodistas-asesinados")
# x <- x0 |> bind_rows()
# write_csv(x, "persiodistas-asesinados.csv")

x0 <- read_csv("persiodistas-asesinados.csv",show_col_types = FALSE) |>
  mutate(year = lubridate::year(fecha_agresion))

autores <- sort(unique(x0$presunto_autor))
departamentos <- sort(unique(x0$departamento))
genero <- sort(unique(x0$genero))

date_range <- range(x0$fecha_agresion)
years <- unique(sort(x0$year))

ui <- fluidPage(
  # busy_start_up(
  #   loader = tags$img(
  #     src = "logos/loading_gris.gif",
  #     width = 100
  #   ),
  #   mode = "manual",
  #   color = "#435b69",
  #   background = "#FFF"
  # ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.min.css"),
    tags$script(src="app.min.js")
  ),
  div(style = "display:flex;",
      div(class = "layout-container",
          div(class = "layout-panels",
              div(class = "app-container",
                  div(id = "filter-moe",
                      class = "panel",
                      style = "width: 350px;",
                      div(id="moe-panel_head",
                          class="panel-header",
                          p(class="panel-header-title",
                            "Datos")
                      ),
                      div(class = "panel-body",
                          div(class = "panel-content",
                              #actionButton("run_inputs", "Aplicar filtros"),
                              uiOutput("view_as"),
                              uiOutput("filtros"),
                              verbatimTextOutput("debug"),
                              br()
                          )
                      )
                  ),
                  div(id = "viz-moe",
                      class = "panel",
                      style = "flex-grow: 1; min-width: 630px; max-width: 800px",
                      div(class = "panel-header",
                          p(class="panel-header-title",
                            "Visualización"),
                          div(class = "head-viz",
                              uiOutput("viz_icons"),
                              # uiOutput("opts_ptc"),
                              # uiOutput("down_data"),
                              # uiOutput("downloads")
                          )
                      ),
                      div(class = "panel-body",
                          div(class = "panel-content",
                              uiOutput("viz_ui"),
                              br()
                          )
                      )
                  ),
                  div(id = "detail-moe",
                      class = "panel",
                      style = "width: 300px;",
                      div(id="moe-panel_head",
                          class="panel-header",
                          p(class="panel-header-title",
                            "Detalle")
                      ),
                      div(class = "panel-body",
                          div(class = "panel-content",
                              # uiOutput("violencia_extra"),
                              # tableOutput("violencia_tabla"),
                              # uiOutput("summary")
                              br()
                          )
                      ),
                      div(class = "panel-footer",
                          div(class = "footer-logos",
                              img(src= 'logos/flip.png',
                                  width = 130, height = 40),
                              tags$a(
                                href="https://www.datasketch.co", target="blank",
                                img(src= 'logos/lg_ds.svg',
                                    align = "left", width = 100, height = 70))
                          )
                      )
                  )

              )
          )
      )
  ),
  dsmodules::showDebug(hosts = c("127.0.0.1", "datasketch.shinyapps.io"))
)

server <-  function(input, output, session) {


  par <- list(region = NULL, tematica = NULL, grupo = NULL)
  url_par <- reactive({
    url_params(par, session)$inputs
  })

  output$view_as <- renderUI({

    radioButtons("view_as", "Ver por:",
                 c("Departamentos",
                   #"Género",
                   "Presuntos autores"),
                 selected = "Departamentos"
    )
  })

  output$filtros <- renderUI({


    list(
      h5("Filtros:"),
      pickerInput(
        inputId = "year",
        label = "Año",
        choices = years,
        options = list(
          `actions-box` = TRUE),
        multiple = TRUE
      ),
      pickerInput(
        inputId = "departamento",
        label = "Departamento",
        choices = departamentos,
        options = list(
          `actions-box` = TRUE),
        multiple = TRUE
      ),
      pickerInput(
        inputId = "autor",
        label = "Presunto autor",
        choices = autores,
        options = list(
          `actions-box` = TRUE),
        multiple = TRUE
      )
    )

  })



  values <- reactiveValues()
  observe({

    values$view_as <- input$view_as

    values$year <- as.numeric(input$year)
    values$departamento <- input$departamento
    values$departamento <- input$departamento
    values$autor <- input$autor

    values$viz_active <- input$viz_selection

  })



  data_filter <- reactive({
    x <- x0
    if(!is.empty(values$year)){
      x <- x0 |>
        filter(year %in% values$year)
    }
    if(!is.empty(values$departamento)){
      x <- x0 |>
        filter(departamento %in% values$departamento)
    }
    if(!is.empty(values$autor)){
      x <- x0 |>
        filter(presunto_autor %in% values$autor)
    }
    x
  })



  ### DATAPLOT

  data_plot <- reactive({

    req(values$viz_active)
    x <- data_filter()

    # If view as Departamentos
    if(values$view_as == "Departamentos"){
      # If map
      if(values$viz_active == "map"){
        x <- x |>
          select(departamento) |>
          summarise(total = n(), .by = "departamento")
      }
      # if Bar
      if(values$viz_active == "bar"){
        x <- x |>
          select(departamento) |>
          summarise(total = n(), .by = "departamento")
      }
    }

    # If view as Presuntos autores
    if(values$view_as == "Presuntos autores"){
      # if Bar
      if(values$viz_active == "bar"){
        x <- x |>
          select(presunto_autor) |>
          summarise(total = n(), .by = "presunto_autor")
      }
    }
    x
  })


  output$debug <- renderPrint({

    str(viz_type())
    str(viz_params())
    l <- reactiveValuesToList(values)
    str(l)
    # str(data_filter())
    # str(data_plot())

  })

  viz_type <- reactive({
    if(values$viz_active == "map"){
      viz_type <- "lt_choropleth"
    } else if(values$viz_active == "table"){
      viz_type <- "dt_table"
    } else {
      type <- "CatNum"
      viz_type <- paste0("hgch_", values$viz_active, "_", type)
    }
    viz_type
  })


  theme_viz <- reactive({
    opts <- NULL
    if(values$viz_active == "map"){
      opts <- list(
        #title = "Periodistas asesinados",
        border_color = "#3a3a3a",
        border_width = 1,
        na_color = "#f0f0f0",
        background_color = "#ffffff",
        # na_label = "Sin&nbsp;Información"
        na_label = "N/A",
        tooltip_template = "<b>{total} asesinatos</b><br><i>{departamento}</i>"
      )
    }

    if(values$view_as == "Presuntos autores"){
      if(values$viz_active == "bar"){
        opts <- list(
          orientation = "hor"
        )
      }
    }

    opts
  })

  viz_params <- reactive({
    opts <- theme_viz()
    data <- list(
      data = data_plot()
    )
    if(values$viz_active == "map"){
      viz_params <- list(
        map_name = "col_departments",
        var = "total"
      )
    }
    if(values$viz_active == "table"){
      viz_params <- NULL
    }
    c(data, viz_params, list(opts = opts))
  })


  hgch_viz <- reactive({
    hv <- do.call(viz_type(), viz_params())
    if (!values$viz_active %in%  c("map", "table")) {
      hv <- hv |> hc_legend(   verticalAlign = "top" )
      # if (!is.null(index_info())) {
      #   hv <- hv |>
      #     hc_caption(text = index_info())
      # }
    }
    hv

  })

  output$hgch_chart <- renderHighchart({
    if (values$viz_active %in% c("map", "table")) return()
    req(hgch_viz())
    hgch_viz()

  })

  output$lt_chart <- renderLeaflet({
    if (values$viz_active != "map") return()
    #req(hgch_viz())
    hgch_viz()
  })

  output$dt_chart <- DT::renderDataTable({
    if (values$viz_active != "table") return()
    #req(hgch_viz())
    hgch_viz()
  })



  output$viz_ui <- renderUI({
    if (is.null(data_plot())) return("No hay información para los filtros seleccionados")
    #req(values$viz_active)
    height_viz <- 500
    if (values$viz_active == "map") {
      viz <- leaflet::leafletOutput("lt_chart", height = height_viz)
    } else if(values$viz_active == "table"){
      viz <- DT::dataTableOutput("dt_chart", height = height_viz)
    } else {
      viz <- highchartOutput("hgch_chart", height = height_viz)
    }
    viz

  })


  # Defaults


  available_viz <- reactive({
    # if(is.null(input$what_table_input)) return()
    # if (is.null(data_plot())) return()
    #viz <- c("map", "bar", "line", "treemap", "table")
    req(values$view_as)

    str(values$view_as)

    viz <- c("table")

    if(values$view_as == "Departamentos"){
      viz <- c("map", "bar", "table")
    } else if(values$view_as == "Presuntos autores"){
      #viz <- c("bar", "line", "treemap", "table")
      viz <- c("bar", "table")
    }
    viz
  })


  # hover_viz <- reactive({
  #   req(available_viz())
  #   df_av <- data.frame(id = available_viz())
  #   df <- data.frame(id = c("map", "bar", "line", "pie", "treemap", "table"),
  #                    label = c("Mapa", "Barras", "Línea", "Pie", "Treemap", "Tabla"))
  #   df <- df_av |> dplyr::inner_join(df)
  #   df$label
  # })

  output$viz_icons <- renderUI({
    req(available_viz())

    if(values$view_as == "Departamentos"){
      active <- "map"
      values$viz_active <- "map"
    } else if(values$view_as == "Presuntos autores"){
      active <- "bar"
      values$viz_active <- "bar"
    }

    buttonImageInput('viz_selection',
                     " ",
                     images = available_viz(),
                     #tooltips = hover_viz(),
                     path = 'icons/',
                     active = active
    )

    ##




  })



}

shinyApp(ui, server)
