
webshot::install_phantomjs(force = TRUE)
library(flip)
library(httr)
library(jsonlite)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinybusy)
library(parmesan)
library(vctrs)
library(dsmodules)
library(hgchmagic) #767c3867535994f1a1fd8c24594d40db3128843d
library(leaflet.extras)
library(ltgeo) #dev
Sys.setenv(OPENSSL_CONF="/dev/null")
ui <-  fluidPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="app.min.css"),
    tags$script(src="app.min.js")
  ),
  busy_start_up(
    loader = tags$img(
      src = "logos/loading_gris.gif",
      width = 100
    ),
    mode = "manual",
    color = "#435b69",
    background = "#FFF"
  ),
  div(class = "layout-container",
      div(class = "layout-panels",
          div(class = "app-container",
              div(class = "panel top-malibu",
                  div (class = "panel-body panel-filters",
                       uiOutput("controls")
                  ),
                  div(class="footer",
                      tags$a(
                        img(src= 'logos/lg_ds.svg', align = "left", width = 100)))
              ),
              div(class = "panel",
                  div (class = "panel-body",
                       div(style="flex-grow: 1; min-width: 620px;",
                           div(class = "head-viz",
                               div(style = "display:flex;gap:20px;margin-bottom: 20px;align-items: flex-end;",
                                   "VISUALIZACIÓN",
                                   uiOutput("viz_icons")
                               ),
                               uiOutput("descargas")
                           )),
                       div(class = "viz-nucleo",
                           uiOutput("viz_view")
                       )
                  )
              ),
              div(class = "panel",
                  div (class = "panel-body",
                       div(style="flex-grow: 1; min-width: 280px;",
                           div(style = "display:block;",
                               div(class = "viz-center",
                                   div(style = "margin: 10px 0px;", "DETALLE"),
                                   verbatimTextOutput("test"),
                                   uiOutput("info_click")
                               )
                           )
                       )
                  )
              )
          )
      )
  )
)


server <- function(input, output, session) {



  data <- reactiveValues(info = NULL)

  observe({
    data$info <- flip:::api_data("https://cms.flip.datasketch.co/api/periodistas-asesinados")
  })

  observe({
    if (is.null(data$info)) return()
    Sys.sleep(3)
    remove_start_up(timeout = 200)
  })

  var_dic <- reactive({

    data.frame(id = c("departamento", "anio_mes_agresion",
                      "presunto_autor",
                      "genero", "cargo"),
               label = c("Departamento", "Fecha del asesinato",
                         "Presunto autor",
                         "Género de la víctima", "Cargo o profesión de la víctima"),
               clasificacion = c("Ubicación y tiempo del evento", "Ubicación y tiempo del evento",
                                 "Detalles del evento",
                                 "Información de la víctima", "Información de la víctima"))
  })

  var_opts <- reactive({
    req(var_dic())
    df <- var_dic()
    organized_list <- lapply(unique(df$clasificacion), function(classif) {
      subset_df <- df |> filter(clasificacion %in% classif)
      setNames(as.list(subset_df$id), subset_df$label)
    })
    names(organized_list) <- unique(df$clasificacion)
    organized_list
  })

  show_deptos <- reactive({
    req(input$var_viz)
    input$var_viz != "departamento"
  })

  fecha_min <- reactive({
    req(data$info)
    min(data$info$fecha_agresion, na.rm = T)
  })
  fecha_max <- reactive({
    req(data$info)
    max(data$info$fecha_agresion, na.rm = T)
  })

  pickerOpts <- reactive({
    list(
      `actions-box` = TRUE,
      `deselect-all-text` = "Ninguno",
      `select-all-text` = "Todos",
      title = "Todos"
    )
  })

  data_fecha_filter <- reactive({
    req(data$info)
    df <- data$info
    if (!is.null(input$fechaId)) {
      df <- dsdataprep:::filter_ranges(df, range = input$fechaId, by = "fecha_agresion")
    }
    df
  })

  deptos_opts <- reactive({
    req(data_fecha_filter())
    sort(unique(data_fecha_filter()$departamento))
  })


  data_depto_filter <- reactive({
    req(data_fecha_filter())
    req(input$var_viz)
    df <- data_fecha_filter()
    if (input$var_viz != "departamento") {
      if (!is.null(input$deptosId)) {
        df <- df |> dplyr::filter(departamento %in% input$deptosId)
      }
    }

    df
  })


  genero_opts <- reactive({
    req(data_depto_filter())
    c("Todos", sort(unique(data_depto_filter()$genero)))
  })

  autor_opts <-  reactive({
    req(data_depto_filter())
    sort(unique(data_depto_filter()$presunto_autor))
  })

  cargo_opts <- reactive({
    req(data_depto_filter())
    sort(unique(data_depto_filter()$cargo))
  })

  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)

  output_parmesan("controls",
                  input = input, output = output, session = session,
                  env = environment())

  dic_inputs <- reactive({
    data.frame(id = c("fechaId",
                      "deptosId",
                      "agresionId",
                      "generoId",
                      "autorId"),
               label = c("Fecha",
                         "Departamento",
                         "Tipo de agresión",
                         "Género",
                         "Presunto autor"))
  })

  caption_text <- reactive({
    req(dic_inputs())
    req(input$var_viz)
    dic <- dic_inputs()
    htmltools::HTML(paste0(
      lapply(1:nrow(dic), function(i) {
        tx <- ""
        input_change <- input[[dic$id[i]]]
        if (input$var_viz == "departamento") {
          if (dic$id[i] == "deptosId") input_change <- NULL
        }

        if (!is.null(input_change)) {
          tx <- paste0("<b>", dic$label[i], ": </b>", paste0(input_change, collapse = ","), "</br>", collapse = "")
        }
        tx
      }), collapse = ""))
  })

  list_inputs <- reactive({
    input_genero <- input$generoId
    if(!is.null(input$generoId)) {
      if (input$generoId == "Todos") {
        input_genero <- NULL
      }
    }

    list(
      "presunto_autor" = input$autorId,
      "genero" = input_genero,
      "cargo" = input$cargoId
    )

  })

  dic <- reactive({
    req(data_depto_filter())
    data.frame(id = c("presunto_autor", "genero", "cargo"),
               hdtype = c("Cat", "Cat", "Cat"))
  })


  data_filter_gen <- reactive({
    req(data_depto_filter())
    if (nrow(data_depto_filter()) == 0) return()
    ls <- list_inputs()
    dsdataprep::data_filter(data = data_depto_filter(),
                            dic = dic(), var_inputs = ls, .id = "id")
  })


  data_filter <- reactive({
    req(data_filter_gen())
    if (nrow(data_filter_gen()) == 0) return()
    df <- data_filter_gen()
    if (!is.null(input$alertaId)) {
      if (input$alertaId) {
        df <- df |> filter(alerta_genero == "Sí")
      }
    }
    df
  })


  data_viz <- reactive({
    req(input$var_viz)
    req(data_filter())
    if (nrow(data_filter()) == 0) return()
    df <- data_filter()
    var_viz <- input$var_viz
    if (input$var_viz == "anio_mes_agresion") {
      var_viz <- "anio_agresion"
      df <- df |> drop_na(anio_mes_agresion)
      df$anio_mes_agresion <- as.character(df$anio_mes_agresion)
      df$anio_agresion <- as.character(df$anio_agresion)
      if (length(unique(df$anio_agresion)) < 3) {
        var_viz <- "anio_mes_agresion"
      }
    }

    df <- dsdataprep::aggregation_data(data = df,
                                       agg = "count",
                                       group_var = var_viz,
                                       percentage = TRUE, percentage_name = "porcentaje")
    dic <- var_dic() |> filter(id %in% input$var_viz)
    df$..labels <- paste0(dic$label, ": ", df[[1]], "<br/>
                         Conteo: ", df[[2]], " (", round(df[[3]], 2), "%)")
    df
  })


  posible_viz <- reactive({
    req(data_viz())
    if (nrow(data_viz()) == 0) return()
    viz <- c("bar", "treemap", "table")
    if ("departamento" %in% names(data_viz())) viz <- c("map", viz)
    if (any(c("anio_mes_agresion", "anio_agresion") %in% names(data_viz()))) viz <- c("line", "table")
    viz
  })

  actual_but <- reactiveValues(active = NULL)

  observe({
    req(posible_viz())
    if (is.null(input$viz_selection)) return()
    viz_rec <- posible_viz()
    if (input$viz_selection %in% viz_rec) {
      actual_but$active <- input$viz_selection
    } else {
      actual_but$active <- viz_rec[1]
    }

  })

  output$viz_icons <- renderUI({
    req(posible_viz())
    possible_viz <- posible_viz()
    shinyinvoer::buttonImageInput('viz_selection',
                                  " ",
                                  images = possible_viz,
                                  path = "icons/",
                                  active = actual_but$active,
                                  imageStyle = list(shadow = TRUE,
                                                    borderColor = "#ffffff",
                                                    padding = "3px"))

  })

  viz_func <- reactive({
    if (is.null(actual_but$active)) return()
    viz_type <- "CatNum"
    if (actual_but$active == "line")  viz_type <- "DatNum"
    viz <- paste0("hgchmagic::hgch_", actual_but$active, "_", viz_type)
    if (actual_but$active == "map") viz <- "ltgeo::lt_choropleth_GnmNum"
    print(viz)
    viz
  })

  title_viz <- reactive({
    req(data_viz())
    if (nrow(data_viz()) == 0) return()
    dic <- var_dic() |> filter(id %in% names(data_viz())[1])
    title_viz <- paste0("Periodistas asesinados por ", dic$label)
    title_viz
  })

  viz_opts <- reactive({
    if (is.null(actual_but$active)) return()
    req(data_viz())
    if (nrow(data_viz()) == 0) return()
    if (actual_but$active != "map") {
      opts <- list(
        data = data_viz(),
        label_wrap = 100,
        label_wrap_legend = 100,
        collapse_rows = TRUE,
        data_labels_show = TRUE,
        shiny_cursor = "pointer",
        shiny_clickable = TRUE,
        data_labels_align = 'middle',
        title = title_viz(),
        text_family ='IBM Plex Sans',
        palette_colors = c("#FF5100", "#FF9A2D", "#FFD35B", "#46B9F3", "#AAEAFF", "#00B18D", "#004286")
      )
      if (actual_but$active == "treemap") {
        opts$data_labels_inside <- TRUE
      }
      if (!any(c("anio_mes_agresion", "anio_agresion") %in% names(data_viz()))) {
        opts$bar_orientation <- "hor"
        opts$sort <- "desc"
      }
    } else {
      opts <- list(
        data = data_viz(),
        map_name = "col_large",
        collapse_rows = TRUE,
        map_tiles = "CartoDB",
        map_zoom_snap = 0.25,
        map_zoom_delta = 0.25,
        palette_colors = c("#FFF37A", "#FF5100"),
        map_min_zoom = 5.25,
        map_max_zoom = 12
      )
    }
    opts$title_size <- 15
    opts$text_family <- "Fira Sans"
    opts$title_family <- "Fira Sans"
    opts
  })


  viz_down <- reactive({
      req(actual_but$active)
      if (actual_but$active == "table") return()
      req(data_viz())
      req(viz_func())
      hc <- suppressWarnings(
        do.call(eval(parse(text = viz_func())), viz_opts())
      )
      if (actual_but$active != "map") {
        if (!is.null(caption_text())) {
          hc <- hc  |>
            hc_legend( verticalAlign = "top" ) |>
            hc_caption(text = caption_text())
        }
      } else {
        hc <- hc |>
          addControl(title_viz(), position = "topleft", className="map-title") |>
          leaflet::setView(lng = -74.29, lat = 3.57, 4)
        if (!is.null(caption_text())) {
          hc <- hc |>
            addControl(caption_text(), position = "bottomright")
        }
      }
      hc
  })



  output$hgch_viz <- highcharter::renderHighchart({
    req(actual_but$active)
    req(data_viz())
    if (actual_but$active %in% c("table", "map")) return()
     viz_down()

  })

  output$lflt_viz <- leaflet::renderLeaflet({
    req(actual_but$active)
    req(data_viz())
    if (!actual_but$active %in% c("map")) return()
    viz_down()
  })


  data_down <- reactive({
    req(data_filter())
    if (nrow(data_filter()) == 0) return()
    df <- data_filter()
    df <- df[, c( "nombre", "apellido","fecha_agresion", "presunto_autor",
                 "departamento", "genero","cargo")]
    df
  })

  output$dt_viz <- DT::renderDataTable({
    req(actual_but$active)
    if (actual_but$active != "table") return()
    req(data_down())
    df <- data_down()
    dtable <- DT::datatable(df,
                            rownames = F,
                            selection = 'none',
                            options = list(
                              language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                              scrollX = T,
                              fixedColumns = TRUE,
                              fixedHeader = TRUE,
                              scrollY = "500px",
                              autoWidth = TRUE
                            ))
    dtable
  })

  output$viz_view <- renderUI({
    req(actual_but$active)
    if (is.null(input$dimension)) return()
    height_viz <- input$dimension[2] - 130
    tx <- "No hay información para los filtros seleccionados"
    if (actual_but$active != "table") {
      if (is.null(data_viz())) return(tx)
    }

    viz <- actual_but$active
    if (viz == "map") {
      shinycustomloader::withLoader(
        leaflet::leafletOutput("lflt_viz", height = height_viz),
        type = "html", loader = "loader4"
      )
    } else if (viz == "table") {
      shinycustomloader::withLoader(
        DT::dataTableOutput("dt_viz", width = input$dimension[1]-500),
        type = "html", loader = "loader4"
      )
    } else {
      shinycustomloader::withLoader(
        highcharter::highchartOutput("hgch_viz", height = height_viz),
        type = "html", loader = "loader4"
      )
    }
  })



  output$descargas <- renderUI({
    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      dsmodules::downloadImageUI("download_viz", dropdownLabel ="Descargar", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown", text = "Descargar")
    } else {
      dsmodules::downloadTableUI("dropdown_table", dropdownLabel = "Descargar", formats = c("csv", "xlsx", "json"), display = "dropdown", text = "Descargar")
    }
  })

  observe({
    dsmodules::downloadTableServer("dropdown_table", element = reactive(data_down()), formats = c("csv", "xlsx", "json"))
    dsmodules::downloadImageServer("download_viz", element = reactive(viz_down()), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")
  })


  click_info <- reactiveValues(id = NULL)

  observeEvent(input$hcClicked, {
    click_info$id <- gsub("<br/>", " ", input$hcClicked$id)
  })
  observe({
    if (is.null(input$lflt_viz_shape_click)) return()
    deptos_mapa <- input$lflt_viz_shape_click$id
    deptos_mapa[deptos_mapa == "BOGOTÁ, D.C."] <- "BOGOTÁ"
    deptos_mapa[deptos_mapa == "ARCHIPIELAGO DE SAN ANDRES, PROVIDENCIA Y SANTA CATALINA"] <- toupper("San Andres y Providencia")
    click_info$id <- deptos_mapa
  })

  observeEvent(list(input$var_viz, input$viz_selection, input$fechaId,
                    input$deptosId, input$generoId,  input$cargoId), {
                      click_info$id <- NULL
                    })


  data_click <- reactive({
    req(click_info$id)
    print(click_info$id)
    req(input$var_viz)
    req(data_filter())
    df <-  data_filter()

    var_viz <- input$var_viz
    if (input$var_viz == "anio_mes_agresion") {
      var_viz <- "anio_agresion"
      if (length(unique(df$anio_agresion)) < 3) {
        var_viz <- "anio_mes_agresion"
      }
    }

    if (actual_but$active == "map") {
      df$departamento <- toupper(df$departamento)
    }

    df <- df |>
      dplyr::filter(!!dplyr::sym(var_viz) %in% click_info$id)

    df
  })


  output$info_click <-renderUI({ # reactive({#
    tx <- HTML("<div class = 'click'>
               <img src='click/click.svg' class = 'click-img'/><br/>
               Da <b>clic sobre la visualización</b> <br/> para ver más información.")
    if (is.null(click_info$id)) return(tx)
    req(data_click())
    if (nrow(data_click()) == 0) return("No hay información adicional disponible")

    df <- data_click()
    df$nombres <- paste0(df$nombre, " ", df$apellido)
    df$descripcion <- ifelse(!is.na(df$descripcion),
                             paste0("**id_", df$id, "**", df$descripcion), "sin detalle")
    data_click <- df |>
      select(nombres, Fecha = fecha_agresion, Lugar = departamento, `Presunto implicado` = presunto_autor, descripcion)
    info <- purrr::map(1:nrow(data_click), function(r) {
      info <- data_click[r, ]
      htmltools::div(class = "click-p",
                     htmltools::HTML(paste0(
                       purrr::map(names(info), function(v) {
                         tx <- paste0("<div class = 'click-body'><div class = 'click-tl'>",
                                      v, ":</div> <div class = 'click-info'>",
                                      info[[v]], "</div>\n                    </div>",
                                      collapse = "")
                         if (v == "nombres") {
                           if (!is.na(info[[v]])) {
                             tx <- paste0("<div class = 'name-click'>",info[[v]], "</div>")
                           }else {
                             tx <- "No identificado"
                           }
                         }
                         if (v == "descripcion") {
                           if (info[[v]] != "sin detalle") {
                             id_button <- str_extract(info[[v]], "(?<=\\*\\*).+?(?=\\*\\*)")
                             tx <- paste0(actionButton(id_button, label = "Descripción"))
                           } else {
                             tx <- " "
                           }
                         }
                         tx
                       }), collapse = "")))
    })
    info
  })


  click_desc <- reactiveVal()

  observe({
    if (is.null(click_info$id)) return()
    req(data_click())
    if (nrow(data_click()) == 0) return()
    df <- data_click() |> drop_na(descripcion)
    ids <- paste0("id_", df$id)
    lapply(ids, function(id) {
      observeEvent(input[[id]], {
        click_desc(id)
      })
    })
  })

  data_modal <- reactive({
    if (is.null(click_info$id)) return()
    req(data_click())
    if (is.null(click_desc())) return()
    df <- data_click()
    df$id <- paste0("id_", df$id)
    df <- df |> filter(id %in% click_desc())
    HTML(df$descripcion)
  })

  observe({
    if (is.null(click_info$id)) return()
    req(data_click())
    if (nrow(data_click()) == 0) return()
    if (is.null(click_desc())) return()
    req(data_modal())
    observeEvent(input[[click_desc()]], {
      showModal(modalDialog(
        title = "Descripción del hecho",
        easyClose = TRUE,
        footer = NULL,
        data_modal()
      ))
    })
  })




}



shinyApp(ui, server)

