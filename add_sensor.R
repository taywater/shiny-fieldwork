add_sensorUI <- function(id, label = "add_sensor"){
  ns <- NS(id)
  
  tabPanel(title = "Add Sensor", value = "add_sensor",
           titlePanel("Add Sensor to Inventory"), 
           sidebarPanel(
             numericInput(ns("serial_no"), html_req("Sensor Serial Number"), value = NA), 
             selectInput(ns("model_no"), html_req("Sensor Model Number"), choices = hobo_options, selected = NULL), 
             dateInput(ns("date_purchased"), "Purchase Date", value = as.Date(NA)), 
             actionButton(ns("add_sensor"), "Add Sensor"), 
             actionButton(ns("add_sensor_deploy"), "Deploy this Sensor")
           ),
           
           mainPanel(
             DTOutput(ns("sensor_table")),
             textOutput(ns("testing"))
           )
  )
}

add_sensor <- function(input, output, session, parent_session){
  #define ns to use in modals
  ns <- session$ns
  #start reactiveValues for this section
  rv <- reactiveValues()
  
  #Sensor Serial Number List
  hobo_list_query <-  "select inv.sensor_serial, inv.sensor_model, inv.date_purchased, ow.smp_id, ow.ow_suffix from fieldwork.inventory_sensors inv
                          left join fieldwork.deployment d on d.inventory_sensors_uid = inv.inventory_sensors_uid AND d.collection_dtime_est is NULL
                          left join fieldwork.ow ow on ow.ow_uid = d.ow_uid"
  rv$hobo_list <- odbc::dbGetQuery(poolConn, hobo_list_query)
  
  #if input serial number is already in the list, then suggest the existing model number. if it isn't already there, show NULL
  model_no_select <- reactive(if(input$serial_no %in% rv$hobo_list$sensor_serial) dplyr::filter(rv$hobo_list, sensor_serial == input$serial_no) %>% dplyr::select(sensor_model) %>% dplyr::pull() else "")
  
  observe(updateSelectInput(session, "model_no", selected = model_no_select()))
  
  #if input serial number is already in the list, then suggest the date_purchased. if it isn't already there, show NULL
  date_purchased_select <- reactive(if(input$serial_no %in% rv$hobo_list$sensor_serial) dplyr::filter(rv$hobo_list, sensor_serial == input$serial_no) %>% dplyr::select(date_purchased) %>% dplyr::pull() else as.Date(NA))
  
  observe(updateDateInput(session, "date_purchased", value = date_purchased_select()))
  
  rv$date_purchased <- reactive(if(length(input$date_purchased) == 0) "NULL" else paste0("'", input$date_purchased, "'"))
  
  #enable/disable the "add sensor button" if all fields are not empty
  observe({toggleState(id = "add_sensor", condition = nchar(input$serial_no) > 0 & nchar(input$model_no) > 0)})
  observe({toggleState(id = "add_sensor_deploy", input$serial_no %in% rv$hobo_list$sensor_serial)})
  
  #change label from Add to Edit if the sensor already exists in db
  #rv$label <- reactive(if(!(as.numeric(input$serial_no) %in% rv$hobo_list$sensor_serial)) "Add Sensor" else "Edit Sensor")
  rv$label <- reactive(if(!(input$serial_no %in% rv$hobo_list$sensor_serial)) "Add Sensor" else "Edit Sensor")
  observe(updateActionButton(session, "add_sensor", label = rv$label()))
  
  #Write to database when button is clicked
  observeEvent(input$add_sensor, { #write new sensor info to db
    if(!(input$serial_no %in% rv$hobo_list$sensor_serial)){
      odbc::dbGetQuery(poolConn, paste0(
        "INSERT INTO fieldwork.inventory_sensors (sensor_serial, sensor_model, date_purchased) 
	      VALUES ('", input$serial_no, "','", input$model_no, "',",  rv$date_purchased(), ")"))
      output$testing <- renderText({
        isolate(paste("Sensor", input$serial_no, "added."))
      })
    }else{ #edit sensor info
      odbc::dbGetQuery(poolConn, paste0("UPDATE fieldwork.inventory_sensors SET sensor_model = '", input$model_no, 
                                        "', date_purchased = ", rv$date_purchased(), " WHERE sensor_serial = '", input$serial_no, "'"))
      output$testing <- renderText({
        isolate(paste("Sensor", input$serial_no, "edited."))
      })
    }
    reset("serial_no")
    reset("model_no")
    reset("date")
    #update sensor list following addition
    rv$hobo_list <- odbc::dbGetQuery(poolConn, hobo_list_query)
  })
  
  #switch tabs to "Deploy" and update Sensor ID to the current Sensor ID (if the add/edit button says edit sensor)
  rv$refresh_serial_no <- 0 
  observeEvent(input$add_sensor_deploy, {
    rv$refresh_serial_no <- rv$refresh_serial_no + 1
    #updateSelectInput(session, "sensor_id", selected = input$serial_no)
    updateTabsetPanel(session = parent_session, "inTabset", selected = "deploy_tab")
  })
  
  rv$hobo_list_display <- reactive(rv$hobo_list %>% mutate("date_purchased" = as.character(date_purchased)) %>% 
                                     rename("Serial Number" = "sensor_serial", "Model Number" = "sensor_model", 
                                            "Date Purchased" = "date_purchased", "SMP ID" = "smp_id", "OW Suffix" = "ow_suffix"))
  
  output$sensor_table <- renderDT(
    rv$hobo_list_display(),
    selection = "single",
    style = 'bootstrap', 
    class = 'table-responsive, table-hover',
    options = list(scroller = TRUE, 
                   scrollX = TRUE, 
                   scrollY = 550)
  )
  
  observeEvent(input$sensor_table_rows_selected, {
    updateTextInput(session, "serial_no", value = rv$hobo_list$sensor_serial[input$sensor_table_rows_selected])
  })
  
  return(
    list(
      refresh_serial_no = reactive(rv$refresh_serial_no),
      serial_no = reactive(input$serial_no),
      sensor_serial = reactive(rv$hobo_list$sensor_serial)
    )
  )
  
}