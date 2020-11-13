#Add Sensor tab
#This page is adding a new sensor to the database, so it can be used for deployments

add_sensorUI <- function(id, label = "add_sensor", hobo_options, html_req, sensor_status_lookup){
  ns <- NS(id)
  
  tabPanel(title = "Add/Edit Sensor", value = "add_sensor",
           titlePanel("Add Sensor to Inventory or Edit Existing Sensor"), 
           sidebarPanel(
             numericInput(ns("serial_no"), html_req("Sensor Serial Number"), value = NA), 
             selectInput(ns("model_no"), html_req("Sensor Model Number"), choices = hobo_options, selected = NULL), 
             dateInput(ns("date_purchased"), "Purchase Date", value = as.Date(NA)), 
             selectInput(ns("sensor_status"), html_req("Sensor Status"), choices = sensor_status_lookup$sensor_status, selected = "Good Order"),
             actionButton(ns("add_sensor"), "Add Sensor"), 
             actionButton(ns("add_sensor_deploy"), "Deploy this Sensor"), 
             actionButton(ns("clear"), "Clear Fields")
           ),
           
           mainPanel(
             DTOutput(ns("sensor_table")),
             textOutput(ns("testing"))
           )
  )
}

add_sensor <- function(input, output, session, parent_session, poolConn, sensor_status_lookup, deploy){
  #define ns to use in modals
  ns <- session$ns
  #start reactiveValues for this section
  rv <- reactiveValues()
  
  #Sensor Serial Number List
  sensor_table_query <-  "select * from fieldwork.inventory_sensors_full"
  rv$sensor_table <- odbc::dbGetQuery(poolConn, sensor_table_query)
  
  #upon breaking a sensor in deploy
  observeEvent(deploy$refresh_sensor(),{
    rv$sensor_table <- odbc::dbGetQuery(poolConn, sensor_table_query)
  })
  
  #if input serial number is already in the list, then suggest the existing model number. if it isn't already there, show NULL
  model_no_select <- reactive(if(input$serial_no %in% rv$sensor_table$sensor_serial) dplyr::filter(rv$sensor_table, sensor_serial == input$serial_no) %>% dplyr::select(sensor_model) %>% dplyr::pull() else "")
  
  observe(updateSelectInput(session, "model_no", selected = model_no_select()))
  
  #if input serial number is already in the list, then suggest the date_purchased. if it isn't already there, show NULL
  date_purchased_select <- reactive(if(input$serial_no %in% rv$sensor_table$sensor_serial) dplyr::filter(rv$sensor_table, sensor_serial == input$serial_no) %>% dplyr::select(date_purchased) %>% dplyr::pull() else as.Date(NA))
  
  observe(updateDateInput(session, "date_purchased", value = date_purchased_select()))
  
  #if sensor status is already in the list, then suggest the sensor status if it isn't already there, show "Good Order"
  sensor_status_select <- reactive(if(input$serial_no %in% rv$sensor_table$sensor_serial) dplyr::filter(rv$sensor_table, sensor_serial == input$serial_no) %>% dplyr::select(sensor_status) %>% dplyr::pull() else "Good Order")
  
  observe(updateSelectInput(session, "sensor_status", selected = sensor_status_select()))
  
  rv$status_lookup_uid <- reactive(sensor_status_lookup %>% dplyr::filter(sensor_status == input$sensor_status) %>% 
                           select(sensor_status_lookup_uid) %>% pull())
  
  rv$date_purchased <- reactive(if(length(input$date_purchased) == 0) "NULL" else paste0("'", input$date_purchased, "'"))
  
  #enable/disable the "add sensor button" if all fields are not empty
  observe({toggleState(id = "add_sensor", condition = nchar(input$serial_no) > 0 & nchar(input$model_no) > 0)})
  observe({toggleState(id = "add_sensor_deploy", input$serial_no %in% rv$sensor_table$sensor_serial)})
  
  #change label from Add to Edit if the sensor already exists in db
  #rv$label <- reactive(if(!(as.numeric(input$serial_no) %in% rv$sensor_table$sensor_serial)) "Add Sensor" else "Edit Sensor")
  rv$label <- reactive(if(!(input$serial_no %in% rv$sensor_table$sensor_serial)) "Add Sensor" else "Edit Sensor")
  observe(updateActionButton(session, "add_sensor", label = rv$label()))
  
  #Write to database when button is clicked
  observeEvent(input$add_sensor, { #write new sensor info to db
    if(!(input$serial_no %in% rv$sensor_table$sensor_serial)){
      odbc::dbGetQuery(poolConn, paste0(
        "INSERT INTO fieldwork.inventory_sensors (sensor_serial, sensor_model, date_purchased, sensor_status_lookup_uid) 
	      VALUES ('", input$serial_no, "','", input$model_no, "',",  rv$date_purchased(), ", '", rv$status_lookup_uid(), "')"))
      output$testing <- renderText({
        isolate(paste("Sensor", input$serial_no, "added."))
      })
    }else{ #edit sensor info
      odbc::dbGetQuery(poolConn, paste0("UPDATE fieldwork.inventory_sensors SET sensor_model = '", input$model_no, 
                                        "', date_purchased = ", rv$date_purchased(), ", 
                                        sensor_status_lookup_uid = '", rv$status_lookup_uid(), "' WHERE sensor_serial = '", input$serial_no, "'"))
      output$testing <- renderText({
        isolate(paste("Sensor", input$serial_no, "edited."))
      })
    }
    #reset("serial_no")
    #reset("model_no")
    #reset("date")
    #reset("sensor_status")
    #update sensor list following addition
    rv$sensor_table <- odbc::dbGetQuery(poolConn, sensor_table_query)
    rv$active_row <- which(rv$sensor_table$sensor_serial == input$serial_no, arr.ind = TRUE)
    row_order <- order(
      seq_along(rv$sensor_table$sensor_serial) %in% rv$active_row, 
      decreasing = TRUE
      )
    
    rv$sensor_table <- rv$sensor_table[row_order, ]
    dataTableProxy('sensor_table') %>% 
      selectRows(1)
    
    
  })
  
  #switch tabs to "Deploy" and update Sensor ID to the current Sensor ID (if the add/edit button says edit sensor)
  rv$refresh_serial_no <- 0 
  observeEvent(input$add_sensor_deploy, {
    rv$refresh_serial_no <- rv$refresh_serial_no + 1
    #updateSelectInput(session, "sensor_id", selected = input$serial_no)
    updateTabsetPanel(session = parent_session, "inTabset", selected = "deploy_tab")
  })
  
  rv$sensor_table_display <- reactive(rv$sensor_table %>% mutate("date_purchased" = as.character(date_purchased)) %>% 
                                     rename("Serial Number" = "sensor_serial", "Model Number" = "sensor_model", 
                                            "Date Purchased" = "date_purchased", "SMP ID" = "smp_id", "Site" = "site_name", "Location" = "ow_suffix", "Status" = "sensor_status"))
  
  output$sensor_table <- renderDT(
    rv$sensor_table_display(),
    selection = "single",
    style = 'bootstrap', 
    class = 'table-responsive, table-hover',
    options = list(scroller = TRUE, 
                   scrollX = TRUE, 
                   scrollY = 550), 
    callback = JS('table.page("next").draw(false);')
  )
  
  observeEvent(input$sensor_table_rows_selected, {
    updateTextInput(session, "serial_no", value = rv$sensor_table$sensor_serial[input$sensor_table_rows_selected])
  })
  
  #clear all fields
  #bring up dialogue box to confirm
  observeEvent(input$clear, {
    showModal(modalDialog(title = "Clear All Fields", 
                          "Are you sure you want to clear all fields on this tab?", 
                          modalButton("No"), 
                          actionButton(ns("confirm_clear"), "Yes")))
  })
  
  observeEvent(input$confirm_clear, {
    reset("serial_no")
    reset("model_no")
    reset("date_purchased")
    reset("sensor_status")
    removeModal()
  })
  
  
  
  return(
    list(
      refresh_serial_no = reactive(rv$refresh_serial_no),
      serial_no = reactive(input$serial_no),
      sensor_serial = reactive(rv$sensor_table$sensor_serial)
    )
  )
  
}