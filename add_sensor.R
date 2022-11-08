#Add Sensor tab
#This page is for adding a new sensor to the database, so it can be used for deployments

#1.0 UI -------
add_sensorUI <- function(id, label = "add_sensor", sensor_model_lookup, html_req, sensor_status_lookup, sensor_issue_lookup){
  #initialize namespace
  ns <- NS(id)
  
  tabPanel(title = "Add/Edit Sensor", value = "add_sensor",
           titlePanel("Add Sensor to Inventory or Edit Existing Sensor"), 
           #1.1 sidebarPanel-----
           sidebarPanel(
             numericInput(ns("serial_no"), html_req("Sensor Serial Number"), value = NA), 
             selectInput(ns("model_no"), html_req("Sensor Model Number"), choices = c("", sensor_model_lookup$sensor_model), 
                         selected = NULL), 
             dateInput(ns("date_purchased"), "Purchase Date", value = as.Date(NA)), 
             selectInput(ns("sensor_status"), html_req("Sensor Status"), choices = sensor_status_lookup$sensor_status, selected = "Good Order"),
             conditionalPanel(width = 12, 
               condition = 'input.sensor_status != "Good Order"', 
                              ns = ns,
                              selectInput(ns("issue_one"), html_req("Issue #1"), 
                                          choices = c("", sensor_issue_lookup$sensor_issue), selected = NULL), 
                              selectInput(ns("issue_two"), "Issue #2", 
                                          choices = c("", sensor_issue_lookup$sensor_issue), selected = NULL), 
                              checkboxInput(ns("request_data"), "Request Data Be Retrieved and Sent to PWD")
                              ), 
             actionButton(ns("add_sensor"), "Add Sensor"), 
             actionButton(ns("add_sensor_deploy"), "Deploy this Sensor"), 
             actionButton(ns("clear"), "Clear Fields"), 
             selectInput(ns("sensor_status_dl"), "Sensor Statuses to Download", choices = c("All", sensor_status_lookup$sensor_status)),
             downloadButton(ns("download"), "Download Sensor Inventory")
           ),
           #1.2 table -------
           mainPanel(
             DTOutput(ns("sensor_table")),
             textOutput(ns("testing"))
           )
  )
}

#2.0 Server ---------
add_sensorServer <- function(id, parent_session, poolConn, sensor_model_lookup, sensor_status_lookup, sensor_issue_lookup, deploy){
  
  moduleServer(
    id, 
    function(input, output, session){
  
      #2.0.1 set up ------
      #define ns to use in modals
      ns <- session$ns
      
      #start reactiveValues for this section
      rv <- reactiveValues()
      
      #2.1 Query sensor table ----
      #2.1.1 intial query -----
      #Sensor Serial Number List
      sensor_table_query <-  "select * from fieldwork.viw_inventory_sensors_full"
      rv$sensor_table <- odbc::dbGetQuery(poolConn, sensor_table_query)
      
      #2.1.2 query on update ----
      #upon breaking a sensor in deploy
      observeEvent(deploy$refresh_sensor(),{
        rv$sensor_table <- odbc::dbGetQuery(poolConn, sensor_table_query)
      })
      
      #2.1.3 show sensor table
      rv$sensor_table_display <- reactive(rv$sensor_table %>% 
                                            mutate("date_purchased" = as.character(date_purchased)) %>% 
                                            select("sensor_serial", "sensor_model", "date_purchased", "smp_id", 
                                                   "site_name", "ow_suffix", "sensor_status", "issue_one", "issue_two", "request_data") %>% 
                                            mutate_at(vars(one_of("request_data")), 
                                                      funs(case_when(. == 1 ~ "Yes"))) %>% 
                                            rename("Serial Number" = "sensor_serial", "Model Number" = "sensor_model", 
                                                   "Date Purchased" = "date_purchased", "SMP ID" = "smp_id", "Site" = "site_name", 
                                                   "Location" = "ow_suffix", "Status" = "sensor_status", 
                                                   "Issue #1" = "issue_one", "Issue #2" = "issue_two", "Request Data" = "request_data")
      )
      
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
      
      
      #2.3 prefilling inputs based on model serial number -----
      
      #select a row in the table to update the serial no. you can also just type in the serial number
      #see above for how updating the serial number updates the rest of the fields
      #this is different than the other tabs because there is no fixed smp_id type of primary key here - so you can either type OR select a row to get to the sensor, and you have to be able to type because you might want to add a new sensor this way. 
      #it works it's just different
      observeEvent(input$sensor_table_rows_selected, {
        updateTextInput(session, "serial_no", value = rv$sensor_table$sensor_serial[input$sensor_table_rows_selected])
      })
      
      #not sure why this is so many of the same ifs. can be consolidated later
      #if input serial number is already in the list, then suggest the existing model number. if it isn't already there, show NULL
      rv$model_no_select <- reactive(if(input$serial_no %in% rv$sensor_table$sensor_serial) dplyr::filter(rv$sensor_table, sensor_serial == input$serial_no) %>% dplyr::select(sensor_model) %>% dplyr::pull() else "")
      
      observe(updateSelectInput(session, "model_no", selected = rv$model_no_select()))
      
      #if input serial number is already in the list, then suggest the date_purchased. if it isn't already there, show NULL
      rv$date_purchased_select <- reactive(if(input$serial_no %in% rv$sensor_table$sensor_serial) dplyr::filter(rv$sensor_table, sensor_serial == input$serial_no) %>% dplyr::select(date_purchased) %>% dplyr::pull() else as.Date(NA))
      
      observe(updateDateInput(session, "date_purchased", value = rv$date_purchased_select()))
      
      observeEvent(input$serial_no, {
        
        #if input serial number is already in the list, then suggest the sensor status if it isn't already there, show "Good Order"
        rv$sensor_status_select <- if(input$serial_no %in% rv$sensor_table$sensor_serial) dplyr::filter(rv$sensor_table, sensor_serial == input$serial_no) %>% dplyr::select(sensor_status) %>% dplyr::pull() else "Good Order"
        
        updateSelectInput(session, "sensor_status", selected = rv$sensor_status_select)
        
      #if input serial number is already in the list, then suggest issue #1
        rv$issue_one_select <- if(input$serial_no %in% rv$sensor_table$sensor_serial) dplyr::filter(rv$sensor_table, sensor_serial == input$serial_no) %>% dplyr::select(issue_one) %>% dplyr::pull() else ""
        
       updateSelectInput(session, "issue_one", selected = rv$issue_one_select)
        
        #if input serial number is already in the list, then suggest issue #2
        rv$issue_two_select <- if(input$serial_no %in% rv$sensor_table$sensor_serial) dplyr::filter(rv$sensor_table, sensor_serial == input$serial_no) %>% dplyr::select(issue_two) %>% dplyr::pull() else ""
        
        updateSelectInput(session, "issue_two", selected = rv$issue_two_select)
        
        #if input serial number is already in the list, then suggest checkbox request data
        rv$request_data_select <- if(input$serial_no %in% rv$sensor_table$sensor_serial) dplyr::filter(rv$sensor_table, sensor_serial == input$serial_no) %>% dplyr::select(request_data) %>% dplyr::pull() else ""
        
        updateCheckboxInput(session, "request_data", value = rv$request_data_select)
      
        })
      
      #2.3 preparing inputs
      #get sensor status uid
      rv$status_lookup_uid <- reactive(sensor_status_lookup %>% dplyr::filter(sensor_status == input$sensor_status) %>% 
                               select(sensor_status_lookup_uid) %>% pull())
      
      #let date purchased be null
      rv$date_purchased <- reactive(if(length(input$date_purchased) == 0) "NULL" else paste0("'", input$date_purchased, "'"))
      
      #get sensor model lookup UID and let it be NULL 
      rv$model_lookup_uid <- reactive(sensor_model_lookup %>% dplyr::filter(sensor_model == input$model_no) %>% 
                                            select(sensor_model_lookup_uid) %>% pull())
      
      rv$sensor_model_lookup_uid <- reactive(if(nchar(input$model_no) == 0) "NULL" else paste0("'", rv$model_lookup_uid(), "'"))
      
      #get sensor issue lookup uid and let it be NULL (for issue #1 and issue #2)
      rv$issue_lookup_uid_one <- reactive(sensor_issue_lookup %>% dplyr::filter(sensor_issue == input$issue_one) %>% 
                                         select(sensor_issue_lookup_uid) %>% pull())
      
      rv$sensor_issue_lookup_uid_one <- reactive(if(nchar(input$issue_one) == 0) "NULL" else paste0("'", rv$issue_lookup_uid_one(), "'"))
      
      rv$issue_lookup_uid_two <- reactive(sensor_issue_lookup %>% dplyr::filter(sensor_issue == input$issue_two) %>% 
                                         select(sensor_issue_lookup_uid) %>% pull())
      
      rv$sensor_issue_lookup_uid_two <- reactive(if(nchar(input$issue_two) == 0) "NULL" else paste0("'", rv$issue_lookup_uid_two(), "'"))
      
      #let checkbox input be NULL if blank (instead of FALSE)
      rv$request_data <- reactive(if(input$request_data == TRUE) paste0("'TRUE'") else "NULL")
      
      #2.4 toggle states/labels -----
      #enable/disable the "add sensor button" if all fields are not empty
      observe({toggleState(id = "add_sensor", condition = nchar(input$serial_no) > 0 & nchar(input$model_no) > 0)})
      observe({toggleState(id = "add_sensor_deploy", input$serial_no %in% rv$sensor_table$sensor_serial)})
      
      #enable/disable issue #2 button if issue #1 is filled
      observe(toggleState(id = "issue_two", condition = nchar(input$issue_one) > 0))
      
      #change label from Add to Edit if the sensor already exists in db
      #rv$label <- reactive(if(!(as.numeric(input$serial_no) %in% rv$sensor_table$sensor_serial)) "Add Sensor" else "Edit Sensor")
      rv$label <- reactive(if(!(input$serial_no %in% rv$sensor_table$sensor_serial)) "Add Sensor" else "Edit Sensor")
      observe(updateActionButton(session, "add_sensor", label = rv$label()))
      
      observeEvent(input$sensor_status, {
        #if Good Order, clear issues fields 
        if(rv$status_lookup_uid() == 1){
          reset("issue_one")
          reset("issue_two")
          reset("request_data")
          # updateSelectInput(session, "issue_one", selected = "")
          # updateSelectInput(session, "issue_two", selected = "")
          # updateCheckboxInput(session, "request_data", value = FALSE)
        }
      })
      
      #2.5 add/edit table ------
      #Write to database when button is clicked
      observeEvent(input$add_sensor, { #write new sensor info to db
        
        if(!(input$serial_no %in% rv$sensor_table$sensor_serial)){
          add_sensor_query <- paste0(
            "INSERT INTO fieldwork.tbl_inventory_sensors (sensor_serial, sensor_model_lookup_uid, date_purchased, sensor_status_lookup_uid, 
            sensor_issue_lookup_uid_one, sensor_issue_lookup_uid_two, request_data) 
    	      VALUES ('", input$serial_no, "', ",rv$sensor_model_lookup_uid(), ", ",  
            rv$date_purchased(), ", '", rv$status_lookup_uid(), "', ", 
    	                   rv$sensor_issue_lookup_uid_one(), ", ", rv$sensor_issue_lookup_uid_two(), ", ", rv$request_data(), ")")
          
          odbc::dbGetQuery(poolConn, add_sensor_query)
          
          output$testing <- renderText({
            isolate(paste("Sensor", input$serial_no, "added."))
          })
        }else{ #edit sensor info
          odbc::dbGetQuery(poolConn, paste0("UPDATE fieldwork.tbl_inventory_sensors SET 
                                            sensor_model_lookup_uid = ", rv$sensor_model_lookup_uid(), ",
                                            date_purchased = ", rv$date_purchased(), ", 
                                            sensor_status_lookup_uid = '", rv$status_lookup_uid(), "', 
                                            sensor_issue_lookup_uid_one = ", rv$sensor_issue_lookup_uid_one(), ",
                                            sensor_issue_lookup_uid_two = ", rv$sensor_issue_lookup_uid_two(), ", 
                                            request_data = ", rv$request_data(), "
                                            WHERE sensor_serial = '", input$serial_no, "'"))
          output$testing <- renderText({
            isolate(paste("Sensor", input$serial_no, "edited."))
          })
        }
        
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
        updateTabsetPanel(session = parent_session, "inTabset", selected = "deploy_tab")
      })
      
      #2.6 clear fields
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

      
      #2.7 download ----
      #downloading the sensor table
      #filter based on selected status
      rv$sensor_table_download <- reactive(if(input$sensor_status_dl == "All"){
        rv$sensor_table_display()
      }else{
        rv$sensor_table_display() %>% dplyr::filter(Status == input$sensor_status_dl)
      })
      
      output$download <- downloadHandler(
        filename = function(){
          paste("Sensor_inventory", "_", Sys.Date(), ".csv", sep = "")
        }, 
        content = function(file){
          write.csv(rv$sensor_table_download(), file, row.names = FALSE)
        }
      )

      #2.8 return values ------
      return(
        list(
          refresh_serial_no = reactive(rv$refresh_serial_no),
          serial_no = reactive(input$serial_no),
          sensor_serial = reactive(rv$sensor_table$sensor_serial)
        )
      )
      
    }
  )
}