#SRT tabs
#This has a tab dropdown with two tabs, one for adding SRTs and one for viewing all SRTs

#1.0 UI -----

SRTUI <- function(id, label = "srt", srt_types, con_phase, priority, html_req, future_req){
  ns <- NS(id)
  navbarMenu("SRT", 
             #1.1 Add/Edit ----
             tabPanel("Add/Edit SRT", value = "srt_tab", 
                      titlePanel("Add/Edit Simulated Runoff Test (SRT)"), 
                      fluidRow(
                        #1.1.1 SidebarPanel ----
                        column(width = 5,
                               #split layout with left and right
                               sidebarPanel(width = 12, 
                                            fluidRow(h5()),
                                            fluidRow(h5()),
                                              selectizeInput(ns("system_id"), future_req(html_req("System ID")), 
                                                          choices = NULL, 
                                                          options = list(
                                                            placeholder = 'Select an Option',
                                                            onInitialize = I('function() { this.setValue(""); }')
                                                          )), 
                                            splitLayout(
                                              dateInput(ns("srt_date"), html_req("Test Date"), value = as.Date(NA)), 
                                              selectInput(ns("flow_data_rec"), "Flow Data Recorded", 
                                                          choices = c("","Yes" = "1", "No" = "0"), selected = NULL)),
                                            splitLayout(
                                              selectInput(ns("con_phase"), html_req("Construction Phase"), 
                                                          choices = c("", con_phase$phase), selected = NULL),
                                              selectInput(ns("water_level_rec"), "Water Level Recorded", 
                                                          choices = c("","Yes" = "1", "No" = "0"), selected = NULL)),
                                            splitLayout(
                                              selectInput(ns("srt_type"), html_req("SRT Type"), 
                                                          choices = c("", srt_types$type), selected = NULL),
                                              selectInput(ns("photos_uploaded"), "Photos Uploaded", 
                                                          choices = c("","Yes" = "1", "No" = "0"), selected = NULL)), 
                                            splitLayout(
                                              numericInput(ns("test_volume"), "Test Volume (cf)",  value = NA, min = 0), 
                                              selectInput(ns("sensor_deployed"), html_req("Sensor Deployed"), 
                                                          choices = c("", "Yes" = "1", "No" = "0"), selected = NULL),
                                              disabled(dateInput(ns("sensor_collect_date"), "Sensor Collection Date", value = as.Date(NA)))),
                                            splitLayout(
                                              numericInput(ns("dcia"), "Impervious Drainage Area (sf)", value = NA),
                                              selectInput(ns("qaqc_complete"), "QA/QC Complete", 
                                                          choices = c("","Yes" = "1", "No" = "0"), selected = NULL)),
                                            splitLayout(
                                              disabled(numericInput(ns("storm_size"), "Simulated Depth (in)",  value = NA, min = 0)), 
                                              dateInput(ns("srt_summary_date"), "SRT Summary Report Sent", value = as.Date(NA))),
                                            conditionalPanel(condition = "input.srt_date === null", 
                                                             ns = ns, 
                                                             selectInput(ns("priority"), "Future SRT Priority", 
                                                                         choices = c("", priority$field_test_priority), selected = NULL)),
                                            # actionButton(ns("browserButton"),"Browser click"),
                                            textAreaInput(ns("srt_summary"), "Notes", height = '85px'), 
                                            conditionalPanel(condition = "input.srt_date === null", 
                                                             ns = ns, 
                                                             actionButton(ns("future_srt"), "Add Future SRT"), 
                                                             actionButton(ns("delete_future_srt"), "Delete Future SRT")),
                                            actionButton(ns("add_srt"), "Add SRT"),
                                            actionButton(ns("clear_srt"), "Clear All Fields"),
                                            fluidRow(
                                              HTML(paste(html_req(""), " indicates required field for complete tests. ", 
                                                         future_req(""), " indicates required field for future tests."))), 
                                            tags$head(tags$style(HTML("
                                                                        .shiny-split-layout > div {
                                                                          overflow: visible;
                                                                        }
                                                                      ")))
                                                                        )
                        ),
                        #1.1.2 Tables -----
                        column(width = 7,
                               conditionalPanel(condition = "input.system_id", 
                                                ns = ns, 
                                                 h4(textOutput(ns("future_system_header"))), 
                                                 DTOutput(ns("future_srt_table")),
                                                 h4(textOutput(ns("system_header"))), 
                                                 DTOutput(ns("srt_table"))
                               )
                        )
                      )
             ),
             #1.2 View SRTs -----
             tabPanel("View SRTs", value = "view_SRT", 
                      titlePanel("All Simulated Runoff Tests"),
                      reactableOutput(ns("all_srt_table"))
             ), 
             #1.3 View Future ------
             tabPanel("View Future SRTs", value = "view_future_SRT", 
                      titlePanel("All Future Simulated Runoff Tests"), 
                      reactableOutput(ns("all_future_srt_table")))
  )
}

#2.0 Server -------
SRTServer <- function(id, parent_session, poolConn, srt_types, con_phase, sys_id, special_char_replace){
  
  moduleServer(
    id,
    function(input, output, session){
  
      #2.0.1 Set up ----
      #define ns to use in modals
      ns <- session$ns
      
      #updates system ids
      updateSelectizeInput(session, "system_id", choices = sys_id, selected = character(0), server = TRUE)
      
      #use reactive values to read in table, and see which tests already exist at the system
      rv <- reactiveValues()
      
      #2.0.2 Tab Name ----
      tab_name <- "SRT Tab"
      
      #2.1 Add/Edit ---------
      #2.1.1 Headers ---------
      
      #Get the Project name, combine it with System ID, and create a reactive header
      rv$sys_and_name_step <- reactive(odbc::dbGetQuery(poolConn, paste0("select system_id, project_name from fieldwork.viw_project_names where system_id = '", input$system_id, "'")))
      
      rv$sys_and_name <- reactive(paste(rv$sys_and_name_step()$system_id[1], rv$sys_and_name_step()$project_name[1]))
      
      output$system_header <- renderText(
        paste("SRTs at", rv$sys_and_name())
      )
      
      output$future_system_header <- renderText(
        paste("Future SRTs at", rv$sys_and_name())
      )
      
      #2.1.2 Query and Display Tables -----
      srt_table_query <- reactive(paste0("SELECT * FROM fieldwork.viw_srt_full WHERE system_id = '", input$system_id, "'"))
      rv$srt_table_db <- reactive(odbc::dbGetQuery(poolConn, srt_table_query()))
      
      rv$srt_table <- reactive(rv$srt_table_db() %>% 
                                 mutate("test_date" = as.character(test_date), 
                                        "srt_stormsize_in" = round(srt_stormsize_in, 2)) %>% 
                                 dplyr::select("system_id", "test_date", "phase", "type", "srt_volume_ft3", "dcia_ft2", "srt_stormsize_in", "srt_summary"))
      
      output$srt_table <- renderDT(
        rv$srt_table(), 
        selection = 'single',
        style = 'bootstrap', 
        class = 'table-responsive, table-hover', 
        colnames = c('System ID', 'Test Date', 'Phase', 'Type', 'Volume (cf)', 'DCIA (sf)', 'Simulated Depth (in)', 'Results Summary') 
      )
      
      #future table 
      future_srt_table_query <- reactive(paste0("SELECT * FROM fieldwork.viw_future_srt_full WHERE system_id = '", input$system_id, "' order by field_test_priority_lookup_uid"))
      rv$future_srt_table_db <- reactive(odbc::dbGetQuery(poolConn, future_srt_table_query()))
      
      rv$future_srt_table <- reactive(rv$future_srt_table_db() %>% 
                                        dplyr::select("system_id", "phase", "type", "dcia_ft2", "field_test_priority", "notes"))
      
      output$future_srt_table <- renderDT(
        rv$future_srt_table(), 
        selection = 'single', 
        style = 'bootstrap', 
        class = 'table-responsive, table-hover', 
        colnames = c('System ID', 'Phase', 'Type', 'DCIA (sf)', 'Priority', 'Notes')
      )
      
      #2.1.3 Prepare Inputs ---------
      rv$type <- reactive(srt_types %>% dplyr::filter(type == input$srt_type) %>% 
                            select(srt_type_lookup_uid) %>% pull())
      
      rv$type_null <- reactive(if(nchar(input$srt_type) == 0) "NULL" else paste0("'", rv$type(), "'"))
      
      rv$phase <- reactive(con_phase %>% dplyr::filter(phase == input$con_phase) %>% 
                             select(con_phase_lookup_uid) %>% pull())
      
      rv$phase_null <- reactive(if(nchar(input$con_phase) == 0) "NULL" else paste0("'", rv$phase(), "'"))
      
      rv$priority_lookup_uid_query <- reactive(paste0("select field_test_priority_lookup_uid from fieldwork.tbl_field_test_priority_lookup where field_test_priority = '", input$priority, "'"))
      rv$priority_lookup_uid_step <- reactive(dbGetQuery(poolConn, rv$priority_lookup_uid_query()))
      rv$priority_lookup_uid <- reactive(if(nchar(input$priority) == 0) "NULL" else paste0("'", rv$priority_lookup_uid_step(), "'"))
      
      #set inputs to reactive values so "NULL" can be entered
      #important to correctly place quotations
      rv$test_volume <- reactive(if(is.na(input$test_volume)) "NULL" else paste0("'", input$test_volume, "'"))
      rv$dcia_write <- reactive(if(is.na(input$dcia)) "NULL" else paste0("'", input$dcia, "'"))
      rv$storm_size <- reactive(if(is.na(input$storm_size)) "NULL" else paste0("'", input$storm_size, "'"))
      rv$srt_summary_step <- reactive(gsub('\'', '\'\'', input$srt_summary))
      rv$srt_summary_step_two <- reactive(special_char_replace(rv$srt_summary_step()))
      rv$srt_summary <- reactive(if(nchar(rv$srt_summary_step_two()) == 0) "NULL" else paste0("'", rv$srt_summary_step_two(), "'"))
      
      rv$flow_data_rec <- reactive(if(nchar(input$flow_data_rec) == 0 | input$flow_data_rec == "N/A") "NULL" else paste0("'", input$flow_data_rec, "'"))
      rv$water_level_rec <- reactive(if(nchar(input$water_level_rec) == 0 | input$water_level_rec == "N/A") "NULL" else paste0("'", input$water_level_rec, "'"))
      rv$photos_uploaded <- reactive(if(nchar(input$photos_uploaded) == 0 | input$photos_uploaded == "N/A") "NULL" else paste0("'", input$photos_uploaded, "'"))
      rv$sensor_collect_date <- reactive(if(length(input$sensor_collect_date) == 0) "NULL" else paste0("'", input$sensor_collect_date, "'"))
      rv$qaqc_complete <- reactive(if(nchar(input$qaqc_complete) == 0 | input$qaqc_complete == "N/A") "NULL" else paste0("'", input$qaqc_complete, "'"))
      rv$srt_summary_date <- reactive(if(length(input$srt_summary_date) == 0) "NULL" else paste0("'", input$srt_summary_date, "'"))
      rv$sensor_deployed <- reactive(if(nchar(input$sensor_deployed) == 0 | input$sensor_deployed == "N/A") "NULL" else paste0("'", input$sensor_deployed, "'"))
      
      #assure that the summary date does not precede the srt date
      #this ws causing summary date and collection date to not populate when changing the table selection from a newer to an older SRT
      #solved by adding a second min = argument when populating these values from the srt table in the observeEvent for SRT row selected 
      observeEvent(input$srt_date,{
        updateDateInput(session, "srt_summary_date", min = input$srt_date)
      }
                  )

      observeEvent(input$srt_date,{
        updateDateInput(session, "sensor_collect_date", min = input$srt_date)
      }
                   )
      
      #2.1.4 Toggle states depending on inputs -----
      #toggle state (enable/disable) buttons based on whether system id, test date, and srt type are selected (this is shinyjs)
      observe(toggleState(id = "add_srt", condition = nchar(input$system_id) > 0 & length(input$srt_date) > 0 &
                            nchar(input$srt_type) >0 & nchar(input$con_phase) > 0 & nchar(input$sensor_deployed) > 0))
      
      #toggle state for future srt
      observe(toggleState(id = "future_srt", condition = nchar(input$system_id) > 0))
      
      #toggle future deployment delete button
      observe(toggleState(id = "delete_future_srt", condition = length(input$future_srt_table_rows_selected) != 0))
      
      #toggle state for metadata depending on whether a test date is included
      observe(toggleState(id = "flow_data_rec", condition = length(input$srt_date) > 0))
      observe(toggleState(id = "water_level_rec", condition = length(input$srt_date) > 0))
      observe(toggleState(id = "photos_uploaded", condition = length(input$srt_date) > 0))
      observe(toggleState(id = "test_volume", condition = length(input$srt_date) > 0))
      observe(toggleState(id = "sensor_deployed", condition = length(input$srt_date) > 0))
      observe(toggleState(id = "sensor_collect_date", condition = input$sensor_deployed == "1"))
      observe(toggleState(id = "qaqc_complete", condition = length(input$srt_date) > 0))
      observe(toggleState(id = "srt_summary_date", condition = length(input$srt_date) > 0))
      
      #update button names
      rv$label <- reactive(if(length(input$srt_table_rows_selected) == 0) "Add New" else "Edit Selected")
      observe(updateActionButton(session, "add_srt", label = rv$label()))
      
      rv$future_label <- reactive(if(length(input$future_srt_table_rows_selected) == 0) "Add Future SRT" else "Edit Selected Future SRT")
      observe(updateActionButton(session, "future_srt", label = rv$future_label()))
      
      rv$summary_label <- reactive(if(length(input$srt_date) == 0) "Notes" else "Results Summary")
      observe(updateTextAreaInput(session, "srt_summary", rv$summary_label()))
      
      #2.1.5 do the math -------
      #update Impervous Drainage Area
      srt_dcia_query <- reactive(paste0("SELECT sys_impervda_ft2 FROM external.tbl_systembdv WHERE 
                                        system_id = '", input$system_id, "'"))
      rv$dcia_x <- reactive(odbc::dbGetQuery(poolConn, srt_dcia_query()) %>% dplyr::pull())
      rv$dcia <- reactive(as.numeric(rv$dcia_x()))
      observe(updateNumericInput(session, "dcia", value = rv$dcia()))
      
      #update Equivalent Storm Size 
      #DCIA (ft2) * 1/12 (ft/in)
      rv$one_inch_storm_vol_cf <- reactive(rv$dcia()*1/12)
      rv$eq_storm_size_in <- reactive(input$test_volume/rv$one_inch_storm_vol_cf())
      observe(updateNumericInput(session, "storm_size", value = rv$eq_storm_size_in()))
      
      #2.1.6 Editing on clicks ---------
      #future table
      observeEvent(input$future_srt_table_rows_selected, {
        #deselect from other table
        dataTableProxy('srt_table') %>% selectRows(NULL)
        
        #update inputs
        
        #update to values from selected row
        updateSelectInput(session, "con_phase", selected = rv$future_srt_table()$phase[input$future_srt_table_rows_selected])
        updateSelectInput(session, "srt_type", selected = rv$future_srt_table()$type[input$future_srt_table_rows_selected])
        updateSelectInput(session, "priority", selected = rv$future_srt_table()$field_test_priority[input$future_srt_table_rows_selected])
        updateTextAreaInput(session, "srt_summary", value = rv$future_srt_table()$notes[input$future_srt_table_rows_selected])
    
        reset("test_volume")
        reset("srt_date")
        reset("flow_data_rec")
        reset("water_level_rec")
        reset("photos_uploaded")
        reset("sensor_collect_date")
        reset("qaqc_complete")
        reset("srt_summary_date")
        reset("sensor_deployed")
      })
      
      # # browserButton
      # observeEvent(input$browserButton,{
      #   browser()
      # })
      
      #current table
      observeEvent(input$srt_table_rows_selected,{
        # print(paste0("The following row is selected: ",input$srt_table_rows_selected,". Sensor Collection Date - ",rv$srt_table_db()$sensor_collection_date[input$srt_table_rows_selected],". SRT Summary Report Sent - ",rv$srt_table_db()$srt_summary_date[input$srt_table_rows_selected]))
        dataTableProxy('future_srt_table') %>% selectRows(NULL)
        #rv_ow$fac <- (rv$srt_table()[input$srt_table_rows_selected, 4])
        updateDateInput(session, "srt_date", value = rv$srt_table()$test_date[input$srt_table_rows_selected])
        
        #update to values from selected row
        updateSelectInput(session, "con_phase", selected = rv$srt_table_db()$phase[input$srt_table_rows_selected])
        updateSelectInput(session, "srt_type", selected = rv$srt_table_db()$type[input$srt_table_rows_selected])
        updateNumericInput(session, "test_volume", value = rv$srt_table()$srt_volume_ft3[input$srt_table_rows_selected])
        #updateNumericInput(session, "storm_size", value = rv$srt_table()[input$srt_table_rows_selected, 7])
        updateTextAreaInput(session, "srt_summary", value = rv$srt_table_db()$srt_summary[input$srt_table_rows_selected])
        
        #update metadata values
        updateSelectInput(session, "flow_data_rec", selected = as.numeric(rv$srt_table_db()$flow_data_recorded[input$srt_table_rows_selected]))
        updateSelectInput(session, "water_level_rec", selected = as.numeric(rv$srt_table_db()$water_level_recorded[input$srt_table_rows_selected]))
        updateSelectInput(session, "photos_uploaded", selected = as.numeric(rv$srt_table_db()$photos_uploaded[input$srt_table_rows_selected]))
        updateDateInput(session, "sensor_collect_date", value = rv$srt_table_db()$sensor_collection_date[input$srt_table_rows_selected],
                                                        min = rv$srt_table()$test_date[input$srt_table_rows_selected])
        updateSelectInput(session, "qaqc_complete", selected = as.numeric(rv$srt_table_db()$qaqc_complete[input$srt_table_rows_selected]))
        updateDateInput(session, "srt_summary_date", value = rv$srt_table_db()$srt_summary_date[input$srt_table_rows_selected],
                                                     min = rv$srt_table()$test_date[input$srt_table_rows_selected])
        updateSelectInput(session, "sensor_deployed", selected = as.numeric(rv$srt_table_db()$sensor_deployed[input$srt_table_rows_selected]))
        
      })
      
      #2.1.7 Add/Edit/Clear buttons-------
      #future table
      observeEvent(input$future_srt, {
        if(length(input$future_srt_table_rows_selected) == 0){
          add_future_srt_query <- paste0("INSERT INTO fieldwork.tbl_future_srt (system_id, con_phase_lookup_uid, srt_type_lookup_uid, 
                                         dcia_ft2, notes, field_test_priority_lookup_uid)
                                         VALUES ('", input$system_id, "', ", rv$phase_null(), ", ", rv$type_null(), ", ", rv$dcia_write(), ", ", 
                                         iconv(rv$srt_summary(), "latin1", "ASCII", sub=""), #Strip unicode characters that WIN1252 encoding will choke on locally
                                         ", ", rv$priority_lookup_uid(), ")")
          
          odbc::dbGetQuery(poolConn, add_future_srt_query)
          
          # log the INSERT query, see utils.R
          insert.query.log(poolConn,
                           add_future_srt_query,
                           tab_name,
                           session)
          
        }else{
          edit_future_srt_query <- paste0("UPDATE fieldwork.tbl_future_srt SET con_phase_lookup_uid = ", rv$phase_null(), ", 
                                          srt_type_lookup_uid = ", rv$type_null(), ", 
                                          dcia_ft2 = ", rv$dcia_write(), ", 
                                          notes = ", iconv(rv$srt_summary(), "latin1", "ASCII", sub=""), ",
                                          field_test_priority_lookup_uid = ", rv$priority_lookup_uid(), "
                                          WHERE future_srt_uid = '", rv$future_srt_table_db()[input$future_srt_table_rows_selected, 1], "'")
          
          odbc::dbGetQuery(poolConn, edit_future_srt_query)
          
          # log the UPDATE query, see utils.R
          insert.query.log(poolConn,
                           edit_future_srt_query,
                           tab_name,
                           session)
          
        }
        
        rv$future_srt_table_db <- reactive(odbc::dbGetQuery(poolConn, future_srt_table_query()))
        rv$all_future_srt_table_db <- odbc::dbGetQuery(poolConn, all_future_srt_table_query)
        reset("srt_date")
        reset("con_phase")
        reset("srt_type")
        reset("test_volume")
        reset("storm_size")
        reset("priority")
        reset("srt_summary")
        reset("flow_data_rec")
        reset("water_level_rec")
        reset("photos_uploaded")
        reset("sensor_deployed")
        reset("sensor_collect_date")
        reset("qaqc_complete")
        reset("srt_summary_date")
      })
      
      #when button is clicked
      #add to srt table
      #use the MAX(srt_uid) from srt table to get the SRT UID of the most recent addition to the table (calculated by SERIAL), which is the current addition
      observeEvent(input$add_srt, {
        if(length(input$srt_table_rows_selected) == 0){
          add_srt_query <- paste0("INSERT INTO fieldwork.tbl_srt (system_id, test_date, con_phase_lookup_uid, srt_type_lookup_uid, 
                          srt_volume_ft3, dcia_ft2, srt_stormsize_in, srt_summary, flow_data_recorded, water_level_recorded, photos_uploaded, 
                                  sensor_collection_date, qaqc_complete, srt_summary_date, sensor_deployed) 
      	                  VALUES ('", input$system_id, "','", input$srt_date, "','", rv$phase(), "', ",  rv$type(), ",", rv$test_volume(), ",", 
                                  rv$dcia_write(), ", ", rv$storm_size(), ",", 
                                  iconv(rv$srt_summary(), "latin1", "ASCII", sub=""), #Strip unicode characters that WIN1252 encoding will choke on locally
                                                                                      #This is dumb. 
                                  ", ", rv$flow_data_rec(), ",", rv$water_level_rec(), ",",  
                                  rv$photos_uploaded(), ",", rv$sensor_collect_date(), ",", rv$qaqc_complete(), ",", 
                                  rv$srt_summary_date(), ", ", rv$sensor_deployed(), ")")
          
          odbc::dbGetQuery(poolConn, add_srt_query)
          
          # log the INSERT query, see utils.R
          insert.query.log(poolConn,
                           add_srt_query,
                           tab_name,
                           session)
          
          #else update srt table
        }else{
          edit_srt_query <- paste0(
            "UPDATE fieldwork.tbl_srt SET system_id = '", input$system_id, "', test_date = '", input$srt_date, 
            "', con_phase_lookup_uid = '", rv$phase(),
            "', srt_type_lookup_uid = '",  rv$type(),
            "', srt_volume_ft3 = ", rv$test_volume(),
            ", dcia_ft2 = " , rv$dcia_write(),
            ", srt_stormsize_in = ", rv$storm_size(), 
            ", srt_summary = ", iconv(rv$srt_summary(), "latin1", "ASCII", sub=""), 
            ", flow_data_recorded = ", rv$flow_data_rec(), 
            ", water_level_recorded = ", rv$water_level_rec(), 
            ", photos_uploaded = ", rv$photos_uploaded(), 
            ", sensor_collection_date = ", rv$sensor_collect_date(),
            ", qaqc_complete = ", rv$qaqc_complete(),
            ", srt_summary_date = ", rv$srt_summary_date(), 
            ", sensor_deployed = ", rv$sensor_deployed(), "
            WHERE srt_uid = '", rv$srt_table_db()[input$srt_table_rows_selected, 1], "'")
          
           dbGetQuery(poolConn, edit_srt_query)
           
           # log the UPDATE query, see utils.R
           insert.query.log(poolConn,
                            edit_srt_query,
                            tab_name,
                            session)
        }
        
        #if editing a future test to become a completed test, delete the future test
        if(length(input$future_srt_table_rows_selected) > 0){
          del_future_srt_query <-  paste0("DELETE FROM fieldwork.tbl_future_srt 
                                          WHERE future_srt_uid = '",
                                          rv$future_srt_table_db()[input$future_srt_table_rows_selected, 1], "'")
          
          odbc::dbGetQuery(poolConn,del_future_srt_query)
          
          # log the UPDATE query, see utils.R
          insert.query.log(poolConn,
                           del_future_srt_query,
                           tab_name,
                           session)
        }
        
        #check if a deployment exists for this test (if a sensor was used). If it does not, bring up a dialogue box asking the user
        #if they would like to create a deployment. If yes, that takes them to the deployment page
        srt_deployment_exists_query <- paste0("select * from fieldwork.viw_deployment_full where term = 'SRT' and admin.fun_smp_to_system(smp_id) = '", input$system_id, "' and deployment_dtime_est < '", input$srt_date, "'::timestamp + interval '3 days' and deployment_dtime_est > '", input$srt_date, "'::timestamp - interval '3 days'")
        
        srt_deployment_exists_table <- dbGetQuery(poolConn, srt_deployment_exists_query)
        
        srt_deployment_exists <- nrow(srt_deployment_exists_table) > 0
        
        if(srt_deployment_exists == FALSE & input$sensor_deployed == 1){
        showModal(modalDialog(title = "Deploy Sensor", 
                              "Would you like to add a sensor deployment for this SRT?", 
                              modalButton("No"), 
                              actionButton(ns("add_deployment"), "Yes")))
        }
        
        #update srt_table with new srt
        rv$srt_table_db <- reactive(odbc::dbGetQuery(poolConn, srt_table_query()))
        
        #update srt view with new/edited srt
        rv$all_srt_table_db <- reactive(dbGetQuery(poolConn, all_srt_table_query))
        
        #update future srt table in case a future srt was delisted 
        rv$future_srt_table_db <- reactive(odbc::dbGetQuery(poolConn, future_srt_table_query()))
        rv$all_future_srt_table_db <- odbc::dbGetQuery(poolConn, all_future_srt_table_query)
        
        #clear contents aside from System ID
        reset("srt_date")
        reset("con_phase")
        reset("srt_type")
        reset("test_volume")
        reset("storm_size")
        reset("srt_summary")
        reset("flow_data_rec")
        reset("water_level_rec")
        reset("photos_uploaded")
        reset("sensor_deployed")
        reset("sensor_collect_date")
        reset("qaqc_complete")
        reset("srt_summary_date")
        reset("priority")
      })
      
      #delete a future srt
      #first, intermediate dialog box
      observeEvent(input$delete_future_srt, {
        showModal(modalDialog(title = "Delete Future SRT", 
                              "Delete Future SRT?", 
                              modalButton("No"), 
                              actionButton(ns("confirm_delete_future"), "Yes")))
      })
      
      observeEvent(input$confirm_delete_future, {
        
        del_future_srt_query <- paste0("DELETE FROM fieldwork.tbl_future_srt WHERE future_srt_uid = '",
                                        rv$future_srt_table_db()[input$future_srt_table_rows_selected, 1], "'")
        
        odbc::dbGetQuery(poolConn, del_future_srt_query)
        
        # log the UPDATE query, see utils.R
        insert.query.log(poolConn,
                         del_future_srt_query,
                         tab_name,
                         session)
        
        #update future srt table
        rv$future_srt_table_db <- reactive(odbc::dbGetQuery(poolConn, future_srt_table_query()))
        rv$all_future_srt_table_db <- odbc::dbGetQuery(poolConn, all_future_srt_table_query)
        
        #remove pop up
        removeModal()
      })
      
      
      rv$refresh_deploy <- 0 
      
      observeEvent(input$add_deployment, {
        rv$refresh_deploy <- rv$refresh_deploy + 1
        updateTabsetPanel(session = parent_session, "inTabset", selected = "deploy_tab")
        removeModal()
      })
      
      observeEvent(input$clear_srt, {
        showModal(modalDialog(title = "Clear All Fields", 
                              "Are you sure you want to clear all fields on this tab?", 
                              modalButton("No"), 
                              actionButton(ns("confirm_clear_srt"), "Yes")))
      })
      
      observeEvent(input$confirm_clear_srt, {
        reset("srt_title")
        reset("system_id")
        reset("srt_date")
        reset("con_phase")
        reset("srt_type")
        reset("dcia")
        reset("test_volume")
        reset("storm_size")
        reset("srt_summary")
        reset("flow_data_rec")
        reset("water_level_rec")
        reset("photos_uploaded")
        reset("sensor_deployed")
        reset("sensor_collect_date")
        reset("qaqc_complete")
        reset("srt_summary_date")
        removeModal()
      })
      
      
      #2.2 View SRTs ---------
      
      #2.2.1 query and display table -------
      all_srt_table_query <- "SELECT * FROM fieldwork.viw_srt_full ORDER BY test_date DESC"
      rv$all_srt_table_db <- reactive(dbGetQuery(poolConn, all_srt_table_query))
      
      #convert 1s and 0s to yes and no, make dates characters so they show properly, and round storm size
      rv$all_srt_table <- reactive(rv$all_srt_table_db() %>% 
                                     mutate(across(c("test_date", "srt_summary_date", "sensor_collection_date"), as.character)) %>% 
                                     mutate("srt_stormsize_in" = round(srt_stormsize_in, 2)) %>% 
                                     mutate(across(c("flow_data_recorded", "water_level_recorded", "photos_uploaded", "qaqc_complete"), 
                                                ~ case_when(. == 1 ~ "Yes", 
                                                              . == 0 ~ "No"))) %>% 
                                     dplyr::select("system_id", "project_name", "test_date", "phase", "type", "srt_volume_ft3",
                                                   "dcia_ft2", "srt_stormsize_in", "flow_data_recorded", "water_level_recorded",
                                                   "photos_uploaded", "sensor_collection_date", "qaqc_complete",
                                                   "srt_summary_date", "turnaround_days", "srt_summary", "sensor_deployed"))
      
      output$all_srt_table <- renderReactable(
        reactable(rv$all_srt_table()[, 1:15], 
                  columns = list(
                    #srt_uid = colDef(name = "SRT UID"),
                    system_id  = colDef(name = "System ID"),
                    project_name = colDef(name = "Project Name"),
                    test_date  = colDef(name = "Test Date"),
                    phase = colDef(name = "Phase"),
                    type = colDef(name = "Type"),
                    srt_volume_ft3  = colDef(name = "Volume (cf)"),
                    dcia_ft2  = colDef(name = "DCIA (sf)"),
                    srt_stormsize_in = colDef(name = "Simulated Depth (in)"),
                    flow_data_recorded = colDef(name = "Flow Data Recorded", style = function(value){
                      if(is.na(value)){
                        color = "#CCC916"
                        textColor = "#000000"
                      }else{
                        color = "#272B30"
                        textColor = "#c8c8c8"
                      }
                      list(backgroundColor = color, color = textColor, fontweight = "bold")
                    }),
                    water_level_recorded = colDef(name = "Water Level Recorded", style = function(value){
                      if(is.na(value)){
                        color = "#CCC916"
                        textColor = "#000000"
                      }else{
                        color = "#272B30"
                        textColor = "#c8c8c8"
                      }
                      list(backgroundColor = color, color = textColor, fontweight = "bold")
                    }),
                    photos_uploaded = colDef(name = "Photos Uploaded", style = function(value){
                      if(is.na(value) | value == "No"){
                        color = "#CCC916"
                        textColor = "#000000"
                      }else{
                        color = "#272B30"
                        textColor = "#c8c8c8"
                      }
                      list(backgroundColor = color, color = textColor, fontweight = "bold")
                    }),
                    sensor_collection_date  = colDef(name = "Sensor Collection Date", style = function(value, index){
                      if(is.na(value) & 
                         rv$all_srt_table()$sensor_deployed[index] == "1"){
                        color = "#CCC916"
                        textColor = "#000000"
                      }else{
                        color = "#272B30"
                        textColor = "#c8c8c8"
                      }
                      list(backgroundColor = color, color = textColor, fontweight = "bold")
                    }),
                    qaqc_complete = colDef(name = "QA/QC Complete", style = function(value, index){
                      if((is.na(value) | value == "No") & 
                         (is.na(rv$all_srt_table()$flow_data_recorded[index]) |
                          rv$all_srt_table()$flow_data_record[index] == "Yes")){
                        color = "#CCC916"
                        textColor = "#000000"
                      }else{
                        color = "#272B30"
                        textColor = "#c8c8c8"
                      }
                      list(backgroundColor = color, color = textColor, fontweight = "bold")
                    }),
                    srt_summary_date = colDef(name = "Summary Date", style = function(value){
                      if(is.na(value)){
                        color = "#CCC916"
                        textColor = "#000000"
                      }else{
                        color = "#272B30"
                        textColor = "#c8c8c8"
                      }
                      list(backgroundColor = color, color = textColor, fontweight = "bold")
                    }),
                    turnaround_days = colDef(name = "Turnaround (days)")
                  ),
                  fullWidth = TRUE,
                  selection = "single",
                  searchable = TRUE,
                  onClick = "select",
                  selectionId = ns("srt_selected"),
                  #searchable = TRUE,
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = c(10, 25, 50),
                  defaultPageSize = 10,
                  height = 750,
                  style = reactableTheme(color = "#c8c8c8",
                                         backgroundColor = "#272B30"),
                 details = function(index){
                   nest_table <- rv$all_srt_table()[rv$all_srt_table_db()$srt_uid == rv$all_srt_table_db()$srt_uid[index], ][16]
                   htmltools::div(style = "padding:16px",
                                  reactable(nest_table,
                                            columns = list(srt_summary = colDef(name = "Results Summary")),
                                                           style = reactableTheme(color = "#c8c8c8",
                                                                                  backgroundColor = "#272B30"))
                   )
                 }
        )
      )
      
      #2.2.2 click a row ------
      #click a row in the all srt table, switch tabs, and select the correct SMP ID, then select the correct test
      observeEvent(input$srt_selected, {
        updateSelectizeInput(session, "system_id", choices = sys_id, 
                             selected = rv$all_srt_table()$system_id[input$srt_selected], 
                             server = TRUE)
        updateTabsetPanel(session = parent_session, "inTabset", selected = "srt_tab")
        updateReactable("all_future_srt_table", selected = NA)
        #delay so that the selectizeInput is updated and table is quereied before it is searched by R
        #basically make sure things happen in the right order
        #delay time based on trail and error
        delay(300,{
                srt_row <- which(rv$srt_table_db()$srt_uid == rv$all_srt_table_db()$srt_uid[input$srt_selected], arr.ind = TRUE)
                dataTableProxy('srt_table') %>% selectRows(srt_row)
              }
        )
      })
      
      #2.3 Future SRTs tab ------
      #2.3.1 query and display table -----
      all_future_srt_table_query <- "select * from fieldwork.viw_future_srt_full order by field_test_priority_lookup_uid"
      rv$all_future_srt_table_db <- odbc::dbGetQuery(poolConn, all_future_srt_table_query)
      
      rv$all_future_srt_table <- reactive(rv$all_future_srt_table_db %>% 
                                            mutate(across(c("sys_storagevolume_ft3", 
                                                            "sys_rawstormsizemanaged_in", 
                                                            "one_inch_storm_volume_cf"), ~ round(., 2))) %>% 
                                        dplyr::select("system_id", "project_name", "phase", "type", "dcia_ft2", "sys_storagevolume_ft3", "sys_rawstormsizemanaged_in", "one_inch_storm_volume_cf", "field_test_priority", "notes"))
      
      output$all_future_srt_table <- renderReactable(
        reactable(rv$all_future_srt_table()[, 1:9], 
                  columns = list(
                    #future_srt_uid = colDef(name = "UID"),
                    system_id  = colDef(name = "System ID"),
                    project_name = colDef(name = "Project Name"),
                    phase = colDef(name = "Phase"),
                    type = colDef(name = "Type"),
                    dcia_ft2  = colDef(name = "DCIA (sf)"),
                    sys_storagevolume_ft3 = colDef(name = "Storage Volume (cf)"), 
                    sys_rawstormsizemanaged_in = colDef(name = "Depth Managed (in)"),
                    one_inch_storm_volume_cf = colDef(name = "1\" Storm Volume (cf)"),
                    field_test_priority = colDef(name = "Priority")
                  ),
                  fullWidth = TRUE,
                  selection = "single",
                  searchable = TRUE,
                  onClick = "select",
                  selectionId = ns("future_srt_selected"),
                  #searchable = TRUE,
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = c(10, 25, 50),
                  defaultPageSize = 10,
                  height = 750,
                  style = reactableTheme(color = "#c8c8c8",
                                         backgroundColor = "#272B30"),
                  details = function(index){
                    nest <- rv$all_future_srt_table()[rv$all_future_srt_table_db$future_srt_uid == rv$all_future_srt_table_db$future_srt_uid[index], ][10]
                    htmltools::div(style = "padding:16px",
                                   reactable(nest,
                                             columns = list(notes = colDef(name = "Notes")),
                                             style = reactableTheme(color = "#c8c8c8",
                                                                    backgroundColor = "#272B30"))
                    )
                  }
        ))
      
      #2.3.2 on click ----
      observeEvent(input$future_srt_selected, {
        updateSelectizeInput(session, "system_id", choices = sys_id,
                             selected = rv$all_future_srt_table()$system_id[input$future_srt_selected], 
                             server = TRUE)
        updateTabsetPanel(session = parent_session, "inTabset", selected = "srt_tab")
        updateReactable("all_srt_table", selected = NA)
        #delay so that the selectizeInput is updated and table is queried before it is searched by R
        #delay time based on trail and error
        #thank you dean attali
        delay(300, {
              future_srt_row <- which(rv$future_srt_table_db()$future_srt_uid == rv$all_future_srt_table_db$future_srt_uid[input$future_srt_selected], arr.ind = TRUE)
              dataTableProxy('future_srt_table') %>% selectRows(future_srt_row)
        }
        )
      })
      
      #2.4 return values ----
      return(
        list(
          refresh_deploy = reactive(rv$refresh_deploy),
          system_id = reactive(input$system_id)
        )
      )
      
    }
  )
}