#Special Investigation Module
#3 tabs: View future SI, past SI, add/edit SIs by site

#1.0 UI _-----

special_investigationsUI <- function(id, label = "special_investigations", 
                                     site_names, html_req, work_number, priority, con_phase, si_lookup, requested_by_lookup, future_req){
  ns <- NS(id)
  navbarMenu("Special Investigations", 
             #1.1 Add/Edit
             tabPanel("Add/Edit Special Investigation", value = "si_tab", 
                      titlePanel("Add/Edit Special Investigation"), 
                      #1.1.1 sidebarPanel ----
                      sidebarPanel(
                        #style = "overflow-y:scroll; overflow-x:hidden; max-height: 650px",
                         fluidRow(h5("Prioritize System ID, then Work Number, then Site Name. Only one is required")),
                        fluidRow(
                          column(4, selectizeInput(ns("system_id"), future_req(html_req("System ID")), 
                                                   choices = NULL, 
                                                   options = list(
                                                     placeholder = 'Select an Option',
                                                     onInitialize = I('function() { this.setValue(""); }')
                                                   ))), 
                          column(4, selectInput(ns("work_number"), future_req(html_req("Work Number")), 
                                                choices = c("", work_number), selected = NULL)),
                          column(4, selectInput(ns("site_name"), future_req(html_req("Site Name")), 
                                                choices = c("", site_names), selected = NULL))),
                        fluidRow(
                          column(6, dateInput(ns("date"), html_req("Test Date"), value = as.Date(NA))), 
                          column(6, selectInput(ns("con_phase"), "Construction Phase", 
                                                choices = c("", con_phase$phase), selected = NULL))),
                        fluidRow(
                          column(6, selectInput(ns("type"), future_req(html_req("Investigation Type")), 
                                                choices = c("", si_lookup$special_investigation_type), selected = NULL)), 
                          column(6, selectInput(ns("requested_by"), future_req(html_req("Requested By")), 
                                                choices = c("", requested_by_lookup$requested_by), selected = NULL))
                        ), 
                        fluidRow(
                          column(6, selectInput(ns("photos"), "Photos Uploaded", 
                                                choices = c("","Yes" = "1", "No" = "0"), selected = NULL)), 
                          column(6, selectInput(ns("sensor_deployed"), html_req("Sensor Deployed?"), 
                                                choices = c("","Yes" = "1", "No" = "0"), selected = NULL))
                        ),
                        conditionalPanel(condition = "input.sensor_deployed == 1", 
                                         ns = ns, 
                                        fluidRow(
                                          column(6, dateInput(ns("sensor_collect_date"), "Sensor Collection Date", value = as.Date(NA))), 
                                          column(6, selectInput(ns("qaqc_complete"), html_req("QA/QC Complete"), 
                                                                choices = c("","Yes" = "1", "No" = "0"), selected = NULL))
                                        )),
                        fluidRow(
                          column(6, selectInput(ns("summary_needed"), html_req("Summary Needed?"), choices = c("","Yes" = "1", "No" = "0"), selected = NULL)
                        ), 
                          column(6, dateInput(ns("summary_date"), "Summary Date", value = as.Date(NA)))),
                        conditionalPanel(condition = "input.date === null", 
                                         ns = ns, 
                                         selectInput(ns("priority"), "Future Test Priority", 
                                                     choices = c("", priority$field_test_priority), selected = NULL)),
                        textAreaInput(ns("notes"), "Notes", height = '90px'), 
                        conditionalPanel(condition = "input.date === null", 
                                         ns = ns, 
                                         actionButton(ns("future_test"), "Add Future Special Investigation"), 
                                         actionButton(ns("delete_future_test"), "Delete Future SI")),
                        actionButton(ns("add_test"), "Add Special Investigation"), 
                        actionButton(ns("clear"), "Clear All Fields"),
                        fluidRow(
                        HTML(paste(html_req(""), " indicates required field for complete tests. ", future_req(""), " indicates required field for future tests.")))
                      ),
                      #1.1.2 tables ----
                      mainPanel(
                        conditionalPanel(condition = "input.system_id || input.work_number || input.site_name", 
                                         ns  = ns, 
                                         h4(textOutput(ns("future_header"))), #("Capture Efficiency Tests at this SMP"), 
                                         DTOutput(ns("future_si_table")),
                                         h4(textOutput(ns("header"))),
                                         DTOutput(ns("si_table"))),
                      )
             ), 
             #1.2 View --------
             tabPanel("View Special Investigations", value = ns("view_si"), 
                      titlePanel("All Special Investigations"), 
                      reactableOutput(ns("all_si_table"))
             ),
             #1.3 View Future ------
             tabPanel("View Future Special Investigations", value = ns("view_future_si"), 
                      titlePanel("All Future Special Investigations"), 
                      reactableOutput(ns("all_future_si_table")))
  )
}

#2.0 server ------
special_investigationsServer <- function(id, parent_session, poolConn, con_phase, si_lookup, requested_by_lookup, sys_id, special_char_replace){
  
  moduleServer(
    id,
    function(input, output, session){
  
      #2.0.1 set up ----------
      #define ns to use in modals 
      ns <- session$ns
      
      #updates system ids
      updateSelectizeInput(session, "system_id", choices = sys_id, selected = character(0), server = TRUE)
      
      rv <- reactiveValues()
      
      #2.0.2 Tab Name ----
      tab_name <- "Special Investigations Tab"
      
      #2.1 Add/Edit -----
      #2.1.0 Root ------
      #toggle work number/system id/site names so when one is selected, the others are disabled
      observe(toggleState("work_number", condition = nchar(input$system_id) == 0 & nchar(input$site_name) == 0))
      observe(toggleState("system_id", condition = nchar(input$work_number) == 0 & nchar(input$site_name) == 0))
      observe(toggleState("site_name", condition = nchar(input$system_id) == 0 & nchar(input$work_number) == 0 ))
      
      #2.1.1 Headers -----
      #Get the Project name, combine it with System ID, and create a reactive header
      rv$sys_and_name_step <- reactive(odbc::dbGetQuery(poolConn, paste0("select system_id, project_name from fieldwork.viw_project_names where system_id = '", input$system_id, "'")))
      
      rv$sys_and_name <- reactive(paste(rv$sys_and_name_step()$system_id[1], rv$sys_and_name_step()$project_name[1]))
      
      #Get project name from work number
      rv$worknumber_and_name_step <- reactive(odbc::dbGetQuery(poolConn, paste0("select worknumber, project_name from fieldwork.viw_project_names where worknumber = '", input$work_number, "'")))
      
      rv$worknumber_and_name <- reactive(paste(rv$worknumber_and_name_step()$worknumber[1],
                                               rv$worknumber_and_name_step()$project_name[1]))
      
      #site name
      rv$site_name <- reactive(input$site_name)
      
      #decide with combo to use in headers
      rv$header_input <- reactive(if(nchar(input$system_id) > 0){
        rv$sys_and_name()
      }else if(nchar(input$work_number) > 0){
        rv$worknumber_and_name()
      }else if(nchar(input$site_name)){
        rv$site_name()
      })
      
      #render headers
      output$header <- renderText(
        paste("Special Investigations at", rv$header_input())
      )
      
      output$future_header <- renderText(
        paste("Future Special Investigations at", rv$header_input())
      )
      
      #2.1.2 Toggle states based on inputs --------
      #toggle "add test" button so it is only active when certain fields are complete
      observe(toggleState("add_test", condition = (nchar(input$system_id) | nchar(input$work_number) > 0 | nchar(input$site_name) > 0) &
                            length(input$date) > 0 &
                            nchar(input$type) > 0 & nchar(input$requested_by) > 0 &
                            nchar(input$sensor_deployed) > 0 &
                            nchar(input$summary_needed) > 0)
      )
      
      observe(toggleState("future_test", condition = (nchar(input$system_id) | nchar(input$work_number) > 0 | nchar(input$site_name) > 0) &
                            length(input$date) == 0 &
                            (nchar(input$type) > 0 & nchar(input$requested_by) > 0)
      ))
      
      #toggle future deployment delete button
      observe(toggleState(id = "delete_future_test", condition = length(input$future_si_table_rows_selected) != 0))
      
      #toggle 'results fields' so they can only be filled when a test date is entered
      observe(toggleState("sensor_deployed", condition = length(input$date) > 0))
      observe(toggleState("qaqc_complete", condition = length(input$date) > 0))
      observe(toggleState("sensor_collect_date", condition = length(input$date) > 0))
      observe(toggleState("photos", condition = length(input$date) > 0))
      observe(toggleState("summary_needed", condition = length(input$date) > 0))
      observe(toggleState("summary_date", condition = length(input$date) > 0 & nchar(input$summary_needed) > 0))
      
      
      #2.1.3 Prepare Inputs --------
      #reset sensor collection and qa/c IF sensor deployed = NO
      observeEvent(input$sensor_deployed == "No", {
        reset("sensor_collect_date")
        reset("qaqc_complete")
      })
      
      observeEvent(input$summary_needed == "No", {
        reset("summary_date")
      })
      
      #lookup priority uid
      rv$priority_lookup_uid_query <- reactive(paste0("select field_test_priority_lookup_uid from fieldwork.tbl_field_test_priority_lookup where field_test_priority = '", input$priority, "'"))
      rv$priority_lookup_uid_step <- reactive(dbGetQuery(poolConn, rv$priority_lookup_uid_query()))
      rv$priority_lookup_uid <- reactive(if(nchar(input$priority) == 0) "NULL" else paste0("'", rv$priority_lookup_uid_step(), "'"))
      
      #lookup site name uid
      rv$site_name_lookup_uid_step <- reactive(odbc::dbGetQuery(poolConn, paste0("select site_name_lookup_uid from fieldwork.tbl_site_name_lookup where site_name = '", input$site_name, "'")) %>% pull())
      
      rv$site_name_lookup_uid <- reactive(if (nchar(input$site_name) > 0) paste0("'", rv$site_name_lookup_uid_step(), "'") else "NULL")
      
      #work number nullable
      rv$work_number <- reactive(if(nchar(input$work_number) > 0) paste0("'", input$work_number, "'") else "NULL")
      
      #make other fields nullable (and add single quotes)
      rv$system_id <- reactive(if(nchar(input$system_id) > 0) paste0("'", input$system_id, "'") else "NULL")
      
      rv$phase <- reactive(con_phase %>% dplyr::filter(phase == input$con_phase) %>% 
                             select(con_phase_lookup_uid) %>% pull())
      
      rv$requested_by <- reactive(requested_by_lookup %>% dplyr::filter(requested_by == input$requested_by) %>% 
                             select(requested_by_lookup_uid) %>% pull())
      
      rv$si_type <- reactive(si_lookup %>% dplyr::filter(special_investigation_type == input$type) %>% 
                             select(special_investigation_lookup_uid) %>% pull())
      
      rv$phase_null <- reactive(if(nchar(input$con_phase) == 0) "NULL" else paste0("'", rv$phase(), "'"))  
      
      rv$test_date <- reactive(if(is.na(input$date)) "NULL" else paste0("'", input$date, "'"))
      rv$sensor_collect_date <- reactive(if(length(input$sensor_collect_date) == 0) "NULL" else paste0("'", input$sensor_collect_date, "'"))
      rv$summary_date <- reactive(if(length(input$summary_date) == 0) "NULL" else paste0("'", input$summary_date, "'"))
      
      rv$photos <- reactive(if(nchar(input$photos) == 0) "NULL" else paste0("'", input$photos, "'"))  
      rv$qaqc_complete <- reactive(if(nchar(input$qaqc_complete) == 0) "NULL" else paste0("'", input$qaqc_complete, "'"))  
      rv$summary_needed <- reactive(if(nchar(input$summary_needed) == 0) "NULL" else paste0("'", input$summary_needed, "'"))  
      rv$sensor_deployed <- reactive(if(nchar(input$sensor_deployed) == 0) "NULL" else paste0("'", input$sensor_deployed, "'"))  
      
      #notes
      rv$notes_step <- reactive(gsub('\'', '\'\'', input$notes))
      rv$notes_step_two <- reactive(special_char_replace(rv$notes_step()))
      rv$notes <- reactive(if(nchar(rv$notes_step_two()) == 0) "NULL" else paste0("'", rv$notes_step_two(), "'"))
      
      #2.1.4 Query and show tables -------
      #get the table of SIs
      rv$si_table_query <- reactive(paste0("SELECT * FROM fieldwork.viw_special_investigation_full 
                                            WHERE system_id = '", input$system_id, "'
                                            OR work_number = ", rv$work_number(), " 
                                            OR site_name_lookup_uid = ", rv$site_name_lookup_uid()))
      rv$si_table_db <- reactive(dbGetQuery(poolConn, rv$si_table_query()))
      rv$si_table <- reactive(rv$si_table_db() %>% mutate(across(c("test_date", "sensor_collection_date"), as.character)) %>% 
                                 mutate(across(c("photos_uploaded", "qaqc_complete"), 
                                           ~ case_when(. == 1 ~ "Yes", 
                                                          . == 0 ~ "No"))) %>% 
                                 dplyr::select("test_date", "special_investigation_type",
                                               "requested_by", "phase", "sensor_collection_date",
                                               "results_summary"))
      
      #show table of SIs
      output$si_table <- renderDT(
        datatable(rv$si_table(), 
                  colnames = c('Test Date', 'Type', 'Requested By', 
                               'Phase', 'Sensor Collection Date', 'Results Summary'),
                  selection = 'single', 
                  style = 'bootstrap', 
                  class = 'table-responsive, table-hover', 
                  escape = FALSE 
        ))
      
      #get the table of future SIs
      rv$future_si_table_query <- reactive(paste0("SELECT * FROM fieldwork.viw_future_special_investigation_full 
                                            WHERE system_id = '", input$system_id, "'
                                            OR work_number = ", rv$work_number(), " 
                                            OR site_name_lookup_uid = ", rv$site_name_lookup_uid()))
      rv$future_si_table_db <- reactive(dbGetQuery(poolConn, rv$future_si_table_query()))
      rv$future_si_table <- reactive(rv$future_si_table_db() %>% 
                                       dplyr::select("special_investigation_type",
                                                     "requested_by", "phase", "field_test_priority",
                                                     "notes"))
      
      #show table of future SIs
      output$future_si_table <- renderDT(
        datatable(rv$future_si_table(), 
                  colnames = c('Type', 'Requested By', 
                               'Phase', 'Priority', 'Notes'),
                  selection = 'single', 
                  style = 'bootstrap', 
                  class = 'table-responsive, table-hover', 
                  escape = FALSE 
        ))
      
      #2.1.5 Editing -----
      
      #when you click on a row, populate fields with data from that row
      observeEvent(input$si_table_rows_selected, {
        
        dataTableProxy('future_si_table') %>% selectRows(NULL)
        
        updateDateInput(session, "date", value = rv$si_table_db()$test_date[input$si_table_rows_selected])
        updateSelectInput(session, "con_phase", selected = rv$si_table_db()$phase[input$si_table_rows_selected])
        updateNumericInput(session, "type", value = rv$si_table_db()$special_investigation_type[input$si_table_rows_selected])
        updateNumericInput(session, "requested_by", value = rv$si_table_db()$requested_by[input$si_table_rows_selected])
        updateSelectInput(session, "qaqc_complete", selected = as.numeric(rv$si_table_db()$qaqc_complete[input$si_table_rows_selected]))
        updateNumericInput(session, "sensor_collect_date", value = rv$si_table_db()$sensor_collection_date[input$si_table_rows_selected])
        updateSelectInput(session, "summary_needed", selected = rv$si_table_db()$summary_needed[input$si_table_rows_selected])
        updateNumericInput(session, "summary_date", value = rv$si_table_db()$summary_date[input$si_table_rows_selected])
        updateSelectInput(session, "sensor_deployed", selected = as.numeric(rv$si_table_db()$sensor_deployed[input$si_table_rows_selected]))
        updateSelectInput(session, "photos", selected = as.numeric(rv$si_table_db()$photos_uploaded[input$si_table_rows_selected]))
        updateTextAreaInput(session, "notes", value = rv$si_table_db()$results_summary[input$si_table_rows_selected])
        reset("priority")
      })
      
      observeEvent(input$future_si_table_rows_selected, {
        
        dataTableProxy('si_table') %>% selectRows(NULL)
        
        updateSelectInput(session, "con_phase", selected = rv$future_si_table_db()$phase[input$future_si_table_rows_selected])
        updateNumericInput(session, "type", value = rv$future_si_table_db()$special_investigation_type[input$future_si_table_rows_selected])
        updateNumericInput(session, "requested_by", value = rv$future_si_table_db()$requested_by[input$future_si_table_rows_selected])
        updateSelectInput(session, "priority", selected = rv$future_si_table_db()$field_test_priority[input$future_si_table_rows_selected])
        updateTextAreaInput(session, "notes", value = rv$future_si_table_db()$notes[input$future_si_table_rows_selected])
        
        reset("date")
        reset("photos")
        reset("sensor_deployed")
        reset("qaqc_complete")
        reset("sensor_collect_date")
        reset("summary_needed")
        reset("summary_date")
      })
      
      #2.1.6 Labels -------
      
      #add/edit button toggle
      rv$label <- reactive(if(length(input$si_table_rows_selected) == 0) "Add New" else "Edit Selected")
      observe(updateActionButton(session, "add_test", label = rv$label()))
      
      rv$future_label <- reactive(if(length(input$future_si_table_rows_selected) == 0) "Add Future Special Investigation" else "Edit Selected Future SI")
      observe(updateActionButton(session, "future_test", label = rv$future_label()))
      
      
      #2.1.7 Add/Edit/Clear buttons -----
      #add and edit special investigation records
      observeEvent(input$add_test, {
        print(rv$qaqc_complete())
        if(length(input$si_table_rows_selected) == 0){
          #add to special investigation
          add_test_query <- paste0("INSERT INTO fieldwork.tbl_special_investigation (system_id, work_number, site_name_lookup_uid, 
          test_date, special_investigation_lookup_uid, requested_by_lookup_uid,  con_phase_lookup_uid, 
          photos_uploaded, sensor_collection_date,  qaqc_complete, summary_date, results_summary, sensor_deployed, summary_needed)
        	                  VALUES (", paste(rv$system_id(), rv$work_number(), rv$site_name_lookup_uid(),
        	                                   rv$test_date(), rv$si_type(), rv$requested_by(), rv$phase_null(), 
        	                                   rv$photos(), rv$sensor_collect_date(),  rv$qaqc_complete(), rv$summary_date(),
        	                                   iconv(rv$notes(), "latin1", "ASCII", sub=""), #Strip unicode characters that WIN1252 encoding will choke on locally
        	                                                                                 #This is dumb.
        	                                   rv$sensor_deployed(), rv$summary_needed(), sep = ", "), ")")

          odbc::dbGetQuery(poolConn, add_test_query)
          
          # log the INSERT query, see utils.R
          insert.query.log(poolConn,
                           add_test_query,
                           tab_name,
                           session)
          
        }else{
          #edit special investigation
          edit_test_query <- paste0("UPDATE fieldwork.tbl_special_investigation SET system_id = ", rv$system_id(), ",
                                   work_number = ", rv$work_number(), ", 
                                   site_name_lookup_uid = ", rv$site_name_lookup_uid(), ", 
                                    test_date = ", rv$test_date(), ", 
                                    special_investigation_lookup_uid = ", rv$si_type(), ",
                                    requested_by_lookup_uid =", rv$requested_by(), ", 
                                    con_phase_lookup_uid = ", rv$phase_null(), ", 
                                    photos_uploaded = ", rv$photos(), ", 
                                    sensor_collection_date = ", rv$sensor_collect_date(), ", 
                                    qaqc_complete = ", rv$qaqc_complete(), ", 
                                    summary_date = ", rv$summary_date(), ",
                                    results_summary = ", iconv(rv$notes(), "latin1", "ASCII", sub=""), ", 
                                    sensor_deployed = ", rv$sensor_deployed(), ", 
                                    summary_needed = ", rv$summary_needed(), "
                                   WHERE special_investigation_uid = '", rv$si_table_db()[input$si_table_rows_selected, 1], "'")
          
          dbGetQuery(poolConn, edit_test_query)
          
          # log the UPDATE query, see utils.R
          insert.query.log(poolConn,
                           edit_test_query,
                           tab_name,
                           session)
          
        }
        
        if(length(input$future_si_table_rows_selected) > 0){
          
          delete_future_query <- paste0("DELETE FROM fieldwork.tbl_future_special_investigation 
                                        WHERE future_special_investigation_uid = '",
                                        rv$future_si_table_db()[input$future_si_table_rows_selected, 1],"'")
                                
          odbc::dbGetQuery(poolConn, delete_future_query)
          
          # log the DELETE query, see utils.R
          insert.query.log(poolConn,
                           delete_future_query,
                           tab_name,
                           session)
        }
        
        rv$future_si_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$future_si_table_query()))
        rv$si_table_db <- reactive(dbGetQuery(poolConn, rv$si_table_query()))
        rv$all_si_table_db <- reactive(dbGetQuery(poolConn, rv$all_query()))
        rv$all_future_si_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$all_future_query()))
        
        
        #check if a deployment exists for this test (if a sensor was used). If it does not, bring up a dialoge box asking the user
        #if they would like to create a deployment. If yes, that takes them to the deployment page
        si_deployment_exists_query <- paste0("select * from fieldwork.viw_deployment_full where term = 'Special' and (admin.fun_smp_to_system(smp_id) = ", rv$system_id(), " OR site_name = ", rv$system_id(), ") and deployment_dtime_est < '", input$date, "'::timestamp + interval '3 days' and deployment_dtime_est > '", input$date, "'::timestamp - interval '3 days'")
        
        si_deployment_exists_table <- dbGetQuery(poolConn, si_deployment_exists_query)
        
        si_deployment_exists <- nrow(si_deployment_exists_table) > 0
        
        if(si_deployment_exists == FALSE & input$sensor_deployed == 1){
          showModal(modalDialog(title = "Deploy Sensor", 
                                "Would you like to add a sensor deployment for this Special Investigation?", 
                                modalButton("No"), 
                                actionButton(ns("add_deployment"), "Yes")))
        }else{
        reset("date")
        reset("type")
        reset("requested_by")
        reset("con_phase")
        reset("photos")
        reset("sensor_deployed")
        reset("qaqc_complete")
        reset("sensor_collect_date")
        reset("summary_date")
        reset("priority")
        reset("summary_needed")
        reset("notes")
        }
        }
        )
      
      #set ticker to notify other module
      rv$refresh_deploy <- 0
      
      observeEvent(input$future_test, {
        if(length(input$future_si_table_rows_selected) == 0){
          #add to future special investigation
          
          add_future_test_query <- paste0("INSERT INTO fieldwork.tbl_future_special_investigation (system_id, work_number, site_name_lookup_uid, 
          special_investigation_lookup_uid, requested_by_lookup_uid,  con_phase_lookup_uid, 
          field_test_priority_lookup_uid, notes)
        	                  VALUES (", paste(rv$system_id(), rv$work_number(), rv$site_name_lookup_uid(),
        	                                   rv$si_type(), rv$requested_by(), rv$phase_null(),
        	                                   rv$priority_lookup_uid(),
        	                                   iconv(rv$notes(), "latin1", "ASCII", sub=""), #Strip unicode characters that WIN1252 encoding will choke on locally
        	                                                                                 #This is dumb. 
        	                                   sep = ", "), ")")

          odbc::dbGetQuery(poolConn, add_future_test_query)
          
          # log the INSERT query, see utils.R
          insert.query.log(poolConn,
                           add_future_test_query,
                           tab_name,
                           session)
          
        }else{
          #edit future special investigation
          
          edit_future_test_query <- paste0("UPDATE fieldwork.tbl_future_special_investigation SET system_id = ", rv$system_id(), ",
                                   work_number = ", rv$work_number(), ", 
                                   site_name_lookup_uid = ", rv$site_name_lookup_uid(), ", 
                                    special_investigation_lookup_uid = ", rv$si_type(), ",
                                    requested_by_lookup_uid =", rv$requested_by(), ", 
                                    con_phase_lookup_uid = ", rv$phase_null(), ", 
                                    field_test_priority_lookup_uid = ", rv$priority_lookup_uid(), ", 
                                    notes = ", iconv(rv$notes(), "latin1", "ASCII", sub=""), "
                                   WHERE future_special_investigation_uid = '", rv$future_si_table_db()[input$future_si_table_rows_selected, 1], "'")
          
          dbGetQuery(poolConn, edit_future_test_query)
          
          # log the UPDATE query, see utils.R
          insert.query.log(poolConn,
                           edit_future_test_query,
                           tab_name,
                           session)
          
        }
        
        rv$future_si_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$future_si_table_query()))
        rv$si_table_db <- reactive(dbGetQuery(poolConn, rv$si_table_query()))
        rv$all_si_table_db <- reactive(dbGetQuery(poolConn, rv$all_query()))
        rv$all_future_si_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$all_future_query()))
        
        reset("date")
        reset("type")
        reset("requested_by")
        reset("con_phase")
        reset("sensor_deployed")
        reset("photos")
        reset("qaqc_complete")
        reset("sensor_collect_date")
        reset("summary_date")
        reset("summary_needed")
        reset("priority")
        reset("notes")
        
      }
      )
      
      observeEvent(input$add_deployment, {
        rv$refresh_deploy <- rv$refresh_deploy + 1
        updateTabsetPanel(session = parent_session, "inTabset", selected = "deploy_tab")
        removeModal()
      })
      
      observeEvent(input$clear, {
        showModal(modalDialog(title = "Clear All Fields", 
                              "Are you sure you want to clear all fields on this tab?", 
                              modalButton("No"), 
                              actionButton(ns("confirm_clear"), "Yes")))
      })
      
      observeEvent(input$confirm_clear, {
        reset("system_id")
        reset("work_number") 
        reset("site_name")
        reset("date")
        reset("type")
        reset("requested_by")
        reset("con_phase")
        reset("photos")
        reset("summary_needed")
        reset("sensor_deployed")
        reset("qaqc_complete")
        reset("sensor_collect_date")
        reset("summary_date")
        reset("priority")
        reset("notes")
        removeModal()
      })
      
      #delete a future SI
      #first, intermediate dialog box
      observeEvent(input$delete_future_test, {
        showModal(modalDialog(title = "Delete Future Special Investigation", 
                              "Delete Future Special Investigation?", 
                              modalButton("No"), 
                              actionButton(ns("confirm_delete_future"), "Yes")))
      })
      
      observeEvent(input$confirm_delete_future, {
        
        delete_future_query <- paste0("DELETE FROM fieldwork.tbl_future_special_investigation
                                      WHERE future_special_investigation_uid = '",
                                      rv$future_si_table_db()[input$future_si_table_rows_selected, 1], "'")
        
        odbc::dbGetQuery(poolConn, delete_future_query)
        
        
        # log the UPDATE query, see utils.R
        insert.query.log(poolConn,
                         delete_future_query,
                         tab_name,
                         session)
        
        #update future cet table
        rv$future_si_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$future_si_table_query()))
        rv$all_future_si_table_db <- reactive(reactive(odbc::dbGetQuery(poolConn, rv$all_future_si_query())))
        #remove pop up
        removeModal()
      }) 
      
      #2.2 View all SIs -----------------------------------------------------------
      #2.2.1 query and show table ------
      rv$all_query <- reactive(paste0("SELECT * FROM fieldwork.viw_special_investigation_full ORDER BY test_date DESC"))
      rv$all_si_table_db <- reactive(dbGetQuery(poolConn, rv$all_query()))
      rv$all_si_table <- reactive(rv$all_si_table_db() %>% mutate(across(c("test_date", "sensor_collection_date", "summary_date"),
                                                                         as.character)) %>% 
                                     mutate(across(c("qaqc_complete", "photos_uploaded", "sensor_deployed", "summary_needed"), 
                                                ~ case_when(. == 1 ~ "Yes", 
                                                              . == 0 ~ "No"))) %>% 
                                     dplyr::select("system_id",  "project_name", "test_date", 
                                                   "special_investigation_type", "requested_by", "phase", "sensor_deployed",
                                                   "sensor_collection_date", "photos_uploaded", "qaqc_complete",
                                                   "summary_date", "turnaround_days", "results_summary", "summary_needed"))
      
      #show table of SIs
      output$all_si_table <- renderReactable(
        reactable(rv$all_si_table()[, c(1:12)], 
                  columns = list(
                    system_id = colDef(name = "System ID"),
                    project_name = colDef(name = "Project Name"),
                    test_date = colDef(name = "Test Date"),
                    special_investigation_type = colDef(name = "Type"), 
                    requested_by = colDef(name = "Requested By"),
                    phase = colDef(name = "Phase"),
                    sensor_deployed = colDef(name = "Sensor Deployed", style = function(value){
                      if(is.na(value)){
                        color = "#CCC916"
                        textColor = "#000000"
                      }else{
                        color = "#272B30"
                        textColor = "#c8c8c8"
                      }
                      list(backgroundColor = color, color = textColor, fontweight = "bold")
                    }),
                    sensor_collection_date = colDef(name = "Sensor Collection Date", style = function(value, index){
                      if(is.na(value) & rv$all_si_table()$sensor_deployed[index] == "Yes"){
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
                    qaqc_complete = colDef(name = "QA/QC Complete", style = function(value,index){
                      if((is.na(value) & rv$all_si_table()$sensor_deployed[index] == "Yes")){
                        color = "#CCC916"
                        textColor = "#000000"
                        #need to do a nested if so that it odesn't try to evaluate a value where there is no value (is.na())
                      }else if(!is.na(value)){
                        if(value == "No"){
                          color = "#CCC916"
                          textColor = "#000000"
                        }else{
                          color = "#272B30"
                          textColor = "#c8c8c8"
                        }
                      }else{
                        color = "#272B30"
                        textColor = "#c8c8c8"
                      }
                      list(backgroundColor = color, color = textColor, fontweight = "bold")
                    }),
                    summary_date = colDef(name = "Summary Date", 
                                          style = function(value, index){
                                            if(is.na(value) & (is.na(rv$all_si_table()$summary_needed[index]) |
                                                               rv$all_si_table()$summary_needed[index] == "Yes")){
                                              color = "#CCC916"
                                              textColor = "#000000"
                                            }else{
                                              color = "#272B30"
                                              textColor = "#c8c8c8"
                                            }
                                            list(backgroundColor = color, color = textColor, fontweight = "bold")
                                          }),
                    turnaround_days = colDef(name = "Turnaround (Days)")
                  ),
                  fullWidth = TRUE,
                  selection = 'single', 
                  searchable = TRUE,
                  onClick = "select",
                  selectionId = ns("si_selected"),
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = c(10, 25, 50),
                  defaultPageSize = 10,
                  height = 750,
                  style = reactableTheme(color = "#c8c8c8",
                                         backgroundColor = "#272B30"),
                  details = function(index){
                    nest <- rv$all_si_table()[rv$all_si_table_db()$special_investigation_uid == rv$all_si_table_db()$special_investigation_uid[index], ][13]
                    htmltools::div(style = "padding:16px",
                                   reactable(nest,
                                             columns = list(results_summary = colDef(name = "Results Summary")),
                                             style = reactableTheme(color = "#c8c8c8",
                                                                    backgroundColor = "#272B30"))
                    )
                  }
         )
      )
      
      #2.2.2  click row -------
      #select row in full si table
      observeEvent(input$si_selected, {
        
        #set all inputs to null
        updateSelectizeInput(session, "system_id", selected = character(0))
        updateSelectInput(session, "work_number", selected = "")
        updateSelectInput(session, "site_name", selected = "")
        
        #check for system id, then work number, then site name
        if(!is.na(rv$all_si_table_db()$system_id[input$si_selected])){
          updateSelectizeInput(session, "system_id", choices = sys_id,
                               selected = rv$all_si_table_db()$system_id[input$si_selected], 
                               server = TRUE)
        }else if(!is.na(rv$all_si_table_db()$work_number[input$si_selected])){
          updateSelectInput(session, "work_number", selected = rv$all_si_table_db()$work_number[input$si_selected])
        }else if(!is.na(rv$all_si_table_db()$site_name[input$si_selected]) > 0){
          updateSelectInput(session, "site_name", selected = rv$all_si_table_db()$site_name[input$si_selected])
        }
        
        updateTabsetPanel(session = parent_session, "inTabset", selected = "si_tab")
        updateReactable("all_future_si_table", selected = NA)
        
        delay(300, {
          si_row <- which(rv$si_table_db()$special_investigation_uid == rv$all_si_table_db()$special_investigation_uid[input$si_selected], arr.ind = TRUE)
          dataTableProxy('si_table') %>% selectRows(si_row)
        })
      })
      
      #2.3 View Future SIs --------
      #2.3.1 query and display table -------
      rv$all_future_query <- reactive(paste0("SELECT * FROM fieldwork.viw_future_special_investigation_full ORDER BY field_test_priority_lookup_uid DESC"))
      rv$all_future_si_table_db <- reactive(dbGetQuery(poolConn, rv$all_future_query()))
      rv$all_future_si_table <- reactive(rv$all_future_si_table_db() %>% 
                                            dplyr::select("system_id", "project_name", "special_investigation_type", 
                                                          "requested_by", "phase",
                                                          "field_test_priority", "notes"))
      
      #show table of SIs
      output$all_future_si_table <- renderReactable(
        reactable(rv$all_future_si_table()[, 1:6], 
                  columns = list(
                    system_id = colDef(name = "System ID"),
                    project_name = colDef(name = "Project Name"),
                    special_investigation_type = colDef(name = "Type"),
                    requested_by = colDef(name = "Requested By"),
                    phase = colDef(name = "Phase"),
                    field_test_priority = colDef(name = "Priority")
                  ),
                  fullWidth = TRUE,
                  selection = 'single', 
                  searchable = TRUE,
                  onClick = "select",
                  selectionId = ns("future_si_selected"),
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = c(10, 25, 50),
                  defaultPageSize = 10,
                  height = 750,
                  style = reactableTheme(color = "#c8c8c8",
                                         backgroundColor = "#272B30"),
                  details = function(index){
                    nest <- rv$all_future_si_table()[rv$all_future_si_table_db()$future_special_investigation_uid == rv$all_future_si_table_db()$future_special_investigation_uid[index], ][7]
                    htmltools::div(style = "padding:16px", 
                                   reactable(nest, 
                                             columns = list(notes = colDef(name = "Notes")),
                                             style = reactableTheme(color = "#c8c8c8",
                                                                    backgroundColor = "#272B30"))
                    )
                  }
        )
      )
      
      #2.3.2 click row ------
      #select fow in full si table
      observeEvent(input$future_si_selected, {
        
        #set all inputs to null
        updateSelectizeInput(session, "system_id", selected = character(0))
        updateSelectInput(session, "work_number", selected = "")
        updateSelectInput(session, "site_name", selected = "")
        
        #check for system id, then work number, then site name
        if(!is.na(rv$all_future_si_table_db()$system_id[input$future_si_selected])){
          updateSelectizeInput(session, "system_id", choices = sys_id,
                               selected = rv$all_future_si_table_db()$system_id[input$future_si_selected], 
                               server = TRUE)
        }else if(!is.na(rv$all_future_si_table_db()$work_number[input$future_si_selected])){
          updateSelectInput(session, "work_number", selected = rv$all_future_si_table_db()$work_number[input$future_si_selected])
        }else if(!is.na(rv$all_future_si_table_db()$site_name[input$future_si_selected]) > 0){
          updateSelectInput(session, "site_name", selected = rv$all_future_si_table_db()$site_name[input$future_si_selected])
        }
        
        updateTabsetPanel(session = parent_session, "inTabset", selected = "si_tab")
        updateReactable("all_si_table", selected = NA)
        
        delay(300, {
          future_si_row <- which(rv$future_si_table_db()$future_special_investigation_uid == rv$all_future_si_table_db()$future_special_investigation_uid[input$future_si_selected], arr.ind = TRUE)
          dataTableProxy('future_si_table') %>% selectRows(future_si_row)
        })
      })
      
      #2.4 return values
      return(
        list(
          refresh_deploy = reactive(rv$refresh_deploy),
          system_id = reactive(input$system_id), 
          site_name = reactive(input$site_name)
        )
      )
      
    }
  )
}