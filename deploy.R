#Deploy Sensor tab
#Left Sidebar with 3 tables: Future, Active, and Past Deployments

deployUI <- function(id, label = "deploy", sensor_serial, site_names, html_req, long_term_lookup, deployment_lookup, research_lookup, priority, future_req, sensor_issue_lookup){
  ns <- NS(id)
  list(
  tabPanel("Deploy Sensor", value = "deploy_tab",
           titlePanel("Deploy Sensor"), 
           fluidRow(
             column(width = 4, 
                    sidebarPanel(width = 12, 
                                 fluidRow(h5()),
                                 fluidRow(h5()),
                                 fluidRow(
                                   column(6,selectizeInput(ns("smp_id"), future_req(html_req("SMP ID")), 
                                                        choices = NULL, 
                                                        options = list(
                                                          placeholder = 'Select an Option',
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                        ))), 
                                   column(6,selectInput(ns("site_name"), future_req(html_req("Site Name")), 
                                                        choices = c("", site_names), selected = NULL))),
                                 #If there is no deployment date, given an option for future test priority
                                 conditionalPanel(condition = "input.deploy_date === null", 
                                                  ns = ns, 
                                                  fluidRow(
                                                    column(6, dateInput(ns("premonitoring_date"), "Pre-Monitoring Inspection Date", 
                                                                        value = as.Date(NA))),
                                                    column(6, selectInput(ns("priority"), "Future Test Priority", 
                                                                          choices = c("", priority$field_test_priority), selected = NULL))
                                                  )),
                                 disabled(selectInput(ns("ready"), "Ready for Deployment?", 
                                                              choices = c("", "Yes" = 1, "No" = 0))
                                 ),
                                 fluidRow(
                                   column(6,selectInput(ns("well_name"), future_req(html_req("Location")), 
                                                        choices = "")), 
                                   column(6,selectInput(ns("sensor_id"), html_req("Sensor ID"), 
                                                        choices = c("", sensor_serial), selected = NULL))),
                                 textOutput(ns("sensor_warning")),
                                 fluidRow(
                                   column(6, selectInput(ns("sensor_purpose"), html_req("Sensor Purpose"), choices = c("", deployment_lookup$type), selected = NULL)),
                                   column(6, selectInput(ns("term"), html_req("Term"), choices = c("", long_term_lookup$type), selected = NULL))),
                                 fluidRow(
                                   column(6, selectInput(ns("research"), "Research", choices = c("", research_lookup$type), selected = NULL)),
                                   column(6, selectInput(ns("interval"), html_req("Interval (min)"), choices = c("", 5, 15), selected = NULL))),
                                 fluidRow(
                                   column(6,dateInput(ns("deploy_date"), html_req("Deployment Date"), value = as.Date(NA))),
                                   column(6,dateInput(ns("collect_date"), "Collection Date", value = as.Date(NA)))), 
                                 #ask about download error and redeploying sensor once you add a collection date
                                 conditionalPanel(width = 12, 
                                                  condition = "input.deploy_date", 
                                                  ns = ns, 
                                                  HTML("If well is dry, enter 0")),
                                 #ask about manual depth to water measurement during deployment
                                 fluidRow(column(6,
                                   conditionalPanel(width = 12, 
                                                    condition = "input.deploy_date",
                                                    ns = ns,
                                                    numericInput(ns("deploy_depth_to_water"), 
                                                                 "Deployment Depth-to-Water (ft)", value = NA, min = 0))),               
                                   column(6, 
                                          #ask about manual depth to water measurement during collection
                                 conditionalPanel(width = 12, 
                                                  condition = "input.collect_date",
                                                  ns = ns,
                                                  numericInput(ns("collect_depth_to_water"), 
                                                               "Collection Depth-to-Water (ft)", value = NA, min = 0),
                                 ))),
                                 #ask if there is a download error
                                 conditionalPanel(width = 12, 
                                                  condition = "input.collect_date",
                                                  ns = ns,
                                                  selectInput(ns("download_error"), html_req("Did you encounter a sensor issue?"), 
                                                              choices = c("", "No" = 0, "Yes" = 1), selected = NULL)),
                                 #if there is a download error, ask if the sensor is broken
                                 conditionalPanel(width = 12, 
                                                  condition = "input.download_error == '1'", 
                                                  ns = ns,
                                                  checkboxInput(ns("sensor_broken"), "Take Sensor Out of Service?")), 
                                 #if sensor is out of service, ask what the issues are and if data should be requested
                                 conditionalPanel(width = 12, 
                                                  ns = ns, 
                                                  condition = "input.sensor_broken", 
                                                  selectInput(ns("issue_one"), html_req("Issue #1"), 
                                                              choices = c("", sensor_issue_lookup$sensor_issue), selected = NULL), 
                                                  selectInput(ns("issue_two"), "Issue #2", 
                                                              choices = c("", sensor_issue_lookup$sensor_issue), selected = NULL), 
                                                  checkboxInput(ns("request_data"), "Request Data Be Retrieved and Sent to PWD")),
                                 #if there is a collection date, ask if user wants to redeploy
                                 conditionalPanel(width = 12, 
                                                  condition = "input.collect_date",
                                                  ns = ns, 
                                                  checkboxInput(ns("redeploy"), "Redeploy?"),
                                                  h6("Redeploy a sensor in the same well on this collection date")), 
                                 #if redeploying and the sensor is broken, put in a new sensor id
                                 conditionalPanel(width = 12, 
                                                  condition = "input.redeploy & input.sensor_broken", 
                                                  ns = ns, 
                                                  selectInput(ns("new_sensor_id"), html_req("New Sensor ID"), 
                                                              choices = c("", sensor_serial), selected = NULL),
                                                  textOutput(ns("new_sensor_warning"))),
                                 #notes for the future deployment or current/past deployment
                                 textAreaInput(ns("notes"), "Notes"),
                                 #if redeploying, enter notes for the new deployment
                                 conditionalPanel(width = 12, 
                                                  condition = "input.redeploy", 
                                                  ns = ns, 
                                                  textAreaInput(ns("redeployment_notes"), "New Deployment Notes")),
                                 #if there is no deployment date, show the "Add Future Deployment" button
                                 conditionalPanel(condition = "input.deploy_date === null", 
                                                  ns = ns, 
                                                  actionButton(ns("future_deploy"), "Add Future Deployment"), 
                                                  actionButton(ns("delete_future"), "Delete Future Deployment")),
                                 actionButton(ns("deploy_sensor"), "Deploy Sensor"), 
                                 actionButton(ns("clear_deploy_fields"), "Clear All Fields"),
                                 #note about requirements
                                 fluidRow(
                                   HTML(paste(html_req(""), " indicates required field for complete tests. ", future_req(""), " indicates required field for future tests.", "Check the \"Add Sensor\" tab to see if the Sensor ID you are trying to deploy is actively deployed elsewhere.")))
                    ), 
             ),
             column(width = 8,
                    #show tables when an smp id is selected
                    conditionalPanel(condition = "input.smp_id || input.site_name",
                                     ns = ns, 
                                     h4(textOutput(ns("future_text"))), 
                                     DTOutput(ns("future_deployment")), 
                                     h4(textOutput(ns("active_text"))),
                                     DTOutput(ns("current_deployment")), 
                                     h4(textOutput(ns("previous_text"))),
                                     DTOutput(ns("prev_deployment"))
                    )
             )
           )
  )
  )
}

deployServer <- function(id, parent_session, ow, collect, sensor, poolConn, deployment_lookup, srt, si, cwl_history, smp_id, sensor_issue_lookup){
  
  moduleServer(
    id, 
    function(input, output, session){
  
      #define ns to use in modals
      ns <- session$ns
      
      #start reactiveValues for this section/tab
      rv <- reactiveValues()
      
      #update SMP IDs
      updateSelectizeInput(session, "smp_id", choices = smp_id, selected = character(0), server = TRUE)
      
      #get a site name UID if a site name is selected
      rv$site_name_lookup_uid_step <- reactive(odbc::dbGetQuery(poolConn, paste0("select site_name_lookup_uid from fieldwork.site_name_lookup where site_name = '", input$site_name, "'")) %>% pull())
      
      rv$site_name_lookup_uid <- reactive(if (nchar(input$site_name) > 0) paste0("'", rv$site_name_lookup_uid_step(), "'") else "NULL")
      
      #set smp id to NULL if using site name
      rv$smp_id <- reactive(if (nchar(input$smp_id) > 0) paste0("'", input$smp_id, "'") else "NULL")
      
      ## well panel
      #update sensor id choices based on the hobo list
      observe(updateSelectInput(session, inputId = "sensor_id", choices = c("", sensor$sensor_serial()), selected = NULL))
    
      #query ow_suffixes based on smp_id
      rv$ow_suffixes <- reactive(odbc::dbGetQuery(poolConn, paste0(
        "select ow_suffix from fieldwork.ow_all where smp_id = ", rv$smp_id(), " OR site_name_lookup_uid = ", rv$site_name_lookup_uid())) %>% dplyr::pull())
      
      observe(updateSelectInput(session, "well_name", choices = c("", rv$ow_suffixes())))
      
      #set minimum collection date to be same day as deployment date
      observe(updateDateInput(session, "collect_date", min = input$deploy_date))
      
      #COMING FROM OTHER MODULES -----------
      #upon clicking "deploy at this smp" in add_ow
      observeEvent(ow$refresh_deploy(), {
        #using shinyjs::reset is too slow and will reset after the selection is updated to the desired SMPID
        #want to set this to NULL so that it refreshes the location query but that is not working (1/11/21)
        #needs to be set as "", not NULL (also 1/11/21)
        updateSelectizeInput(session, "smp_id", selected = character(0))
        updateSelectizeInput(session, "smp_id", choices = smp_id, selected = ow$smp_id(), server = TRUE)
      })
      
      #upon adding or removing a site in add_ow
      observeEvent(ow$site_name_db(), {
        updateSelectInput(session, "site_name", choices = c("", ow$site_name_db()$site_name))
      })
      
      #upon clicking "deploy this sensor" in add_sensor
      observeEvent(sensor$refresh_serial_no(), {
        updateSelectInput(session, "sensor_id", selected = sensor$serial_no())
      })
      
      #upon clicking through the dialogue box in srt tab
      #add -1 to convert from system to smp lol
      #update the smp id and term
      observeEvent(srt$refresh_deploy(), {
        if(srt$refresh_deploy() > 0){
        updateSelectizeInput(session, "smp_id", selected = character(0))
        updateSelectizeInput(session, "smp_id", choices = smp_id, selected = paste0(srt$system_id(), "-1"), server = TRUE)
        updateSelectInput(session, "term", selected = "SRT")
        }
      })
      
      #upon clicking through the dialogue box in SI tab
      #update the (smp_id or site name) and term
      observeEvent(si$refresh_deploy(), {
        if(si$refresh_deploy() > 0){
          if(nchar(si$system_id()) > 0){
          updateSelectInput(session, "site_name", selected = NULL)
          updateSelectizeInput(session, "smp_id", selected = character(0))
          updateSelectizeInput(session, "smp_id", choices = smp_id, selected = paste0(si$system_id(), "-1"), server = TRUE)
          updateSelectInput(session, "term", selected = "Special")
          }else if(nchar(si$site_name()) > 0){
            updateSelectInput(session, "site_name", selected = NULL)
            updateSelectizeInput(session, "smp_id", selected = character(0))
            updateSelectInput(session, "site_name", selected = si$site_name())
            updateSelectInput(session, "term", selected = "Special")
          }
        }
      })
      
      #upon clicking a row in collection_calendar
      observeEvent(collect$deploy_refresh(), {
        updateSelectizeInput(session, "smp_id", selected = character(0))
        updateSelectInput(session, "site_name", selected = "")
        # need to get through the initial load where length == 0 (is.na does not work in that case)
        # go to either smp id or site name
        if(length(collect$smp_id()) > 0){
          if(!is.na(collect$smp_id())){
          updateSelectizeInput(session, "smp_id", choices = smp_id, selected = collect$smp_id(), server = TRUE)
          }else{
          updateSelectInput(session, "site_name", selected = collect$site_name())
          }
          #delay so that the selectizeInput is updated and table is quereied before it is searched by R
          delay(250,{
                  rv$active_row <- reactive(which(rv$active_table_db()$deployment_uid == collect$row(), arr.ind = TRUE))
                  dataTableProxy('current_deployment') %>% selectRows(rv$active_row())
                })
        }
      })
      
      #upon click a row in future table (in collection calendar)
      observeEvent(collect$future_deploy_refresh(), {
        #print(collect$future_smp_id())
        if(length(collect$future_smp_id()) > 0){
          if(!is.na(collect$future_smp_id())){
            updateSelectInput(session, "site_name", selected = "")
            updateSelectizeInput(session, "smp_id", choices = smp_id,  selected = collect$future_smp_id(), server = TRUE)
          }else{
            updateSelectizeInput(session, "smp_id", selected = character(0))
            updateSelectInput(session, "site_name", selected = collect$future_site_name())
          }
          #delay so that the selectizeInput is updated and table is quereied before it is searched by R
          delay(250, {
                  rv$future_row <- reactive(which(rv$future_table_db()$future_deployment_uid == collect$future_row(), arr.ind = TRUE))
                  dataTableProxy('future_deployment') %>% selectRows(rv$future_row())
                })
        }
      })
      
      #upon clicking a row in current sites (in history)
      observeEvent(cwl_history$active_deploy_refresh(), {
        updateSelectizeInput(session, "smp_id", selected = character(0))
        updateSelectInput(session, "site_name", selected = "")
        #make sure this exists
        #"missing" function might help
        #changing order might help
        if(length(cwl_history$active_smp_id()) > 0){
          if(!is.na(cwl_history$active_smp_id())){
            updateSelectizeInput(session, "smp_id", choices = smp_id, selected = cwl_history$active_smp_id(), server = TRUE)
          }else{
            updateSelectInput(session, "site_name", selected = cwl_history$active_site_name())
          }
        }
      }
      )
      
      #upon clicking a row in past sites (in history)
      observeEvent(cwl_history$past_deploy_refresh(), {
        updateSelectizeInput(session, "smp_id", selected = character(0))
        updateSelectInput(session, "site_name", selected = "")
        if(length(cwl_history$past_smp_id()) > 0){
          if(!is.na(cwl_history$past_smp_id())){
            updateSelectizeInput(session, "smp_id", choices = smp_id, selected = cwl_history$past_smp_id(), server = TRUE)
          }else{
            updateSelectInput(session, "site_name", selected = cwl_history$past_site_name())
          }
        }
      }
      )
      
    #back to this tab ----------
      #toggle to make sure that only of SMP ID or Site Name is selected
      observe(toggleState("smp_id", condition = nchar(input$site_name) == 0))
      observe(toggleState("site_name", condition = nchar(input$smp_id) == 0))
      
      
      #toggle label for redeployment
      rv$redeploy_label <- reactive(if(input$sensor_broken == TRUE){
        "Redeploy with Different Sensor"
      }else{
        "Redeploy with Same Sensor"
      })
      
      observe(updateCheckboxInput(session, "redeploy", label = rv$redeploy_label()))
      
      #toggle labels for depth to water / water depth 
      rv$deploy_depth_to_water_label <- reactive(if(str_detect(input$well_name, "^SW")){
        "Deployment Water Depth (ft)"
      }else{
        "Deployment Depth-to-Water (ft)"
      })
      
      rv$collect_depth_to_water_label <- reactive(if(str_detect(input$well_name, "^SW")){
        "Collection Water Depth (ft)"
      }else{
        "Collection Depth-to-Water (ft)"
      })
      
      observe(updateNumericInput(session, "deploy_depth_to_water", label = rv$deploy_depth_to_water_label()))
      observe(updateNumericInput(session, "collect_depth_to_water", label = rv$collect_depth_to_water_label()))
      
      #toggle label active/previous deployment notes
      rv$notes_label <- reactive(if(length(input$collect_date) == 0){
        "Notes"
      }else{
        "Completed Deployment Notes"
      })
      
      observe(toggleState("priority", condition = input$ready != 0))
      
      observeEvent(input$ready, {
        if(input$ready == 0){
        updateSelectInput(session, "priority", selected = "Dependent (see Notes)")
        updateSelectInput(session, "notes", label = "Note why deployment is not ready")
        }else{
          updateSelectInput(session, "notes", label = "Notes")
        }
      })
      
      observe(updateTextAreaInput(session, "notes", label = rv$notes_label()))
      
      #Get the Project name, combine it with SMP ID, and create a reactive header
      rv$smp_and_name_step <- reactive(odbc::dbGetQuery(poolConn, paste0("select smp_id, project_name from project_names where smp_id = '", input$smp_id, "'")))
      
      rv$smp_and_name <- reactive(paste(rv$smp_and_name_step()$smp_id[1], rv$smp_and_name_step()$project_name[1]))
      
      rv$future_text <- reactive(if(nchar(input$smp_id) > 0){
        paste("Future Deployments at", rv$smp_and_name())
      }else{
        paste("Future Deployments at", input$site_name)
      })
      
      output$future_text <- renderText(
        rv$future_text()
      )
      
      rv$active_text <- reactive(if(nchar(input$smp_id) > 0){
        paste("Active Deployments at", rv$smp_and_name())
      }else{
        paste("Active Deployments at", input$site_name)
      })
      
      output$active_text <- renderText(
        rv$active_text()
      )
      
      rv$previous_text <- reactive(if(nchar(input$smp_id) > 0){
        paste("Previous Deployments at", rv$smp_and_name())
      }else{
        paste("Previous Deployments at", input$site_name)
      })
      
      output$previous_text <- renderText(
        rv$previous_text()
      )
      
      ## sensor panel
      #get sensor purpose lookup uid
      rv$purpose <- reactive(deployment_lookup %>% dplyr::filter(type == input$sensor_purpose) %>% 
                               select(sensor_purpose_lookup_uid) %>% pull())
      
      #for future deployment - make inputs equal to NULL if blank
      rv$purpose_null <- reactive(if(nchar(input$sensor_purpose) == 0) "NULL" else paste0(rv$purpose()))
      
      rv$inventory_sensors_uid <- reactive(odbc::dbGetQuery(poolConn, paste0(
        "SELECT inventory_sensors_uid FROM fieldwork.inventory_sensors WHERE sensor_serial = '", input$sensor_id, "'"
      )))
      
      rv$inventory_sensors_uid_null <- reactive(if(nchar(input$sensor_id) == 0) "NULL" else paste0("'", rv$inventory_sensors_uid(), "'"))
      
      rv$interval_min <- reactive(if(nchar(input$interval) == 0) "NULL" else (paste0("'", input$interval, "'")))
      rv$deploy_depth_to_water <- reactive(if(is.na(input$deploy_depth_to_water)) "NULL" else paste0("'", input$deploy_depth_to_water, "'"))
      rv$collect_depth_to_water <- reactive(if(is.na(input$collect_depth_to_water)) "NULL" else paste0("'", input$collect_depth_to_water, "'"))
    
      #get term UID; make null if blank  
      rv$term <- reactive(odbc::dbGetQuery(poolConn, paste0("Select long_term_lookup_uid FROM fieldwork.long_term_lookup WHERE type = '", input$term, "'")) %>% pull)
      
      rv$term_null <- reactive(if(nchar(input$term) == 0) "NULL" else paste0("'", rv$term(), "'"))
      
      #get research uid
      rv$research <- reactive(odbc::dbGetQuery(poolConn, paste0("select research_lookup_uid FROM fieldwork.research_lookup WHERE type = '", input$research, "'")) %>%  pull)
      
      #sanitize notes
      rv$notes_step1 <- reactive(gsub('\'', '\'\'', input$notes))
      rv$notes <- reactive(if(nchar(rv$notes_step1()) == 0) "NULL" else paste0("'", rv$notes_step1(), "'"))
      
      #sanitize deployment notes
      rv$redeployment_notes_step1 <- reactive(gsub('\'', '\'\'', input$redeployment_notes))
      rv$redeployment_notes <- reactive(if(nchar(rv$redeployment_notes_step1()) == 0) "NULL" else paste0("'", rv$redeployment_notes_step1(), "'"))
      
      rv$download_error <- reactive(if(nchar(input$download_error) == 0 | length(input$collect_date) == 0){
        "NULL" 
      }else{paste0("'", input$download_error, "'")
      })
      
      #reset sensor broken and new sensor id if changing download error to no 
      observeEvent(input$download_error == 0, {
        reset("sensor_broken")
        reset("new_sensor_id")
      })
      
      #reset new sensor id if sensor broken changed
      observeEvent(input$sensor_broken, {
        reset("new_sensor_id")
      })
      
      #lookup priority uid
      rv$priority_lookup_uid_query <- reactive(paste0("select field_test_priority_lookup_uid from fieldwork.field_test_priority_lookup where field_test_priority = '", input$priority, "'"))
      rv$priority_lookup_uid_step <- reactive(dbGetQuery(poolConn, rv$priority_lookup_uid_query()))
      rv$priority_lookup_uid <- reactive(if(nchar(input$priority) == 0) "NULL" else paste0("'", rv$priority_lookup_uid_step(), "'"))
      
      #set fields to "NULL" if they are empty
      rv$collect_date <- reactive(if(length(input$collect_date) == 0) "NULL" else paste0("'", input$collect_date, "'"))
      rv$research_lookup_uid <- reactive(if(length(rv$research()) == 0) "NULL" else paste0("'", rv$research(), "'"))
      
      rv$premonitoring_date <- reactive(if(length(input$premonitoring_date) == 0) "NULL" else paste0("'", input$premonitoring_date, "'"))
      
      rv$ready <- reactive(if(nchar(input$ready) == 0) "NULL" else paste0("'", input$ready, "'"))
      
      #get sensor id for redeployment 
      rv$new_sensor_id <- reactive(if(nchar(input$new_sensor_id) > 0){
        paste0("'", input$new_sensor_id, "'")
      }else{
        paste0("'", input$sensor_id, "'")
      }
      )
      
      #get sensor uid from sensor id
      rv$new_inventory_sensors_uid <- reactive(odbc::dbGetQuery(poolConn, paste0(
        "SELECT inventory_sensors_uid FROM fieldwork.inventory_sensors WHERE sensor_serial = ", rv$new_sensor_id()
      )))
      
      ## show tables
      #query for active deployments
      active_table_query <- reactive(if(nchar(input$smp_id) > 0){
        paste0(
        "SELECT * FROM fieldwork.active_deployments
            WHERE smp_id = ", rv$smp_id(), " ORDER BY deployment_dtime_est")
      }else{
        paste0(
          "SELECT * FROM fieldwork.active_deployments
            WHERE site_name = '", input$site_name, "' ORDER BY deployment_dtime_est")
      })
      
      #create table as a reactive value based on query
      rv$active_table_db <- reactive(odbc::dbGetQuery(poolConn, active_table_query())%>% 
                                       mutate(across("download_error", 
                                                 ~ case_when(. == 1 ~ "Yes", 
                                                                . == 0 ~ "No"))))
      
      #select columns to show in app, and rename
      rv$active_table <- reactive(rv$active_table_db() %>% 
                                    mutate(across(where(is.POSIXct), trunc, "days")) %>% 
                                    mutate(across(where(is.POSIXlt), as.character)) %>% 
                                    dplyr::select(deployment_dtime_est, ow_suffix, type, term, research, interval_min, sensor_serial, date_80percent, date_100percent) %>% 
                                    dplyr::rename("Deploy Date" = "deployment_dtime_est", 
                                                  "Location" = "ow_suffix", "Purpose" = "type", "Term" = "term",
                                                  "Research" = "research", "Interval (min)" = "interval_min", "Sensor ID" = "sensor_serial",
                                                  "80% Full Date" = "date_80percent", "100% Full Date" = "date_100percent"))
      
      #when a row in active deployments table is clicked
      observeEvent(input$current_deployment_rows_selected, {
        #deselect from other tables
        dataTableProxy('prev_deployment') %>% selectRows(NULL)
        dataTableProxy('future_deployment') %>% selectRows(NULL)
      })
      
      #render Datatable
      output$current_deployment <- renderDT(
        rv$active_table(),
        selection = "single",
        style = 'bootstrap', 
        class = 'table-responsive, table-condensed',
        options = list(dom = 'tp')
      )
      
      #query for previous deployments
      old_table_query <- reactive(if(nchar(input$smp_id) > 0){
        paste0(
          "SELECT * FROM fieldwork.previous_deployments
            WHERE smp_id = ", rv$smp_id(), " ORDER BY deployment_dtime_est desc")
      }else{
        paste0(
          "SELECT * FROM fieldwork.previous_deployments
            WHERE site_name = '", input$site_name, "' ORDER BY deployment_dtime_est desc")
      })
      
      
      #create table as a reactive value based on query
      rv$old_table_db <- reactive(odbc::dbGetQuery(poolConn, old_table_query()))
      
      #select key fields and rename
      rv$old_table <- reactive(rv$old_table_db() %>% 
                                 mutate(across("download_error", 
                                               ~ case_when(. == 1 ~ "Yes", 
                                                           . == 0 ~ "No"))) %>% 
                                 mutate(across(where(is.POSIXct), trunc, "days")) %>% 
                                 mutate(across(where(is.POSIXlt), as.character)) %>% 
                                 dplyr::select(deployment_dtime_est, collection_dtime_est, ow_suffix, type, term, research, interval_min, sensor_serial) %>% 
                                 dplyr::rename("Deploy Date" = "deployment_dtime_est", "Location" = "ow_suffix", 
                                               "Purpose" = "type", "Term" = "term", "Research" = "research",
                                               "Interval (min)" = "interval_min", "Sensor ID" = "sensor_serial", 
                                               "Collection Date" = "collection_dtime_est"))
      
      #render datatable
      output$prev_deployment <- renderDT(
        rv$old_table(),
        selection = "single",
        style = 'bootstrap', 
        class = 'table-responsive, table-condensed', 
        options = list(dom = 'tp')
      )
      
      #query future table based on smp_id or site_name
      future_table_query <- reactive(if(nchar(input$smp_id) > 0){
        paste0(
        "SELECT * FROM fieldwork.future_deployments_full
         WHERE smp_id = ", rv$smp_id(), " ORDER BY ow_suffix asc")
      }else{
        paste0(
          "SELECT * FROM fieldwork.future_deployments_full
         WHERE site_name = '", input$site_name, "' ORDER BY ow_suffix asc")
      })
      
      rv$future_table_db <- reactive(odbc::dbGetQuery(poolConn, future_table_query()))
      
      #select key fields and rename
      rv$future_table <- reactive(rv$future_table_db() %>% 
                                    mutate(across(where(is.POSIXct), trunc, "days")) %>% 
                                    mutate(across(where(is.POSIXlt), as.character)) %>% 
                                    dplyr::select(ow_suffix, type, term, research, interval_min, premonitoring_inspection, field_test_priority) %>% 
                                    dplyr::rename("Location" = "ow_suffix", 
                                                  "Purpose" = "type", "Term" = "term", "Research" = "research",
                                                  "Interval (min)" = "interval_min", "Pre-Monitoring Inspection Date" = "premonitoring_inspection", 
                                                  "Priority" = "field_test_priority"))
      
      #render datatable for future deployments
      output$future_deployment <- renderDT(
        rv$future_table(), 
        selection = "single", 
        style = 'bootstrap', 
        class = 'table-responsive, table-condensed', 
        options = list(dom = 'tp')
      )
      
      #shorten name of selected rows from active and prev deployments tables
      rv$active <- reactive(input$current_deployment_rows_selected) 
      rv$prev <- reactive(input$prev_deployment_rows_selected)
      rv$future <- reactive(input$future_deployment_rows_selected)
      
      #define inputs ---------
      #define inputs, based on whether previous or active deployments tables are selected 
      #this was in two separate observeEvent calls, but changed to try to address the issue where collection date is sometimes blank when it shouldn't be
      #that was not resolved - I believe the issue is related to having two dateInputs update at the same time
      #essentialy - if an active row is selected, get the field from the active table
      #if prev row is selected, get the field from the prev table
      #if future row is selected, get the field from the future table
      rv$well_name <- reactive(if(length(rv$active()) > 0){
        rv$active_table_db()$ow_suffix[rv$active()]
      }else if(length(rv$prev()) > 0){
        rv$old_table_db()$ow_suffix[rv$prev()]
      }else if(length(rv$future()) > 0){
        rv$future_table_db()$ow_suffix[rv$future()]
      })
      
      rv$sensor_id <- reactive(if(length(rv$active()) > 0){
        rv$active_table_db()$sensor_serial[rv$active()] 
      }else if(length(rv$prev()) > 0){
        rv$old_table_db()$sensor_serial[rv$prev()]
      }else if(length(rv$future()) > 0){
        rv$future_table_db()$sensor_serial[rv$future()]
      })
      
      rv$sensor_purpose <- reactive(if(length(rv$active()) > 0){
        rv$active_table()$`Purpose`[rv$active()] 
      }else if(length(rv$prev()) > 0){
        rv$old_table()$`Purpose`[rv$prev()]
      }else if(length(rv$future()) > 0){
        rv$future_table()$`Purpose`[rv$future()]
      })
      
      rv$term_step <- reactive(if(length(rv$active()) > 0){
        rv$active_table()$`Term`[rv$active()] 
      }else if(length(rv$prev()) > 0){
        rv$old_table()$`Term`[rv$prev()]
      }else if(length(rv$future()) > 0){
        rv$future_table()$`Term`[rv$future()]
      })
      
      rv$research_step <- reactive(if(length(rv$active()) > 0){
        rv$active_table()$`Research`[rv$active()] 
      }else if(length(rv$prev()) > 0){
        rv$old_table()$`Research`[rv$prev()]
      }else if(length(rv$future()) > 0){
        rv$future_table()$`Research`[rv$future()]
      })
      
      rv$mea_int <- reactive(if(length(rv$active()) > 0){
        rv$active_table_db()$interval_min[rv$active()] 
      }else if(length(rv$prev()) > 0){
        rv$old_table_db()$interval_min[rv$prev()]
      }else if(length(rv$future()) > 0){
        rv$future_table_db()$interval_min[rv$future()]
      })
      
      rv$deploy_date <- reactive(if(length(rv$active()) > 0){
        rv$active_table_db()$deployment_dtime_est[rv$active()] 
      }else if(length(rv$prev()) > 0){
        rv$old_table_db()$deployment_dtime_est[rv$prev()]
      }else if(length(rv$future()) > 0){
        NA
      })
      
      rv$collect <- reactive(if(length(rv$active()) > 0){
        NA 
      }else if(length(rv$prev()) > 0){
        rv$old_table_db()$collection_dtime_est[rv$prev()]
      }else if(length(rv$future()) > 0){
        NA
      })
      
      rv$notes_step <- reactive(if(length(rv$active()) > 0){
        rv$active_table_db()$notes[rv$active()] 
      }else if(length(rv$prev()) > 0){
        rv$old_table_db()$notes[rv$prev()]
      }else if(length(rv$future()) > 0){
        rv$future_table_db()$notes[rv$future()]
      }) 
      
      #priority is NULL if it's not from the future table
      rv$priority_step <- reactive(if(length(rv$active()) > 0){
        NULL 
      }else if(length(rv$prev()) > 0){
        NULL
      }else if(length(rv$future()) > 0){
        rv$future_table_db()$field_test_priority[rv$future()]
      }) 
      
      #download error is NA if it's a future deployment
      rv$download_error_step <- reactive(if(length(rv$active()) > 0){
        rv$active_table_db()$download_error[rv$active()] 
      }else if(length(rv$prev()) > 0){
        rv$old_table_db()$download_error[rv$prev()]
      }else if(length(rv$future()) > 0){
        NA
      }) 
      
      #depth to water is NULL if future
      rv$deploy_depth_to_water_step <- reactive(if(length(rv$active()) > 0){
        rv$active_table_db()$deployment_dtw_or_depth_ft[rv$active()]
      }else if(length(rv$future()) > 0){
        NULL
      }else if(length(rv$prev()) > 0){
        rv$old_table_db()$deployment_dtw_or_depth_ft[rv$prev()]
      }
      )
      
      #collection depth to water is null if active or future
      rv$collect_depth_to_water_step <- reactive(if(length(rv$active()) > 0){
        NULL
      }else if(length(rv$future()) > 0){
        NULL
      }else if(length(rv$prev()) > 0){
        rv$old_table_db()$collection_dtw_or_depth_ft[rv$prev()]
      }
      )
      
      rv$premonitoring_step <- reactive(if(length(rv$active()) >0){
        NULL
      }else if(length(rv$future()) > 0){
        rv$future_table_db()$premonitoring_inspection[rv$future()]
      }else if(length(rv$prev()) > 0){
        NULL
      })
      
      rv$ready_step <- reactive(if(length(rv$active()) > 0){
        NULL
      }else if(length(rv$future()) > 0){
        rv$future_table_db()$ready[rv$future()]
      }else if(length(rv$prev()) > 0){
        NULL
      })
      
      #update inputs based on the reactive value definitions above
      observe({
        updateSelectInput(session, "well_name", selected = rv$well_name())
        updateSelectInput(session, "sensor_id", selected = rv$sensor_id())
        updateSelectInput(session, "sensor_purpose", selected = rv$sensor_purpose())
        updateSelectInput(session, "term", selected = rv$term_step())
        updateSelectInput(session, "research", selected = rv$research_step())
        updateSelectInput(session, "interval", selected = rv$mea_int())
        updateDateInput(session, "deploy_date", value = rv$deploy_date())
        updateDateInput(session, "collect_date", value = rv$collect())
        updateSelectInput(session, "priority", selected = rv$priority_step())
        updateTextAreaInput(session, "notes", value = rv$notes_step())
        updateSelectInput(session, "download_error", selected = rv$download_error_step())
        updateNumericInput(session, "deploy_depth_to_water", value = rv$deploy_depth_to_water_step())
        updateNumericInput(session, "collect_depth_to_water", value = rv$collect_depth_to_water_step())
        updateDateInput(session, "premonitoring_date", value = rv$premonitoring_step())
        updateSelectInput(session, "ready", selected = rv$ready_step())
        reset("redeploy")
        reset("sensor_broken")
        reset("new_sensor_id")
      })
      
      #-----
      #when a row in the previous deployments table is clicked
      observeEvent(input$prev_deployment_rows_selected, {
        #deselect from other table
        dataTableProxy('current_deployment') %>% selectRows(NULL)
        dataTableProxy('future_deployment') %>% selectRows(NULL)
      })
      
      #when a row in future deployments table is clicked
      observeEvent(input$future_deployment_rows_selected, {
        #deselect from other tables
        dataTableProxy('prev_deployment') %>% selectRows(NULL)
        dataTableProxy('current_deployment') %>% selectRows(NULL)
      })
      
      
      #control for redeploy checkbox. had to add, because after checking it, then removing collect_date, it goes away, but is still TRUE
      rv$redeploy <- reactive(if(length(input$collect_date > 0) & input$redeploy == TRUE) TRUE else FALSE)
      
      # observeEvent(input$collect_date, {
      #   reset('download_error')
      # })
      
      
      #check if the are current measurements at monitoring location (if it is not an SW or DL)
      rv$end_dates_at_ow_uid <- reactive(
        if(nchar(input$smp_id) > 0 & nchar(input$well_name) > 0){
          odbc::dbGetQuery(poolConn, paste0("select count(end_dtime_est) from fieldwork.ow_plus_measurements 
                                                       where smp_id = '", input$smp_id, "' and ow_suffix = '", input$well_name, "' and
                                          well_measurements_uid is not null")) %>% pull()
        }else if(nchar(input$site_name) > 0 & nchar(input$well_name) > 0){
          odbc::dbGetQuery(poolConn, paste0("select count(end_dtime_est) from fieldwork.ow_plus_measurements 
                                                       where site_name = '", input$site_name, "' and ow_suffix = '", input$well_name, "' and
                                          well_measurements_uid is not null")) %>%  pull()
        }
      )
      
      #count how many well measurements are at the monitoring location
      rv$count_at_ow_uid <- reactive(
        if(nchar(input$smp_id) > 0 & nchar(input$well_name) > 0){
          odbc::dbGetQuery(poolConn, paste0("select count(*) from fieldwork.ow_plus_measurements 
                                                       where smp_id = '", input$smp_id, "' and ow_suffix = '", input$well_name, "' and
                                          well_measurements_uid is not null")) %>% pull()
        }else if(nchar(input$site_name) > 0 & nchar(input$well_name) > 0){
          odbc::dbGetQuery(poolConn, paste0("select count(*) from fieldwork.ow_plus_measurements 
                                                       where site_name = '", input$site_name, "' and ow_suffix = '", input$well_name, "' and
                                          well_measurements_uid is not null")) %>%  pull()
        }
      )
      
      #true of false if # of end dates == counts
      rv$complete_end_dates <- reactive(
        if(rv$count_at_ow_uid() == 0 | rv$end_dates_at_ow_uid() == rv$count_at_ow_uid()){
          FALSE
        }else{
          TRUE
        }
      )
      
      #if there is an end date, look for measurements with an overlap of that deployment
      rv$count_end_date <- reactive(
        odbc::dbGetQuery(poolConn, paste0("select count(*) from fieldwork.ow_plus_measurements
                                          where smp_id = '", input$smp_id, "' and ow_suffix = '", input$well_name, "' and 
                                          well_measurements_uid is not null and start_dtime_est <= '", input$deploy_date, "'
                                          and end_dtime_est is null or end_dtime_est >= '", input$collect_date, "'")) %>% pull()
      )
      
      #check if the well is a Shallow Well or Datalogger - they do not require measurements so we don't want a popup
      rv$sw_or_dl <- reactive(
        if(str_detect(input$well_name, "^SW") == TRUE | str_detect(input$well_name, "^DL") == TRUE){
          TRUE
        }else{
          FALSE
        }
      )
      
      #tell whether you are adding new deployments or future deployments, or editing
      rv$add_new <- reactive((length(input$prev_deployment_rows_selected) == 0 & length(input$current_deployment_rows_selected) == 0))
      rv$add_new_future <- reactive(length(input$future_deployment_rows_selected) == 0)
      
      #Create a modal title depending on the last criteria
      rv$modal_title <- reactive(if(rv$add_new()) "Add New Deployment" else "Edit Deployment")
      
      #Change modal text depending on logicals
      rv$modal_text <- reactive(if(rv$add_new() & input$well_name %in% rv$future_table_db()$ow_suffix & rv$add_new_future()){
        "There is already a future deployment at this location. Add new deployment at same location without removing future deployment?"
      }else if(rv$add_new() & input$well_name %in% rv$future_table_db()$ow_suffix & rv$add_new_future() == FALSE){
        "Add New Deployment and Remove Future Deployment?"
      }else if(rv$add_new() & rv$redeploy() & input$sensor_broken != TRUE){
        "Add New Deployment and Redeploy Same Sensor?"
      }else if(rv$add_new() & rv$redeploy() & input$sensor_broken == TRUE){
        paste0("Add New Deployment with Sensor ", input$sensor_id, ", Mark Sensor ", input$sensor_id, " As Out of Service, and Deploy Sensor ", input$new_sensor_id, "?")
      }else if(rv$add_new() & rv$redeploy() == FALSE){
        "Add New Deployment?"
      }else if(rv$add_new() == FALSE & rv$redeploy() == TRUE & input$sensor_broken != TRUE){
        "Edit Deployment and Redeploy Sensor?"
      }else if(rv$add_new() == FALSE & rv$redeploy() == TRUE & input$sensor_broken == TRUE){
        paste0("Edit Deployment, Mark Sensor ", input$sensor_id, " As Out of Service, and Deploy Sensor ", input$new_sensor_id, "?")
      }else if(rv$add_new() == FALSE & rv$redeploy() == FALSE & input$sensor_broken == TRUE){
        paste0("Edit Deployment and Mark Sensor ", input$sensor_id, " As Out of Service?")
      }else if(rv$add_new() == FALSE & rv$redeploy() == FALSE){
        "Edit Deployment?"
      })
      
      #relabel deploy button if editing
      rv$label <- reactive(if(rv$add_new()) "Add New Deployment" else "Edit Selected Deployment")
      
      observe(updateActionButton(session, "deploy_sensor", label = rv$label()))
      
      rv$update_deployment_uid <- reactive(if(length(input$current_deployment_rows_selected) > 0){
        rv$active_table_db()$deployment_uid[input$current_deployment_rows_selected]
      }else if(length(input$prev_deployment_rows_selected) > 0){
        rv$old_table_db()$deployment_uid[input$prev_deployment_rows_selected]
      }else{
        0
      }
      )
      
      rv$update_future_deployment_uid <- reactive(if(length(input$future_deployment_rows_selected) > 0){
        rv$future_table_db()$future_deployment_uid[input$future_deployment_rows_selected]
      }else{
        0
      }
      )
      
      #relabel future deploy button if editing
      rv$future_label <- reactive(if(rv$add_new_future()) "Add Future Deployment" else "Edit Selected Future Deployment")
      
      observe(updateActionButton(session, "future_deploy", label = rv$future_label()))
      
      #write to database on click
      #go through dialog box to confirm action
      observeEvent(input$deploy_sensor, {
        showModal(modalDialog(title = rv$modal_title(), 
                              rv$modal_text(), 
                              modalButton("No"), 
                              actionButton(ns("confirm_deploy"), "Yes")))
      })
      
      #get sensor issue lookup uid and let it be NULL (for issue #1 and issue #2)
      rv$issue_lookup_uid_one <- reactive(sensor_issue_lookup %>% dplyr::filter(sensor_issue == input$issue_one) %>% 
                                            select(sensor_issue_lookup_uid) %>% pull())
      
      rv$sensor_issue_lookup_uid_one <- reactive(if(nchar(input$issue_one) == 0) "NULL" else paste0("'", rv$issue_lookup_uid_one(), "'"))
      
      rv$issue_lookup_uid_two <- reactive(sensor_issue_lookup %>% dplyr::filter(sensor_issue == input$issue_two) %>% 
                                            select(sensor_issue_lookup_uid) %>% pull())
      
      rv$sensor_issue_lookup_uid_two <- reactive(if(nchar(input$issue_two) == 0) "NULL" else paste0("'", rv$issue_lookup_uid_two(), "'"))
      
      #let checkbox input be NULL if blank (instead of FALSE)
      rv$request_data <- reactive(if(input$request_data == TRUE) paste0("'TRUE'") else "NULL")
      
      #create tickers that update to notify other modules of updates
      rv$refresh_collect <- 0 
      rv$refresh_sensor <- 0
      
      observeEvent(input$confirm_deploy, {
        if(rv$add_new()){
          #write new deployment
          odbc::dbGetQuery(poolConn,
                           paste0("INSERT INTO fieldwork.deployment (deployment_dtime_est, ow_uid,
         inventory_sensors_uid, sensor_purpose, long_term_lookup_uid, research_lookup_uid, interval_min, collection_dtime_est, notes, download_error, deployment_dtw_or_depth_ft, collection_dtw_or_depth_ft)
            VALUES ('", input$deploy_date, "', fieldwork.get_ow_uid(",rv$smp_id(),", '", input$well_name, "', ", rv$site_name_lookup_uid(), "), ",
                                  rv$inventory_sensors_uid_null(), ",'", rv$purpose(), "','", rv$term(), "',", rv$research_lookup_uid(), ",'",input$interval, "',", rv$collect_date(),",", rv$notes(),",", rv$download_error(), ", ", rv$deploy_depth_to_water(), ", ", rv$collect_depth_to_water(), ")"))
        }else{
          #update existing deployment
          odbc::dbGetQuery(poolConn, 
                           paste0("UPDATE fieldwork.deployment SET deployment_dtime_est = '", input$deploy_date, "', 
                           ow_uid = fieldwork.get_ow_uid(",rv$smp_id(),", '", input$well_name, "', ", rv$site_name_lookup_uid(), "), 
                                  inventory_sensors_uid = ",  rv$inventory_sensors_uid_null(), ", 
                                  sensor_purpose = '", rv$purpose(), "',
                                  long_term_lookup_uid = '", rv$term(), "',
                                  research_lookup_uid = ", rv$research_lookup_uid(), ",
                                  interval_min = '", input$interval, "',
                                  collection_dtime_est = ", rv$collect_date(), ",
                                  notes = ", rv$notes(), ", 
                                  download_error = ", rv$download_error(), ", 
                                  deployment_dtw_or_depth_ft = ", rv$deploy_depth_to_water(), ", 
                                  collection_dtw_or_depth_ft = ", rv$collect_depth_to_water(), " WHERE 
                                  deployment_uid = '", rv$update_deployment_uid(), "'"
                           ))
        }
        
        #write sensor status
        if(input$sensor_broken == TRUE){
          dbGetQuery(poolConn, paste0("UPDATE fieldwork.inventory_sensors SET sensor_status_lookup_uid = '2', 
                                            sensor_issue_lookup_uid_one = ", rv$sensor_issue_lookup_uid_one(), ",
                                            sensor_issue_lookup_uid_two = ", rv$sensor_issue_lookup_uid_two(), ", 
                                            request_data = ", rv$request_data(), "
                                      WHERE sensor_serial = '", input$sensor_id, "'"))
          rv$refresh_sensor <- rv$refresh_sensor + 1
        }
        #write redeployment
        if(rv$redeploy() == TRUE){
          dbGetQuery(poolConn, paste0("INSERT INTO fieldwork.deployment (deployment_dtime_est, ow_uid,
         inventory_sensors_uid, sensor_purpose, long_term_lookup_uid, research_lookup_uid, interval_min, notes, deployment_dtw_or_depth_ft)
            VALUES (", rv$collect_date(), ", fieldwork.get_ow_uid(",rv$smp_id(),", '", input$well_name, "', ", rv$site_name_lookup_uid(), "), '",
                                      rv$new_inventory_sensors_uid(), "','", rv$purpose(), "','", rv$term(), "',", rv$research_lookup_uid(), 
                                      ",'",input$interval, "', ", rv$redeployment_notes(), ", ", rv$collect_depth_to_water(), ")"))
        }
        
        #delete from future table when a future deployment is converted to current
        if(!rv$add_new_future()){
          odbc::dbGetQuery(poolConn, paste0("DELETE FROM fieldwork.future_deployment WHERE future_deployment_uid = '", rv$update_future_deployment_uid(), "'"))
        }
        
        #query active table
        rv$active_table_db  <- reactive(odbc::dbGetQuery(poolConn, active_table_query())) 
        #query prev table
        rv$old_table_db <- reactive(odbc::dbGetQuery(poolConn, old_table_query())) 
        #query future table
        rv$future_table_db <- reactive(odbc::dbGetQuery(poolConn, future_table_query()))
        
        #query collection table
        rv$refresh_collect <- rv$refresh_collect + 1
        #rv_collect$collect_table_db <- odbc::dbGetQuery(poolConn, collect_query)
        
        #remove pop up
        removeModal()
        
        #if you need to add a current deployment measurement:
        if(length(input$collect_date) > 0 & rv$sw_or_dl() == FALSE){
          if(rv$count_end_date() == 0){
            showModal(modalDialog(title = "Check Measurements", 
                                  "This monitoring location does not have measurements form this period. Would you like to add?", 
                                  modalButton("No"), 
                                  actionButton(ns("confirm_add_measurements"), "Yes")))
          }
        }
        if(rv$complete_end_dates() == FALSE & rv$sw_or_dl() == FALSE & length(input$collect_date) == 0){
          showModal(modalDialog(title = "Check Measurements", 
                                "This monitoring location does not have current measurements. Would you like to add?", 
                                modalButton("No"), 
                                actionButton(ns("confirm_add_measurements"), "Yes")))
        #if you need to add an end date when a sensor is not being redeployed
        }else if(rv$redeploy() == FALSE & rv$collect_date() != "NULL" & rv$complete_end_dates() == TRUE & rv$sw_or_dl() == FALSE){
          showModal(modalDialog(title = "Check Measurements", 
                                "Did you collect hardware? Would you like to add an end date to these deployment measurements?", 
                                modalButton("No"), 
                                actionButton(ns("confirm_add_end_date"), "Yes")))
        }else{
          #clear entries
          dataTableProxy('current_deployment') %>% selectRows(NULL)
          dataTableProxy('prev_deployment') %>% selectRows(NULL)
          reset("sensor_id")
          reset("sensor_purpose")
          reset("term")
          reset("research")
          reset("interval")
          reset("deploy_date")
          reset("collect_date")
          reset("well_name")
          reset("download_error")
          reset("redeploy")
          reset("sensor_broken")
          reset("new_sensor_id")
          reset("deploy_depth_to_water")
          reset("collect_depth_to_water")
          reset("notes")
        }
      })
      
      #future deployments ---------
      observeEvent(input$premonitoring_date, {
        if(length(input$premonitoring_date) > 0){
          
          rv$cet_asset_query <- reactive(paste0("SELECT count(*) FROM smpid_facilityid_componentid_inlets_limited WHERE smp_id = '", input$smp_id, "' AND component_id != 'NULL'"))
          
          rv$cet_count_query <- reactive(paste0("select count(*) from fieldwork.capture_efficiency_full 
                                              where component_to_smp(component_id) = ", rv$smp_id(), "
                                              and test_date < ", rv$premonitoring_date(), "::timestamp + interval '1 day' 
                                              and test_date > ", rv$premonitoring_date(), "::timestamp - interval '30 days'" ))
          
          rv$cet_asset_count <- reactive(dbGetQuery(poolConn, rv$cet_asset_query()) %>% pull())
          
          #print(rv$cet_asset_count())
          
          #print(rv$cet_count_query())
          
          rv$cet_count <- reactive(dbGetQuery(poolConn, rv$cet_count_query()) %>% pull())
          
          #print(rv$cet_count())
          
          cet_good <- rv$cet_asset_count() == rv$cet_count()
          
          if(cet_good == FALSE){
            showModal(modalDialog(title = "Add Capture Efficiency Test", 
                                  "Add capture efficiency tests from the pre-monitoring inspection?", 
                                  modalButton("No"), 
                                  actionButton(ns("add_cet"), "Yes")))
            disable("ready")
          }else{
            enable("ready")
          }
        }
      })
      
      
      #write or edit a future deployment
      observeEvent(input$future_deploy, {
        if(rv$add_new_future()){
          odbc::dbGetQuery(poolConn,
                           paste0("INSERT INTO fieldwork.future_deployment (ow_uid, inventory_sensors_uid, sensor_purpose,
    			interval_min, long_term_lookup_uid, research_lookup_uid, notes, field_test_priority_lookup_uid, premonitoring_inspection, ready)
    			VALUES (fieldwork.get_ow_uid(",rv$smp_id(),", '", input$well_name, "', ", rv$site_name_lookup_uid(), "), ", rv$inventory_sensors_uid_null(),
                                  ", ", rv$purpose_null(), ", ", rv$interval_min(), ", ", rv$term_null(),
                                  ", ", rv$research_lookup_uid(), ", ", rv$notes(), ", ", rv$priority_lookup_uid(), 
                                  ", ", rv$premonitoring_date(), ", ", rv$ready(), ")"))
        }else{
          odbc::dbGetQuery(poolConn, 
                           paste0("UPDATE fieldwork.future_deployment SET 
                           	ow_uid = fieldwork.get_ow_uid(",rv$smp_id(),", '", input$well_name, "', ", rv$site_name_lookup_uid(), "), 
                                  inventory_sensors_uid = ",  rv$inventory_sensors_uid_null(), ", 
                                  sensor_purpose = ", rv$purpose_null(), ",
                                  long_term_lookup_uid = ", rv$term_null(), ",
                                  research_lookup_uid = ", rv$research_lookup_uid(), ",
                                  interval_min = ", rv$interval_min() , ",
                                  notes = ", rv$notes(), ", 
                                  field_test_priority_lookup_uid = ", rv$priority_lookup_uid(), ", 
                                  premonitoring_inspection = ", rv$premonitoring_date(), ", 
                                  ready = ", rv$ready(), " WHERE 
                                  future_deployment_uid = '", rv$update_future_deployment_uid(), "'" 
                           ))
        }
        
        rv$future_table_db <- reactive(odbc::dbGetQuery(poolConn, future_table_query()))
        rv$refresh_collect <- rv$refresh_collect + 1
      })
      
      #set up for editing CET
      rv$refresh_cet <- 0
      
      observeEvent(input$add_cet, {
        rv$refresh_cet <- rv$refresh_cet + 1
        updateTabsetPanel(session = parent_session, "inTabset", selected = "cet_tab")
        removeModal()
      })
      
      
      
      #delete a future deployment
      #first, intermediate dialog box
      observeEvent(input$delete_future, {
        showModal(modalDialog(title = "Delete Future Deployment", 
                              "Delete Future Deployment?", 
                              modalButton("No"), 
                              actionButton(ns("confirm_delete_future"), "Yes")))
      })
      
      observeEvent(input$confirm_delete_future, {
        odbc::dbGetQuery(poolConn, 
                         paste0("DELETE FROM fieldwork.future_deployment WHERE future_deployment_uid = '", rv$update_future_deployment_uid(), "'"))
        
        rv$future_table_db <- reactive(odbc::dbGetQuery(poolConn, future_table_query()))
        rv$refresh_collect <- rv$refresh_collect + 1
        #remove pop up
        removeModal()
      })
      
      #update ticker so other modules update
      rv$refresh_location <- 0
      
      #update ticker and tab for measurements
      observeEvent(input$confirm_add_measurements, {
        rv$refresh_location <- rv$refresh_location + 1
        updateTabsetPanel(session = parent_session, "inTabset", selected = "add_ow")
        removeModal()
      })
      
      #update ticker and tab for measurements
      observeEvent(input$confirm_add_end_date, {
        rv$refresh_location <- rv$refresh_location + 1
        updateTabsetPanel(session = parent_session, "inTabset", selected = "add_ow")
        removeModal()
      })
      
      
      #enable/disable deploy sensor button based on whether all inputs (except collect date) are not empty
      #logical to add to the toggleState below
      #require "download error" if collection date is not 0
      rv$req_dl_error <- reactive(if(length(input$collect_date) == 0){
        TRUE
      }else if(nchar(input$download_error) > 0){
        TRUE
      }else{
        FALSE
      })
      
      #set sensor purpose to datalogger and disable sensor id if a "DL" is selected
      observeEvent(input$well_name, {
        if(startsWith(input$well_name, "DL")){
          updateSelectInput(session, "sensor_purpose", selected = "DATALOGGER")
          updateSelectInput(session, "sensor_id", selected = NULL)
          disable("sensor_id")
        }else{
          enable("sensor_id")
        }
      })
      
      #sensor warning
      rv$sensor_warning <- reactive(if(input$sensor_id %in% collect$sensor_serial() & length(input$collect_date) == 0){
        "Sensor is deployed at another location. Search Sensor ID in \"Add Sensor\" tab for more info."
      }else{
        NULL
      })
      
      output$sensor_warning <- renderText(
        rv$sensor_warning()
      )
      
      #new sensor warning
      rv$new_sensor_warning <- reactive(if(input$new_sensor_id %in% collect$sensor_serial()){
        "Sensor is deployed at another location. Search Sensor ID in \"Add Sensor\" tab for more info."
      }else{
        NULL
      })
      
      output$new_sensor_warning <- renderText(
        rv$new_sensor_warning()
      )
      
      #check that if sensor is broken, and you are redeploying, a new sensor is specified to activate button
      rv$sensor_break <- reactive(if(input$sensor_broken != TRUE){
        TRUE
      }else if(input$sensor_broken == TRUE & 
               (input$redeploy != TRUE | (nchar(input$new_sensor_id) > 0 & !(input$new_sensor_id %in% collect$sensor_serial())))){
        TRUE
      }else if(input$sensor_broken == TRUE & nchar(input$new_sensor_id == 0)){
        FALSE
      })
      
      #also check to make sure the sensor is not already deployed, or the sensor has a collection date, or a row selected
      observe({toggleState(id = "deploy_sensor", condition = (nchar(input$smp_id) > 0 | nchar(input$site_name) > 0) &
                             nchar(input$well_name) > 0 & (nchar(input$sensor_id) > 0 | input$sensor_purpose == "DATALOGGER") & 
                             nchar(input$sensor_purpose) > 0 &
                             nchar(input$interval) > 0 & length(input$deploy_date) > 0 & nchar(input$term) > 0 &
                             rv$req_dl_error() & rv$sensor_break() &
                             (!(input$sensor_id %in% collect$sensor_serial()) |
                                (length(input$collect_date) > 0 & rv$redeploy() == TRUE & !(input$sensor_id %in% collect$sensor_serial())) |
                                (length(input$collect_date) >0 & rv$redeploy() == FALSE) |
                                length(input$current_deployment_rows_selected) > 0 |
                                length(input$prev_deployment_rows_selected) > 0))})
      
      #toggle future deployment add state
      # 
      # rv$toggle_ready <- reactive(if(length(input$premonitoring_date) == 0){
      #   TRUE
      # }else if(length(input$ready) > 0){
      #   TRUE
      # }else if(length(input$ready) == 0){
      #   FALSE
      # })
      
      observe({toggleState(id = "future_deploy", condition = (nchar(input$smp_id) > 0 | nchar(input$site_name) > 0) &
                             nchar(input$well_name) > 0 & nchar(input$ready) > 0) })
      
      #toggle future deployment delete button
      observe({toggleState(id = "delete_future", condition = length(rv$future()) > 0 )})
      
      #clear all fields
      #bring up dialogue box to confirm
      observeEvent(input$clear_deploy_fields, {
        showModal(modalDialog(title = "Clear All Fields", 
                              "Are you sure you want to clear all fields on this tab?", 
                              modalButton("No"), 
                              actionButton(ns("confirm_clear_deploy"), "Yes")))
      })
      
      observeEvent(input$confirm_clear_deploy, {
        reset("sensor_id")
        reset("sensor_purpose")
        reset("interval")
        reset("term")
        reset("research")
        reset("deploy_date")
        reset("collect_date")
        reset("smp_id")
        reset("well_name")
        reset("download_error")
        reset("redeploy")
        reset("sensor_broken")
        reset("new_sensor_id")
        reset("deploy_depth_to_water")
        reset("collect_depth_to_water")
        reset("notes")
        removeModal()
      })
      
      #return variables that will be passed into other modules
      #the "refresh" variables trigger updates in the other modules
      return(
        list(
          refresh_collect = reactive(rv$refresh_collect),
          refresh_sensor = reactive(rv$refresh_sensor),
          refresh_location = reactive(rv$refresh_location),
          refresh_cet = reactive(rv$refresh_cet),
          smp_id_check = reactive(rv$smp_id()),
          smp_id = reactive(input$smp_id),
          site_name = reactive(input$site_name),
          location = reactive(input$well_name), 
          system_id = reactive(gsub('-\\d+$', '', input$smp_id))
        )
      )
    }
  )
}