
deployUI <- function(id, label = "deploy", smp_id, sensor_serial, site_names, html_req, long_term_lookup, deployment_lookup, research_lookup, priority, future_req){
  ns <- NS(id)
  list(
  tabPanel("Deploy Sensor", value = "deploy_tab",
           titlePanel("Deploy Sensor"), 
           fluidRow(
             column(width = 4, 
                    sidebarPanel(width = 12, 
                                 fluidRow(
                                   column(6,selectInput(ns("smp_id"), future_req(html_req("SMP ID")), choices = c("", smp_id), selected = NULL)), 
                                   column(6,selectInput(ns("site_name"), future_req(html_req("Site Name")), choices = c("", site_names), selected = NULL))),
                                 fluidRow(
                                   column(6,selectInput(ns("well_name"), future_req(html_req("Location")), choices = "")), 
                                   column(6,selectInput(ns("sensor_id"), html_req("Sensor ID"), choices = c("", sensor_serial), selected = NULL))),
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
                                 conditionalPanel(condition = "input.deploy_date === null", 
                                                  ns = ns, 
                                                  selectInput(ns("priority"), "Future Test Priority", 
                                                              choices = c("", priority$field_test_priority), selected = NULL)),
                                 #ask about download error and redeploying sensor once you add a collection date
                                 conditionalPanel(width = 12, 
                                                  condition = "input.deploy_date", 
                                                  ns = ns, 
                                                  HTML("If well is dry, enter 0")),
                                 fluidRow(column(6,
                                   conditionalPanel(width = 12, 
                                                    condition = "input.deploy_date",
                                                    ns = ns,
                                                    numericInput(ns("deploy_depth_to_water"), "Deployment Depth-to-Water (ft)", value = NA, min = 0))),               
                                   column(6,               
                                 conditionalPanel(width = 12, 
                                                  condition = "input.collect_date",
                                                  ns = ns,
                                                  numericInput(ns("collect_depth_to_water"), "Collection Depth-to-Water (ft)", value = NA, min = 0),
                                 ))),
                                 conditionalPanel(width = 12, 
                                                  condition = "input.collect_date",
                                                  ns = ns,
                                                  selectInput(ns("download_error"), html_req("Did you encounter a download error?"), 
                                                              choices = c("", "No" = 0, "Yes" = 1), selected = NULL)),
                                 conditionalPanel(width = 12, 
                                                  condition = "input.download_error == '1'", 
                                                  ns = ns,
                                                  checkboxInput(ns("sensor_broken"), "Sensor Broken?")), 
                                 conditionalPanel(width = 12, 
                                                  condition = "input.collect_date",
                                                  ns = ns, 
                                                  checkboxInput(ns("redeploy"), "Redeploy?"),
                                                  h6("Redeploy a sensor in the same well on this collection date")), 
                                 conditionalPanel(width = 12, 
                                                  condition = "input.redeploy & input.sensor_broken", 
                                                  ns = ns, 
                                                  selectInput(ns("new_sensor_id"), html_req("New Sensor ID"), 
                                                              choices = c("", sensor_serial), selected = NULL),
                                                  textOutput(ns("new_sensor_warning"))),
                                 textAreaInput(ns("notes"), "Notes"),
                                 conditionalPanel(width = 12, 
                                                  condition = "input.redeploy", 
                                                  ns = ns, 
                                                  textAreaInput(ns("redeployment_notes"), "New Deployment Notes")),
                                 conditionalPanel(condition = "input.deploy_date === null", 
                                                  ns = ns, 
                                                  actionButton(ns("future_deploy"), "Add Future Deployment")),
                                 actionButton(ns("deploy_sensor"), "Deploy Sensor"), 
                                 actionButton(ns("clear_deploy_fields"), "Clear All Fields"),
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

deploy <- function(input, output, session, parent_session, ow, collect, sensor, poolConn, deployment_lookup){
  
  #define ns to use in modals
  ns <- session$ns
  
  #start reactiveValues for this section/tab
  rv <- reactiveValues()
  
  rv$site_name_lookup_uid_step <- reactive(odbc::dbGetQuery(poolConn, paste0("select site_name_lookup_uid from fieldwork.site_name_lookup where site_name = '", input$site_name, "'")) %>% pull())
  
  rv$site_name_lookup_uid <- reactive(if (nchar(input$site_name) > 0) paste0("'", rv$site_name_lookup_uid_step(), "'") else "NULL")
  rv$smp_id <- reactive(if (nchar(input$smp_id) > 0) paste0("'", input$smp_id, "'") else "NULL")
  
  ## well panel
  #update sensor id choices based on the hobo list
  observe(updateSelectInput(session, inputId = "sensor_id", choices = c("", sensor$sensor_serial()), selected = NULL))
  #query ow_suffixes based on smp_id
  rv$ow_suffixes <- reactive(odbc::dbGetQuery(poolConn, paste0(
    "select ow_suffix from fieldwork.ow_all where smp_id = ", rv$smp_id(), " OR site_name_lookup_uid = ", rv$site_name_lookup_uid())) %>% dplyr::pull())
  
  observe(updateSelectInput(session, "well_name", choices = c("", rv$ow_suffixes())))
  observe(updateDateInput(session, "collect_date", min = input$deploy_date))
  
  #upon clicking "deploy at this smp" in add_ow
  observeEvent(ow$refresh_deploy(), {
    #using shinyjs::reset is too slow and will reset after the selection is updated to the desired SMPID
    updateSelectInput(session, "smp_id", selected = NULL)
    updateSelectInput(session, "smp_id", selected = ow$smp_id())
  })
  
  #upon adding or removing a site in add_ow
  observeEvent(ow$site_name_db(), {
    updateSelectInput(session, "site_name", choices = c("", ow$site_name_db()$site_name))
  })
  
  #upon clicking "deploy this sensor" in add_sensor
  observeEvent(sensor$refresh_serial_no(), {
    updateSelectInput(session, "sensor_id", selected = sensor$serial_no())
  })
  
  #upon clicking a row in collection_calendar
  observeEvent(collect$deploy_refresh(), {
    updateSelectInput(session, "smp_id", selected = "")
    updateSelectInput(session, "site_name", selected = "")
    # need to get through the initial load where length == 0 (is.na does not work in that case)
    # go to either smp id or site name
    if(length(collect$smp_id()) > 0){
      if(!is.na(collect$smp_id())){
      updateSelectInput(session, "smp_id", selected = collect$smp_id())
      }else{
      updateSelectInput(session, "site_name", selected = collect$site_name())
      }
    }
  })
  
  observeEvent(rv$active_table_db(), {
    if(length(collect$rows_selected()) > 0){
      rv$active_row <- reactive(which(rv$active_table_db()$deployment_uid == collect$row(), arr.ind = TRUE))
      dataTableProxy('current_deployment') %>% selectRows(rv$active_row())
    }
  })
  
  #upon click a row in future table (in collection calendar)
  observeEvent(collect$future_deploy_refresh(), {
    #print(collect$future_smp_id())
    updateSelectInput(session, "smp_id", selected = "")
    updateSelectInput(session, "site_name", selected = "")
    if(length(collect$future_smp_id()) > 0){
      if(!is.na(collect$future_smp_id())){
        updateSelectInput(session, "smp_id", selected = "")
        updateSelectInput(session, "smp_id", selected = collect$future_smp_id())
      }else{
        updateSelectInput(session, "site_name", selected = collect$future_site_name())
      }
    }
  })
  
  observeEvent(rv$future_table_db(), {
    if(length(collect$future_rows_selected()) > 0){
      rv$future_row <- reactive(which(rv$future_table_db()$future_deployment_uid == collect$future_row(), arr.ind = TRUE))
      dataTableProxy('future_deployment') %>% selectRows(rv$future_row())
    }
  })
  
  #toggle to make sure that only of SMP ID or Site Name is selected
  observe(toggleState("smp_id", condition = nchar(input$site_name) == 0))
  observe(toggleState("site_name", condition = nchar(input$smp_id) == 0))
  
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
  
  #rv$term <- reactive(long_term_lookup %>% dplyr::filter(type == input$term) %>% 
  #                         select(long_term_lookup_uid) %>% pull())
  
  rv$term <- reactive(odbc::dbGetQuery(poolConn, paste0("Select long_term_lookup_uid FROM fieldwork.long_term_lookup WHERE type = '", input$term, "'")) %>% pull)
  
  rv$term_null <- reactive(if(nchar(input$term) == 0) "NULL" else paste0("'", rv$term(), "'"))
  
  rv$research <- reactive(odbc::dbGetQuery(poolConn, paste0("select research_lookup_uid FROM fieldwork.research_lookup WHERE type = '", input$research, "'")) %>%  pull)
  
  rv$notes_step1 <- reactive(gsub('\'', '\'\'', input$notes))
  rv$notes <- reactive(if(nchar(rv$notes_step1()) == 0) "NULL" else paste0("'", rv$notes_step1(), "'"))
  
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
  
  #get sensor id for redeployment 
  rv$new_sensor_id <- reactive(if(nchar(input$new_sensor_id) > 0){
    paste0("'", input$new_sensor_id, "'")
  }else{
    paste0("'", input$sensor_id, "'")
  }
  )
  
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
                                   mutate_at(vars(one_of("download_error")), 
                                             funs(case_when(. == 1 ~ "Yes", 
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
  rv$old_table_db <- reactive(odbc::dbGetQuery(poolConn, old_table_query())%>% 
                                mutate_at(vars(one_of("download_error")), 
                                          funs(case_when(. == 1 ~ "Yes", 
                                                         . == 0 ~ "No"))))
  
  rv$old_table <- reactive(rv$old_table_db() %>% 
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
  
  rv$future_table <- reactive(rv$future_table_db() %>% 
                                dplyr::select(ow_suffix, type, term, research, interval_min, field_test_priority) %>% 
                                dplyr::rename("Location" = "ow_suffix", 
                                              "Purpose" = "type", "Term" = "term", "Research" = "research",
                                              "Interval (min)" = "interval_min", "Priority" = "field_test_priority"))
  
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
  
  rv$download_error_step <- reactive(if(length(rv$active()) > 0){
    rv$active_table_db()$download_error[rv$active()] 
  }else if(length(rv$prev()) > 0){
    rv$old_table_db()$download_error[rv$prev()]
  }else if(length(rv$future()) > 0){
    NA
  }) 
  
  rv$deploy_depth_to_water_step <- reactive(if(length(rv$active()) > 0){
    rv$active_table_db()$deployment_dtw_or_depth_ft[rv$active()]
  }else if(length(rv$future()) > 0){
    NULL
  }else if(length(rv$prev()) > 0){
    rv$old_table_db()$deployment_dtw_or_depth_ft[rv$prev()]
  }
  )
  
  rv$collect_depth_to_water_step <- reactive(if(length(rv$active()) > 0){
    NULL
  }else if(length(rv$future()) > 0){
    NULL
  }else if(length(rv$prev()) > 0){
    rv$old_table_db()$collection_dtw_or_depth_ft[rv$prev()]
  }
  )
  
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
    updateTextAreaInput(session, "notes", value = rv$notes_step())
    updateSelectInput(session, "download_error", selected = rv$download_error_step())
    updateNumericInput(session, "deploy_depth_to_water", value = rv$deploy_depth_to_water_step())
    updateNumericInput(session, "collect_depth_to_water", value = rv$collect_depth_to_water_step())
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
  
  #define 
  rv$add_new <- reactive((length(input$prev_deployment_rows_selected) == 0 & length(input$current_deployment_rows_selected) == 0))
  rv$add_new_future <- reactive(length(input$future_deployment_rows_selected) == 0)
  rv$modal_title <- reactive(if(rv$add_new()) "Add New Deployment" else "Edit Deployment")
  rv$modal_text <- reactive(if(rv$add_new() & input$well_name %in% rv$future_table_db()$ow_suffix & rv$add_new_future()){
    "There is already a future deployment at this location. Add new deployment at same location without removing future deployment?"
  }else if(rv$add_new() & input$well_name %in% rv$future_table_db()$ow_suffix & rv$add_new_future() == FALSE){
    "Add New Deployment and Remove Future Deployment?"
  }else if(rv$add_new() & rv$redeploy()){
    "Add New Deployment and Redeploy Sensor?"
  }else if(rv$add_new() & rv$redeploy() == FALSE){
    "Add New Deployment?"
  }else if(rv$add_new() == FALSE & rv$redeploy() == TRUE & input$sensor_broken != TRUE){
    "Edit Deployment and Redeploy Sensor?"
  }else if(rv$add_new() == FALSE & rv$redeploy() == TRUE & input$sensor_broken == TRUE){
    paste0("Edit Deployment, Mark Sensor ", input$sensor_id, " As Broken, and Deploy Sensor ", input$new_sensor_id, "?")
  }else if(rv$add_new() == FALSE & rv$redeploy() == FALSE & input$sensor_broken == TRUE){
    paste0("Edit Deployment and Mark Sensor ", input$sensor_id, " As Broken?")
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
    
    if(input$sensor_broken == TRUE){
      dbGetQuery(poolConn, paste0("UPDATE fieldwork.inventory_sensors SET sensor_status_lookup_uid = '2'
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
    removeModal()
  })
  
  observeEvent(input$future_deploy, {
    if(rv$add_new_future()){
      odbc::dbGetQuery(poolConn,
                       paste0("INSERT INTO fieldwork.future_deployment (ow_uid, inventory_sensors_uid, sensor_purpose,
			interval_min, long_term_lookup_uid, research_lookup_uid, notes, field_test_priority_lookup_uid)
			VALUES (fieldwork.get_ow_uid(",rv$smp_id(),", '", input$well_name, "', ", rv$site_name_lookup_uid(), "), ", rv$inventory_sensors_uid_null(),
                              ", ", rv$purpose_null(), ", ", rv$interval_min(), ", ", rv$term_null(),
                              ", ", rv$research_lookup_uid(), ", ", rv$notes(), ", ", rv$priority_lookup_uid(), ")"))
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
                              field_test_priority_lookup_uid = ", rv$priority_lookup_uid(), " WHERE 
                              future_deployment_uid = '", rv$update_future_deployment_uid(), "'" 
                       ))
    }
    rv$future_table_db <- reactive(odbc::dbGetQuery(poolConn, future_table_query()))
    rv$refresh_collect <- rv$refresh_collect + 1
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
  
  observe({toggleState(id = "future_deploy", condition = (nchar(input$smp_id) > 0 | nchar(input$site_name) > 0) &
                         nchar(input$well_name) > 0)})
  
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
  
  
  return(
    list(
      refresh_collect = reactive(rv$refresh_collect),
      refresh_sensor = reactive(rv$refresh_sensor),
      dud = "dud"
    )
  )
}