#Deploy Sensor tab


deployUI <- function(id, label = "deploy", smp_id, html_req){
  ns <- NS(id)
  tabPanel("Deploy Sensor", value = "deploy_tab",
           titlePanel("Deploy Sensor"), 
           fluidRow(
             column(width = 4, 
                    sidebarPanel(width = 12, 
                                 fluidRow(
                                   column(6,selectInput(ns("smp_id"), html_req("SMP ID"), choices = c("", smp_id), selected = NULL)), 
                                   column(6,selectInput(ns("well_name"), html_req("Well Name"), choices = ""))), 
                                 fluidRow(
                                   column(6,selectInput(ns("sensor_id"), html_req("Sensor ID"), choices = c("", ""), selected = NULL)),
                                   column(6, selectInput(ns("sensor_purpose"), html_req("Sensor Purpose"), choices = c("", "BARO", "LEVEL"), selected = NULL))),
                                 fluidRow(
                                   column(6, selectInput(ns("term"), html_req("Term"), choices = c("", "Short", "Long", "SRT", "NA"), selected = NULL)),
                                   column(6, selectInput(ns("research"), "Research", choices = c("", "USEPA STAR"), selected = NULL))),
                                 selectInput(ns("interval"), html_req("Measurement Interval (min)"), choices = c("", 5, 15), selected = NULL),
                                 fluidRow(
                                   column(6,dateInput(ns("deploy_date"), html_req("Deployment Date"), value = as.Date(NA))),
                                   column(6,dateInput(ns("collect_date"), "Collection Date", value = as.Date(NA)))), 
                                 #ask about download error and redeploying sensor once you add a collection date
                                 conditionalPanel(width = 12, 
                                                  condition = "input.collect_date",
                                                  ns = ns,
                                                  selectInput(ns("download_error"), html_req("Did you encounter a download error?"), 
                                                              choices = c("", "No", "Yes"), selected = NULL),
                                                  checkboxInput(ns("redeploy"), "Redeploy Sensor?"),
                                                  h6("Redeploy sensor in the same well on this collection date")
                                 ),
                                 textAreaInput(ns("notes"), "Notes"),
                                 actionButton(ns("deploy_sensor"), "Deploy Sensor"), 
                                 actionButton(ns("clear_deploy_fields"), "Clear All Fields")
                    )
             ),
             column(width = 8,
                    #show tables when an smp id is selected
                    conditionalPanel(condition = "input.smp_id",
                                     ns = ns, 
                                      h4(textOutput(ns("active_text"))),
                                      DTOutput(ns("current_deployment")), 
                                      h4(textOutput(ns("previous_text"))),
                                      DTOutput(ns("prev_deployment"))
                    )
             )
           )
  )
}

deploy <- function(input, output, session, parent_session, ow, collect, sensor, poolConn, deployment_lookup){
  
  #define ns to use in modals
  ns <- session$ns
  #update sensor id choices based on the hobo list
  observe(updateSelectInput(session, inputId = "sensor_id", choices = c("", sensor$sensor_serial()), selected = NULL))
  
  #start reactiveValues for this section/tab
  rv <- reactiveValues()
  ## well panel
  #query ow_suffixes based on smp_id
  rv$ow_suffixes <- reactive(odbc::dbGetQuery(poolConn, paste0(
    "select ow_suffix from fieldwork.ow where smp_id = '", input$smp_id, "'")) %>% dplyr::pull())
  
  observe(updateSelectInput(session, "well_name", choices = c("", rv$ow_suffixes())))
  observe(updateDateInput(session, "collect_date", min = input$deploy_date))
  
  #upon clicking "deploy at this smp" in add_ow
  observeEvent(ow$refresh_deploy(), {
    #using shinyjs::reset is too slow and will reset after the selection is updated to the desired SMPID
    updateSelectInput(session, "smp_id", selected = NULL)
    updateSelectInput(session, "smp_id", selected = ow$smp_id())
  })
  
  #upon clicking "deploy this sensor" in add_sensor
  observeEvent(sensor$refresh_serial_no(), {
    updateSelectInput(session, "sensor_id", selected = sensor$serial_no())
  })
  
  #upon clicking a row in collection_calendar
  observeEvent(collect$deploy_refresh(), {
    updateSelectInput(session, "smp_id", selected = "")
    updateSelectInput(session, "smp_id", selected = collect$smp_id())
  })

  observeEvent(rv$active_table_db(), {
    if(length(collect$rows_selected()) > 0){
      rv$active_row <- reactive(which(rv$active_table_db()$deployment_uid == collect$row(), arr.ind = TRUE))
      dataTableProxy('current_deployment') %>% selectRows(rv$active_row())
    }
  })
  
  #Get the Project name, combine it with SMP ID, and create a reactive header
  rv$smp_and_name_step <- reactive(odbc::dbGetQuery(poolConn, paste0("select smp_id, project_name from project_names where smp_id = '", input$smp_id, "'")))
  
  rv$smp_and_name <- reactive(paste(rv$smp_and_name_step()$smp_id[1], rv$smp_and_name_step()$project_name[1]))
  
  output$active_text <- renderText(
    paste("Active Deployments at", rv$smp_and_name())
  )
  
  output$previous_text <- renderText(
    paste("Active Deployments at", rv$smp_and_name())
  )
  
  
  ## sensor panel
  
  rv$purpose <- reactive(deployment_lookup %>% dplyr::filter(type == input$sensor_purpose) %>% 
                           select(sensor_purpose_lookup_uid) %>% pull())
  #rv$purpose <- reactive(if(input$sensor_purpose == "LEVEL") 1 else if(input$sensor_purpose == "BARO") 2 else NA)
  rv$inventory_sensors_uid <- reactive(odbc::dbGetQuery(poolConn, paste0(
    "SELECT inventory_sensors_uid FROM fieldwork.inventory_sensors WHERE sensor_serial = '", input$sensor_id, "'"
  )))
  
  #rv$term <- reactive(long_term_lookup %>% dplyr::filter(type == input$term) %>% 
  #                         select(long_term_lookup_uid) %>% pull())
  
  rv$term <- reactive(odbc::dbGetQuery(poolConn, paste0("Select long_term_lookup_uid FROM fieldwork.long_term_lookup WHERE type = '", input$term, "'")) %>% pull)
  
  rv$research <- reactive(odbc::dbGetQuery(poolConn, paste0("select research_lookup_uid FROM fieldwork.research_lookup WHERE type = '", input$research, "'")) %>%  pull)
  
  rv$notes_step1 <- reactive(gsub('\'', '\'\'', input$notes))
  rv$notes <- reactive(if(nchar(rv$notes_step1()) == 0) "NULL" else paste0("'", rv$notes_step1(), "'"))
  
  rv$download_error <- reactive(if(nchar(input$download_error) == 0 | length(input$collect_date) == 0){
    "NULL" 
    }else{paste0("'", input$download_error, "'")
    })
  
  #set fields to "NULL" if they are empty
  rv$collect_date <- reactive(if(length(input$collect_date) == 0) "NULL" else paste0("'", input$collect_date, "'"))
  rv$research_lookup_uid <- reactive(if(length(rv$research()) == 0) "NULL" else paste0("'", rv$research(), "'"))
  
  ## show tables
  #query for active deployments
  active_table_query <- reactive(paste0(
    "SELECT * FROM fieldwork.active_deployments
        WHERE smp_id = '", input$smp_id, "' ORDER BY deployment_dtime_est"))
  
  #create table as a reactive value based on query
  rv$active_table_db <- reactive(odbc::dbGetQuery(poolConn, active_table_query())%>% 
                                   mutate_at(vars(one_of("download_error")), 
                                             funs(case_when(. == 1 ~ "Yes", 
                                                            . == 0 ~ "No"))))
  #select columns to show in app, and rename
  rv$active_table <- reactive(rv$active_table_db() %>% 
                                mutate_at(c("deployment_dtime_est", "date_80percent", "date_100percent"), as.character) %>% 
                                dplyr::select(deployment_dtime_est, smp_id, ow_suffix, type, term, research, interval_min, date_80percent, date_100percent) %>% 
                                dplyr::rename("Deploy Date" = "deployment_dtime_est", "SMP ID" = "smp_id", 
                                              "OW" = "ow_suffix", "Purpose" = "type", "Term" = "term",
                                              "Research" = "research", "Interval (min)" = "interval_min", 
                                              "80% Full Date" = "date_80percent", "100% Full Date" = "date_100percent"))
  
  #when a row in active deployments table is clicked
  observeEvent(input$current_deployment_rows_selected, {
    #deselect from other table
    dataTableProxy('prev_deployment') %>% selectRows(NULL)
    
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
  old_table_query <- reactive(paste0(
    "SELECT * FROM fieldwork.previous_deployments
        WHERE smp_id = '", input$smp_id, "' ORDER BY deployment_dtime_est desc"))
  
  #create table as a reactive value based on query
  rv$old_table_db <- reactive(odbc::dbGetQuery(poolConn, old_table_query())%>% 
                                mutate_at(vars(one_of("download_error")), 
                                          funs(case_when(. == 1 ~ "Yes", 
                                                         . == 0 ~ "No"))))
  
  rv$old_table <- reactive(rv$old_table_db() %>% 
                             mutate_at(c("deployment_dtime_est", "collection_dtime_est"), as.character) %>% 
                             dplyr::select(deployment_dtime_est, collection_dtime_est, smp_id, ow_suffix, type, term, research, interval_min) %>% 
                             dplyr::rename("Deploy Date" = "deployment_dtime_est", "SMP ID" = "smp_id", "OW" = "ow_suffix", 
                                           "Purpose" = "type", "Term" = "term", "Research" = "research",
                                           "Interval (min)" = "interval_min", "Collection Date" = "collection_dtime_est"))
  
  #render datatable
  output$prev_deployment <- renderDT(
    rv$old_table(),
    selection = "single",
    style = 'bootstrap', 
    class = 'table-responsive, table-condensed', 
    options = list(dom = 'tp')
  )
  
  #shorten name of selected rows from active and prev deployments tables
  rv$active <- reactive(input$current_deployment_rows_selected) 
  rv$prev <- reactive(input$prev_deployment_rows_selected)
  
  #define inputs, based on whether previous or active deployments tables are selected
  #this was in two separate observeEvent calls, but changed to try to address the issue where collection date is sometimes blank when it shouldn't be
  #that was not resolved - I believe the issue is related to having two dateInputs update at the same time
  rv$well_name <- reactive(if(length(rv$active()) > 0) rv$active_table_db()$ow_suffix[rv$active()] else if(length(rv$prev()) > 0) rv$old_table_db()$ow_suffix[rv$prev()])
  rv$sensor_id <- reactive(if(length(rv$active()) > 0) rv$active_table_db()$sensor_serial[rv$active()] else if(length(rv$prev()) > 0) rv$old_table_db()$sensor_serial[rv$prev()])
  rv$sensor_purpose <- reactive(if(length(rv$active()) > 0) rv$active_table()$`Purpose`[rv$active()] else if(length(rv$prev()) > 0) rv$old_table()$`Purpose`[rv$prev()])
  rv$term_step <- reactive(if(length(rv$active()) > 0) rv$active_table()$`Term`[rv$active()] else if(length(rv$prev()) > 0) rv$old_table()$`Term`[rv$prev()])
  rv$research_step <- reactive(if(length(rv$active()) > 0) rv$active_table()$`Research`[rv$active()] else if(length(rv$prev()) > 0) rv$old_table()$`Research`[rv$prev()])
  rv$mea_int <- reactive(if(length(rv$active()) > 0) rv$active_table_db()$interval_min[rv$active()] else if(length(rv$prev()) > 0) rv$old_table_db()$interval_min[rv$prev()])
  rv$deploy_date <- reactive(if(length(rv$active()) > 0) rv$active_table_db()$deployment_dtime_est[rv$active()] else if(length(rv$prev()) > 0) rv$old_table_db()$deployment_dtime_est[rv$prev()])
  rv$collect <- reactive(if(length(rv$active()) > 0) NA else if(length(rv$prev()) > 0) rv$old_table_db()$collection_dtime_est[rv$prev()])
  rv$notes_step <- reactive(if(length(rv$active()) > 0) rv$active_table_db()$notes[rv$active()] else if(length(rv$prev()) > 0) rv$old_table_db()$notes[rv$prev()]) 
  rv$download_error_step <- reactive(if(length(rv$active()) > 0) rv$active_table_db()$download_error[rv$active()] else if(length(rv$prev()) > 0) rv$old_table_db()$download_error[rv$prev()]) 
  
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
  })
  
  #when a row in the previous deployments table is clicked
  observeEvent(input$prev_deployment_rows_selected, {
    #deselect from other table
    dataTableProxy('current_deployment') %>% selectRows(NULL)
  })
  
  #control for redeploy checkbox. had to add, because after checking it, then removing collect_date, it goes away, but is still TRUE
  rv$redeploy <- reactive(if(length(input$collect_date > 0) & input$redeploy == TRUE) TRUE else FALSE)
  
  # observeEvent(input$collect_date, {
  #   reset('download_error')
  # })
  
  #relabel deploy button if editing
  rv$label <- reactive(if(length(input$prev_deployment_rows_selected) == 0 & length(input$current_deployment_rows_selected) == 0) "Add New Deployment" else "Edit Selected Deployment")
  
  observe(updateActionButton(session, "deploy_sensor", label = rv$label()))
  
  rv$update_deployment_uid <- reactive(if(length(input$current_deployment_rows_selected) > 0){
    rv$active_table_db()$deployment_uid[input$current_deployment_rows_selected]
  }else if(length(input$prev_deployment_rows_selected) > 0){
    rv$old_table_db()$deployment_uid[input$prev_deployment_rows_selected]
  }else{
    0
  }
  )
  
  #define 
  rv$add_new <- reactive((length(input$prev_deployment_rows_selected) == 0 & length(input$current_deployment_rows_selected) == 0))
  rv$modal_title <- reactive(if(rv$add_new()) "Add New Deployment" else "Edit Deployment")
  rv$modal_text <- reactive(if(rv$add_new() & rv$redeploy()){
    "Add New Deployment and Redeploy Sensor?"
  }else if(rv$add_new() & rv$redeploy() == FALSE){
    "Add New Deployment?"
  }else if(rv$add_new() == FALSE & rv$redeploy() == TRUE){
    "Edit Deployment and Redeploy Sensor?"
  }else if(rv$add_new() == FALSE & rv$redeploy() == FALSE){
    "Edit Deployment?"
  })
  
  #write to database on click
  #go through dialog box to confirm action
  observeEvent(input$deploy_sensor, {
    showModal(modalDialog(title = rv$modal_title(), 
                          rv$modal_text(), 
                          modalButton("No"), 
                          actionButton(ns("confirm_deploy"), "Yes")))
  })
  
  rv$refresh_collect <- 0 
  
  observeEvent(input$confirm_deploy, {
    if(rv$add_new()){
      #write new deployment
      odbc::dbGetQuery(poolConn,
                       paste0("INSERT INTO fieldwork.deployment (deployment_dtime_est, ow_uid,
     inventory_sensors_uid, sensor_purpose, long_term_lookup_uid, research_lookup_uid, interval_min, collection_dtime_est, notes, download_error)
        VALUES ('", input$deploy_date, "', fieldwork.get_ow_uid_fieldwork('",input$smp_id,"', '", input$well_name, "'), '",
                              rv$inventory_sensors_uid(), "','", rv$purpose(), "','", rv$term(), "',", rv$research_lookup_uid(), ",'",input$interval, "',", rv$collect_date(),",", rv$notes(),",", rv$download_error(), ")"))
    }else{
      #update existing deployment
      odbc::dbGetQuery(poolConn, 
                       paste0("UPDATE fieldwork.deployment SET deployment_dtime_est = '", input$deploy_date, "', 
                       ow_uid = fieldwork.get_ow_uid_fieldwork('",input$smp_id,"', '", input$well_name, "'), 
                              inventory_sensors_uid = '",  rv$inventory_sensors_uid(), "', 
                              sensor_purpose = '", rv$purpose(), "',
                              long_term_lookup_uid = '", rv$term(), "',
                              research_lookup_uid = ", rv$research_lookup_uid(), ",
                              interval_min = '", input$interval, "',
                              collection_dtime_est = ", rv$collect_date(), ",
                              notes = ", rv$notes(), ", 
                              download_error = ", rv$download_error(), " WHERE 
                              deployment_uid = '", rv$update_deployment_uid(), "'"
                              ))
    }
    #write redeployment
    if(rv$redeploy() == TRUE){
      dbGetQuery(poolConn, paste0("INSERT INTO fieldwork.deployment (deployment_dtime_est, ow_uid,
     inventory_sensors_uid, sensor_purpose, long_term_lookup_uid, research_lookup_uid, interval_min, collection_dtime_est)
        VALUES (", rv$collect_date(), ", fieldwork.get_ow_uid_fieldwork('",input$smp_id,"', '", input$well_name, "'), '",
                                  rv$inventory_sensors_uid(), "','", rv$purpose(), "','", rv$term(), "',", rv$research_lookup_uid(), ",'",input$interval, "', NULL)"))
    }
    #query active table
    rv$active_table_db  <- reactive(odbc::dbGetQuery(poolConn, active_table_query())) 
    #query prev table
    rv$old_table_db <- reactive(odbc::dbGetQuery(poolConn, old_table_query())) 
    
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
    removeModal()
  })
  
  
  #enable/disable deploy sensor button based on whether all inputs (except collect date) are not empty
  #logical to add to the toggleState below
  rv$req_dl_error <- reactive(if(length(input$collect_date) == 0){
    TRUE
  }else if(nchar(input$download_error) > 0){
    TRUE
  }else{
    FALSE
  })
  #also check to make sure the sensor is not already deployed, or the collection has a sensor date, or a row selected
  observe({toggleState(id = "deploy_sensor", condition = nchar(input$smp_id) > 0 &
                         nchar(input$well_name) > 0 & nchar(input$sensor_id) > 0 & nchar(input$sensor_purpose) > 0 &
                         nchar(input$interval) > 0 & length(input$deploy_date) > 0 & nchar(input$term) > 0 &
                         rv$req_dl_error() &
                         (!(input$sensor_id %in% collect$sensor_serial()) |
                            (length(input$collect_date) > 0 & rv$redeploy() == TRUE & !(input$sensor_id %in% collect$sensor_serial())) |
                            (length(input$collect_date) >0 & rv$redeploy() == FALSE) |
                            length(input$current_deployment_rows_selected) > 0 |
                            length(input$prev_deployment_rows_selected) > 0))})
  
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
    removeModal()
  })
  
  
  return(
    list(
    refresh_collect = reactive(rv$refresh_collect),
    dud = "dud"
      )
  )
}