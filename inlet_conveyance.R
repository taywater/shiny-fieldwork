#Inlet Conveyance module
#3 tabs: View future ICT, past ICT, add/edit ICTs by site

inlet_conveyanceUI <- function(id, label = "inlet_conveyance", sys_id, site_names, html_req, work_number, priority, con_phase, future_req){
  ns <- NS(id)
  navbarMenu("Inlet Conveyance", 
             tabPanel("Add/Edit Inlet Conveyance Tests", value = "ict_tab", 
                      titlePanel("Add Inlet Conveyance Test"), 
                      sidebarPanel(
                        style = "overflow-y:scroll; overflow-x:hidden; max-height: 650px",
                                  h5("Prioritize System ID, then Work Number, then Site Name. Only one is required."),
                                  fluidRow(
                                    column(4, selectInput(ns("system_id"), future_req(html_req("System ID")), 
                                                          choices = c("", sys_id), selected = NULL)), 
                                    column(4, selectInput(ns("work_number"), future_req(html_req("Work Number")), 
                                                          choices = c("", work_number), selected = NULL)),
                                    column(4, selectInput(ns("site_name"), future_req(html_req("Site Name")), 
                                                          choices = c("", site_names), selected = NULL))),
                                 fluidRow(
                                   column(6, selectInput(ns("comp_id"), future_req(html_req("Component ID")), choice = "", selected = NULL)), 
                                   column(6, textInput(ns("comp_id_custom"), future_req(html_req("Custom Component ID"))))), 
                                  disabled(textInput(ns("facility_id"), "Facility ID")), 
                                  fluidRow(
                                    column(6, dateInput(ns("date"), html_req("Test Date"), value = as.Date(NA))), 
                                    column(6, selectInput(ns("con_phase"), "Construction Phase", 
                                                          choices = c("", con_phase$phase), selected = NULL))),
                                 fluidRow(
                                   column(6, numericInput(ns("calc_flow_rate"), "Calculated Flow Rate (CFM)", value = NA)),
                                   column(6, numericInput(ns("eq_flow_rate"), "Equilibrated Flow Rate (CFM)", value = NA))),
                                 fluidRow(
                                   column(6, numericInput(ns("test_volume_cf"), "Test Volume (CF)", value = NA)),
                                   column(6, numericInput(ns("max_water_depth_ft"), "Max. Water Depth (ft)", value = NA))
                                 ),
                                 fluidRow(
                                   column(6, selectInput(ns("surcharge"), "Surcharge", 
                                                         choices = c("","Yes" = "1", "No" = "0"), selected = NULL)), 
                                   column(6, numericInput(ns("time_to_surcharge"), "Time to Surcharge (min)", value = NA))
                                 ), 
                                 fluidRow(
                                   column(6, selectInput(ns("photos"), "Photos Uploaded", 
                                                         choices = c("","Yes" = "1", "No" = "0"), selected = NULL)), 
                                   column(6, dateInput(ns("summary_sent"), "Summary Date", 
                                                         value = NA))
                                 ),
                                  conditionalPanel(condition = "input.date === null", 
                                                   ns = ns, 
                                                   selectInput(ns("priority"), "Future Test Priority", 
                                                               choices = c("", priority$field_test_priority), selected = NULL)),
                                  textAreaInput(ns("notes"), "Notes", height = '90px'), 
                                  conditionalPanel(condition = "input.date === null", 
                                                   ns = ns, 
                                                   actionButton(ns("future_test"), "Add Future Inlet Conveyance Test")),
                                  actionButton(ns("add_test"), "Add Inlet Conveyance Test"), 
                                  actionButton(ns("clear"), "Clear All Fields"), 
                        fluidRow(
                          HTML(paste(html_req(""), " indicates required field for complete tests. ", future_req(""), " indicates required field for future tests.")))
                      ), 
                      mainPanel(
                        conditionalPanel(condition = "input.system_id || input.work_number || input.site_name", 
                                         ns  = ns, 
                                         h4(textOutput(ns("future_header"))), #("Capture Efficiency Tests at this SMP"), 
                                         DTOutput(ns("future_ict_table")),
                                         h4(textOutput(ns("header"))),
                                         DTOutput(ns("ict_table"))),
             ),
                      ), 
             tabPanel("View Inlet Conveyance Tests", value = ns("view_ict"), 
                      titlePanel("All Inlet Conveyance Tests"), 
                      reactableOutput(ns("all_ict_table"))
             ), 
             tabPanel("View Future Inlet Conveyance Tests", value = ns("view_future_ict"), 
                      titlePanel("All Future Inlet Conveyance Tests"), 
                      reactableOutput(ns("all_future_ict_table")))
             )
}

inlet_conveyance <- function(input, output, session, parent_session, poolConn, con_phase){
  
  #define ns to use in modals 
  ns <- session$ns
  
  rv <- reactiveValues()
  
  #toggle work number/system id/site names so when one is selected, the others are disabled
  observe(toggleState("work_number", condition = nchar(input$system_id) == 0 & nchar(input$site_name) == 0))
  observe(toggleState("system_id", condition = nchar(input$work_number) == 0 & nchar(input$site_name) == 0))
  observe(toggleState("site_name", condition = nchar(input$system_id) == 0 & nchar(input$work_number) == 0 ))
  
  
  #Get the Project name, combine it with System ID, and create a reactive header
  rv$sys_and_name_step <- reactive(odbc::dbGetQuery(poolConn, paste0("select system_id, project_name from project_names where system_id = '", input$system_id, "'")))
  
  rv$sys_and_name <- reactive(paste(rv$sys_and_name_step()$system_id[1], rv$sys_and_name_step()$project_name[1]))
  
  #Get project name from work number
  rv$worknumber_and_name_step <- reactive(odbc::dbGetQuery(poolConn, paste0("select worknumber, project_name from project_names where worknumber = '", input$work_number, "'")))
  
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
  
  output$header <- renderText(
    paste("Inlet Conveyance Tests at", rv$header_input())
  )
  
  output$future_header <- renderText(
    paste("Future Inlet Conveyance Tests at", rv$header_input())
  )
  
  
  #toggle "add test" button so it is only active when certain fields are complete
  observe(toggleState("add_test", condition = (nchar(input$system_id) > 0 | nchar(input$work_number) > 0 | nchar(input$site_name) > 0) &
                        length(input$date) > 0 &
                        (nchar(input$comp_id) > 0 | nchar(input$comp_id_custom) > 0)
                        ))
  
  observe(toggleState("future_test", condition = (nchar(input$system_id) | nchar(input$work_number) > 0 | nchar(input$site_name) > 0) &
                        length(input$date) == 0&
                        (nchar(input$comp_id) > 0 | nchar(input$comp_id_custom) > 0)
                      ))
  
  #toggle 'results fields' so they can only be filled when a test date is entered
  observe(toggleState("eq_flow_rate", condition = length(input$date) > 0))
  observe(toggleState("test_volume_cf", condition = length(input$date) > 0))
  observe(toggleState("max_water_depth_ft", condition = length(input$date) > 0))
  observe(toggleState("surcharge", condition = length(input$date) > 0))
  observe(toggleState("time_to_surcharge", condition = input$surcharge == 1))#length(input$date) > 0))# & input$surchage == 1))
  observe(toggleState("photos", condition = length(input$date) > 0))
  observe(toggleState("summary_sent", condition = length(input$date) > 0))
  
  #if one of the component ids is filled out, disable the other
  #have only custom component ID available for non SMP
  observe(toggleState(id = "comp_id_custom", condition = input$comp_id == ""))
  observe(toggleState(id = "comp_id", condition = input$comp_id_custom == "" & nchar(input$work_number) == 0 & nchar(input$site_name) == 0))
  
  #component IDs
  #adjust query to accurately target NULL values once back on main server
  rv$component_and_asset_query <- reactive(paste0("SELECT component_id, asset_type FROM smpid_facilityid_componentid_inlets WHERE system_id = '", input$system_id, "' AND component_id != 'NULL'"))
  rv$component_and_asset <- reactive(odbc::dbGetQuery(poolConn, rv$component_and_asset_query()))
  
  rv$asset_comp <- reactive(rv$component_and_asset() %>% 
                              mutate("asset_comp_code" = paste(component_id, asset_type, sep = " | ")))
  
  rv$asset_combo <- reactive(rv$asset_comp()$asset_comp_code)
  
  observe(updateSelectInput(session, "comp_id", choices = c("", rv$asset_combo())))
  
  #get the row/case that has the selected asset_comp_code
  rv$select_combo_row <- reactive(rv$asset_comp() %>% 
                                    dplyr::filter(asset_comp_code == input$comp_id))
  
  #get component id from the chosen asset_comp_code, then use to get facility id
  rv$select_component_id <- reactive(rv$select_combo_row() %>% 
                                       dplyr::select(component_id) %>%
                                       dplyr::pull() %>% 
                                       dplyr::first())
  
  #get facility ID for systems. Either use SMP footprint (for an unknown component) or the facility ID of the existing component
  rv$facility_id_system <- reactive(if(input$comp_id != ""){
    odbc::dbGetQuery(poolConn, paste0(
      "SELECT facility_id from smpid_facilityid_componentid_inlets WHERE component_id = '", rv$select_component_id(), "'"))[1,1]
  }else if(input$system_id != ""){
    odbc::dbGetQuery(poolConn, paste0("SELECT facility_id from smpid_facilityid_componentid_inlets WHERE component_id is NULL and system_id = '", input$system_id, "' LIMIT 1"))
  }else{
    ""
  }
  )
  
  observe(updateSelectInput(session, "facility_id", selected = rv$facility_id_system()))
  
  #make facility id nullable
  rv$facility_id <- reactive(if(nchar(input$facility_id) > 0) paste0("'", input$facility_id, "'") else "NULL")
  
  #lookup priority uid
  rv$priority_lookup_uid_query <- reactive(paste0("select field_test_priority_lookup_uid from fieldwork.field_test_priority_lookup where field_test_priority = '", input$priority, "'"))
  rv$priority_lookup_uid_step <- reactive(dbGetQuery(poolConn, rv$priority_lookup_uid_query()))
  rv$priority_lookup_uid <- reactive(if(nchar(input$priority) == 0) "NULL" else paste0("'", rv$priority_lookup_uid_step(), "'"))
  
  #lookup site name uid
  rv$site_name_lookup_uid_step <- reactive(odbc::dbGetQuery(poolConn, paste0("select site_name_lookup_uid from fieldwork.site_name_lookup where site_name = '", input$site_name, "'")) %>% pull())
  
  rv$site_name_lookup_uid <- reactive(if (nchar(input$site_name) > 0) paste0("'", rv$site_name_lookup_uid_step(), "'") else "NULL")
  
  #work number nullable
  rv$work_number <- reactive(if(nchar(input$work_number) > 0) paste0("'", input$work_number, "'") else "NULL")
  
  #make other fields nullable (and add single quotes)
  rv$system_id <- reactive(if(nchar(input$system_id) > 0) paste0("'", input$system_id, "'") else "NULL")
  
  #pick a component ID
  #protect against SQL injection
  rv$comp_id_custom_step <- reactive(gsub('\'', '\'\'', input$comp_id_custom))
  rv$comp_id_custom <- reactive(if(nchar(rv$comp_id_custom_step()) == 0) "NULL" else paste0("'", rv$comp_id_custom_step(), "'"))
  
  rv$component_id <- reactive(if(nchar(input$comp_id) > 0){
    paste0("'",rv$select_component_id(), "'")
  }else{
    rv$comp_id_custom()
  })
  
  
  rv$phase <- reactive(con_phase %>% dplyr::filter(phase == input$con_phase) %>% 
                         select(con_phase_lookup_uid) %>% pull())
  
  rv$phase_null <- reactive(if(nchar(input$con_phase) == 0) "NULL" else paste0("'", rv$phase(), "'"))  
  
  rv$test_date <- reactive(if(is.na(input$date)) "NULL" else paste0("'", input$date, "'"))
  
  rv$calc_flow_rate <- reactive(if(is.na(input$calc_flow_rate)) "NULL" else paste0("'", input$calc_flow_rate, "'"))  
  
  rv$eq_flow_rate <- reactive(if(is.na(input$eq_flow_rate)) "NULL" else paste0("'", input$eq_flow_rate, "'"))  
  
  rv$test_volume_cf <- reactive(if(is.na(input$test_volume_cf)) "NULL" else paste0("'", input$test_volume_cf, "'"))  
  
  rv$max_water_depth_ft <- reactive(if(is.na(input$max_water_depth_ft)) "NULL" else paste0("'", input$max_water_depth_ft, "'"))  
  
  rv$surcharge <- reactive(if(nchar(input$surcharge) == 0) "NULL" else paste0("'", input$surcharge, "'"))  
  
  rv$time_to_surcharge <- reactive(if(is.na(input$time_to_surcharge)) "NULL" else paste0("'", input$time_to_surcharge, "'"))  
  
  rv$photos <- reactive(if(nchar(input$photos) == 0) "NULL" else paste0("'", input$photos, "'"))  
  
  rv$summary_sent <- reactive(if(length(input$summary_sent) == 0) "NULL" else paste0("'", input$summary_sent, "'"))  
  
  #notes
  rv$notes_step <- reactive(gsub('\'', '\'\'', input$notes))
  rv$notes <- reactive(if(nchar(rv$notes_step()) == 0) "NULL" else paste0("'", rv$notes_step(), "'"))
  
  #get the table of ICTs
  rv$ict_table_query <- reactive(paste0("SELECT * FROM fieldwork.inlet_conveyance_full 
                                        WHERE system_id = '", input$system_id, "'
                                        OR work_number = ", rv$work_number(), " 
                                        OR site_name_lookup_uid = ", rv$site_name_lookup_uid()))
  rv$ict_table_db <- reactive(dbGetQuery(poolConn, rv$ict_table_query()))
  rv$ict_table <- reactive(rv$ict_table_db() %>% mutate_at("test_date", as.character) %>% 
                             mutate_at(vars(one_of("surcharge", "photos_uploaded", "summary_report_sent")), 
                                       funs(case_when(. == 1 ~ "Yes", 
                                                      . == 0 ~ "No"))) %>% 
                             dplyr::select("component_id", "test_date", "phase",
                                           "calculated_flow_rate_cfm", "equilibrated_flow_rate_cfm", "test_volume_cf",
                                           "max_water_depth_ft",
                                           "surcharge", "time_to_surcharge_min"))
  
  #show table of ICTs
  output$ict_table <- renderDT(
    datatable(rv$ict_table(), 
              colnames = c('Component ID', 'Test Date', 'Phase', 'Calculated Flow Rate (CFM)', 
                           'Equilibrated Flow Rate (CFM)', 'Test Volume (CF)', 'Max Water Depth (ft)',
                           'Surcharge', 'Time to Surcharge (min)'),
              selection = 'single', 
              style = 'bootstrap', 
              class = 'table-responsive, table-hover', 
              escape = FALSE 
    ))
  
  #get table of future ICTs
  #query future CETs
  future_ict_table_query <- reactive(paste0("SELECT * FROM fieldwork.future_inlet_conveyance_full 
                                            WHERE system_id = '", input$system_id, "' 
                                            OR work_number = ", rv$work_number(), " 
                                            OR site_name_lookup_uid = ", rv$site_name_lookup_uid(), " 
                                            order by field_test_priority_lookup_uid"))
  rv$future_ict_table_db <- reactive(odbc::dbGetQuery(poolConn, future_ict_table_query()))
  
  rv$future_ict_table <- reactive(rv$future_ict_table_db() %>% 
                                    dplyr::select("component_id", "phase", "calculated_flow_rate_cfm", "field_test_priority", "notes"))
  
  output$future_ict_table <- renderDT(
    datatable(
    rv$future_ict_table(), 
    selection = 'single', 
    style = 'bootstrap', 
    class = 'table-responsive, table-hover', 
    colnames = c('System ID', 'Component ID',  'Phase', 'Calculated Flow Rate (CFM)', 'Priority', 'Notes')
    )
  )
  
  
  
  #when you click on a row, populate fields with data from that row
  observeEvent(input$ict_table_rows_selected, {
    
    dataTableProxy('future_ict_table') %>% selectRows(NULL)
    #reset("comp_id")
    #reset("comp_id_custom")
    
    #get facility id from table
    rv$fac_step <- (rv$ict_table_db()$facility_id[input$ict_table_rows_selected])
    
    rv$fac <- if(is.na(rv$fac_step)) "NULL" else paste0("'", rv$fac_step, "'")
      
    #get component id
    comp_id_query <- paste0("select distinct component_id from smpid_facilityid_componentid_inlets where facility_id = ", rv$fac, "
        AND component_id IS NOT NULL")
    comp_id_step <- odbc::dbGetQuery(poolConn, comp_id_query) %>% pull()
    #determine whether component id exists and is useful
    comp_id_click <- if(length(comp_id_step) > 0) comp_id_step else "NA"
    
    #get asset type - base on component id (if exists)
    
    if(nchar(comp_id_click) > 2){
      asset_type_click <- dplyr::filter(rv$asset_comp(), component_id == comp_id_click) %>% select(asset_type) %>% pull()
      #combine asset type, ow, and component id
      rv$asset_comp_code_click = paste(comp_id_click, asset_type_click,  sep = " | ")
      updateSelectInput(session, "comp_id_custom", selected = "")
      updateSelectInput(session, "comp_id", selected = rv$asset_comp_code_click)
    }else{
      updateSelectInput(session, "comp_id", selected = "")
      updateSelectInput(session, "comp_id_custom", selected = rv$ict_table_db()$component_id[input$ict_table_rows_selected])
    }
    
    updateDateInput(session, "date", value = rv$ict_table_db()$test_date[input$ict_table_rows_selected])
    updateSelectInput(session, "con_phase", selected = rv$ict_table_db()$phase[input$ict_table_rows_selected])
     updateNumericInput(session, "calc_flow_rate", value = rv$ict_table_db()$calculated_flow_rate_cfm[input$ict_table_rows_selected])
     updateNumericInput(session, "eq_flow_rate", value = rv$ict_table_db()$equilibrated_flow_rate_cfm[input$ict_table_rows_selected])
     updateNumericInput(session, "test_volume_cf", value = rv$ict_table_db()$test_volume_cf[input$ict_table_rows_selected])
     updateNumericInput(session, "max_water_depth_ft", value = rv$ict_table_db()$max_water_depth_ft[input$ict_table_rows_selected])
     updateNumericInput(session, "time_to_surcharge", value = rv$ict_table_db()$time_to_surcharge_min[input$ict_table_rows_selected])
    updateSelectInput(session, "surcharge", selected = rv$ict_table_db()$surcharge[input$ict_table_rows_selected])
    updateSelectInput(session, "photos", selected = rv$ict_table_db()$photos_uploaded[input$ict_table_rows_selected])
    updateDateInput(session, "summary_sent", value = rv$ict_table_db()$summary_report_sent[input$ict_table_rows_selected])
    updateTextAreaInput(session, "notes", value = rv$ict_table_db()$notes[input$ict_table_rows_selected])
    reset("priority")
  })
  
  observeEvent(input$future_ict_table_rows_selected, {
    
    dataTableProxy('ict_table') %>% selectRows(NULL)
    #reset("comp_id")
    #reset("comp_id_custom")
    
    #get facility id from table
    rv$fac_step <- (rv$future_ict_table_db()$facility_id[input$future_ict_table_rows_selected])
    rv$fac <- if(is.na(rv$fac_step)) "NULL" else paste0("'", rv$fac_step, "'")
    #get component id
    comp_id_query <- paste0("select distinct component_id from smpid_facilityid_componentid_inlets where facility_id = ", rv$fac, "
        AND component_id IS NOT NULL")
    comp_id_step <- odbc::dbGetQuery(poolConn, comp_id_query) %>% pull()
    #determine whether component id exists and is useful
    comp_id_click <- if(length(comp_id_step) > 0) comp_id_step else "NA"
    
    #get asset type - base on component id (if exists)
    
    if(nchar(comp_id_click) > 2){
      asset_type_click <- dplyr::filter(rv$asset_comp(), component_id == comp_id_click) %>% select(asset_type) %>% pull()
      #combine asset type, ow, and component id
      rv$asset_comp_code_click = paste(comp_id_click, asset_type_click,  sep = " | ")
      updateSelectInput(session, "comp_id_custom", selected = "")
      updateSelectInput(session, "comp_id", selected = rv$asset_comp_code_click)
    }else{
      updateSelectInput(session, "comp_id", selected = "")
      updateSelectInput(session, "comp_id_custom", selected = rv$future_ict_table_db()$component_id[input$future_ict_table_rows_selected])
    }
    
    updateSelectInput(session, "priority", selected = rv$future_ict_table_db()$field_test_priority[input$future_ict_table_rows_selected])
    updateSelectInput(session, "con_phase", selected = rv$future_ict_table_db()$phase[input$future_ict_table_rows_selected])
    updateNumericInput(session, "calc_flow_rate", value = rv$future_ict_table_db()$calculated_flow_rate_cfm[input$future_ict_table_rows_selected])
    updateTextAreaInput(session, "notes", value = rv$future_ict_table_db()$notes[input$future_ict_table_rows_selected])
    
    reset("date")
    reset("eq_flow_rate")
    reset("test_volume_cf")
    reset("max_water_depth_ft")
    reset("surcharge")
    reset("time_to_surcharge")
    reset("photos")
    reset("summary_sent")
    
  })
  
  
  
  #add/edit button toggle
  rv$label <- reactive(if(length(input$ict_table_rows_selected) == 0) "Add New" else "Edit Selected")
  observe(updateActionButton(session, "add_test", label = rv$label()))
  
  rv$future_label <- reactive(if(length(input$future_ict_table_rows_selected) == 0) "Add Future Inlet Conveyance Test" else "Edit Selected Future ICT")
  observe(updateActionButton(session, "future_test", label = rv$future_label()))
  
  #add and edit inlet conveyance records
  observeEvent(input$add_test, {
    if(length(input$ict_table_rows_selected) == 0){
      #add to inlet conveyance
      add_test_query <- paste0("INSERT INTO fieldwork.inlet_conveyance (system_id, work_number, site_name_lookup_uid, 
      component_id, facility_id, test_date, con_phase_lookup_uid, calculated_flow_rate_cfm, equilibrated_flow_rate_cfm, 
      test_volume_cf, max_water_depth_ft, surcharge, time_to_surcharge_min, photos_uploaded, summary_report_sent, notes)
    	                  VALUES (", paste(rv$system_id(), rv$work_number(), rv$site_name_lookup_uid(), rv$component_id(), 
    	                                   rv$facility_id(), rv$test_date(), rv$phase_null(), rv$calc_flow_rate(), 
    	                                   rv$eq_flow_rate(), rv$test_volume_cf(), rv$max_water_depth_ft(), 
    	                                   rv$surcharge(), rv$time_to_surcharge(), rv$photos(), rv$summary_sent(), 
    	                                   rv$notes(), sep = ", "), ")")
      
      odbc::dbGetQuery(poolConn, add_test_query)
    }else{
      #edit inlet conveyance
      edit_test_query <- paste0("UPDATE fieldwork.inlet_conveyance SET system_id = ", rv$system_id(), ",
                               work_number = ", rv$work_number(), ", 
                               site_name_lookup_uid = ", rv$site_name_lookup_uid(), ", 
                                component_id = ", rv$component_id(), ", 
                                facility_id = ", rv$facility_id(), ", 
                                test_date = ", rv$test_date(), ", 
                                con_phase_lookup_uid = ", rv$phase_null(), ", 
                                calculated_flow_rate_cfm = ", rv$calc_flow_rate(), ", 
                                equilibrated_flow_rate_cfm = ", rv$eq_flow_rate(), ", 
                                test_volume_cf = ", rv$test_volume_cf(), ", 
                                max_water_depth_ft = ", rv$max_water_depth_ft(), ", 
                                surcharge = ", rv$surcharge(), ", 
                                time_to_surcharge_min = ", rv$time_to_surcharge(), ", 
                                photos_uploaded = ", rv$photos(), ", 
                                summary_report_sent = ", rv$summary_sent(), ", 
                                notes = ", rv$notes(), "
                               WHERE inlet_conveyance_uid = '", rv$ict_table_db()[input$ict_table_rows_selected, 1], "'")

      dbGetQuery(poolConn, edit_test_query)
    }
    
    if(length(input$future_ict_table_rows_selected) > 0){
      odbc::dbGetQuery(poolConn, paste0("DELETE FROM fieldwork.future_inlet_conveyance 
                                        WHERE future_inlet_conveyance_uid = '", rv$future_ict_table_db()[input$future_ict_table_rows_selected, 1], "'"))
    }
    
    rv$future_ict_table_db <- reactive(odbc::dbGetQuery(poolConn, future_ict_table_query()))
    rv$ict_table_db <- reactive(dbGetQuery(poolConn, rv$ict_table_query()))
    rv$all_ict_table_db <- reactive(dbGetQuery(poolConn, rv$all_query()))
    rv$all_future_ict_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$all_future_query()))
    
    reset("comp_id")
    reset("comp_id_custom")
    reset("facility_id")
    reset("date")
    reset("con_phase")
    reset("calc_flow_rate")
    reset("eq_flow_rate")
    reset("test_volume_cf")
    reset("max_water_depth_ft")
    reset("surcharge")
    reset("time_to_surcharge")
    reset("photos")
    reset("summary_sent")
    reset("priority")
    reset("notes")
  })
  
  observeEvent(input$future_test, {
    if(length(input$future_ict_table_rows_selected) == 0){
      #add to inlet conveyance
      add_future_test_query <- paste0("INSERT INTO fieldwork.future_inlet_conveyance (system_id, work_number, site_name_lookup_uid, 
      component_id, facility_id, con_phase_lookup_uid, calculated_flow_rate_cfm, 
      field_test_priority_lookup_uid, notes)
    	                  VALUES (", paste(rv$system_id(), rv$work_number(), rv$site_name_lookup_uid(), rv$component_id(), 
    	                                   rv$facility_id(), rv$phase_null(), rv$calc_flow_rate(), 
    	                                   rv$priority_lookup_uid(),  
    	                                   rv$notes(), sep = ", "), ")")
      print(add_future_test_query)
      
      odbc::dbGetQuery(poolConn, add_future_test_query)
    }else{
      #edit inlet conveyance
      
      edit_future_test_query <- paste0("UPDATE fieldwork.future_inlet_conveyance SET system_id = ", rv$system_id(), ",
                               work_number = ", rv$work_number(), ", 
                               site_name_lookup_uid = ", rv$site_name_lookup_uid(), ", 
                                component_id = ", rv$component_id(), ", 
                                facility_id = ", rv$facility_id(), ", 
                                con_phase_lookup_uid = ", rv$phase_null(), ", 
                                calculated_flow_rate_cfm = ", rv$calc_flow_rate(), ", 
                                field_test_priority_lookup_uid = ", rv$priority_lookup_uid(), ", 
                                notes = ", rv$notes(), "
                               WHERE future_inlet_conveyance_uid = '", rv$future_ict_table_db()[input$future_ict_table_rows_selected, 1], "'")
      
      dbGetQuery(poolConn, edit_future_test_query)
    }
    
    rv$future_ict_table_db <- reactive(odbc::dbGetQuery(poolConn, future_ict_table_query()))
    rv$ict_table_db <- reactive(dbGetQuery(poolConn, rv$ict_table_query()))
    rv$all_ict_table_db <- reactive(dbGetQuery(poolConn, rv$all_query()))
    rv$all_future_ict_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$all_future_query()))
    
    reset("comp_id")
    reset("comp_id_custom")
    reset("facility_id")
    reset("date")
    reset("con_phase")
    reset("calc_flow_rate")
    reset("eq_flow_rate")
    reset("test_volume_cf")
    reset("max_water_depth_ft")
    reset("surcharge")
    reset("time_to_surcharge")
    reset("photos")
    reset("summary_sent")
    reset("priority")
    reset("notes")
  
  }
  )
  
  
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
    reset("comp_id")
    reset("comp_id_custom")
    reset("facility_id")
    reset("date")
    reset("con_phase")
    reset("calc_flow_rate")
    reset("eq_flow_rate")
    reset("test_volume_cf")
    reset("max_water_depth_ft")
    reset("surcharge")
    reset("time_to_surcharge")
    reset("photos")
    reset("summary_sent")
    reset("priority")
    reset("notes")
    removeModal()
  })
  
    # View all ICTs -----------------------------------------------------------

  rv$all_query <- reactive(paste0("SELECT * FROM fieldwork.inlet_conveyance_full ORDER BY test_date DESC"))
  rv$all_ict_table_db <- reactive(dbGetQuery(poolConn, rv$all_query()))
  rv$all_ict_table <- reactive(rv$all_ict_table_db() %>% mutate_at(c("test_date","summary_report_sent") , as.character) %>% 
                             mutate_at(vars(one_of("surcharge", "photos_uploaded")), 
                                       funs(case_when(. == 1 ~ "Yes", 
                                                      . == 0 ~ "No"))) %>% 
                             dplyr::select("system_id", "project_name", "component_id", "test_date", "phase",
                                           "calculated_flow_rate_cfm", "equilibrated_flow_rate_cfm", "test_volume_cf",
                                           "max_water_depth_ft", "surcharge", "time_to_surcharge_min", 
                                           "photos_uploaded", "summary_report_sent", "turnaround_days", "notes"))
  
  #show table of ICTs
  output$all_ict_table <- renderReactable(
    reactable(rv$all_ict_table()[, 1:14], 
              columns = list(
                system_id = colDef(name = "System ID"),
                project_name = colDef(name = "Project Name"),
                component_id = colDef(name = "Component ID"),
                test_date = colDef(name = "Test Date"),
                phase = colDef(name = "Phase"),
                calculated_flow_rate_cfm = colDef(name = "Calculated Flow Rate (CFM)"),
                equilibrated_flow_rate_cfm = colDef(name = "Equilibrated Flow Rate (CFM)"),
                test_volume_cf = colDef(name = "Test Volume (CF)"),
                max_water_depth_ft = colDef(name = "Max Water Depth (ft)"),
                surcharge = colDef(name = "Surcharge"),
                time_to_surcharge_min = colDef(name = "Time to Surcharge (min)"),
                photos_uploaded = colDef(name = "Photos Uploaded", style = function(value){
                  if(is.na(value) | value == "No"){
                    color = "#FFFC1C"
                  }else{
                    color = "#FFFFFF"
                  }
                  list(backgroundColor = color, fontweight = "bold")
                }), 
                summary_report_sent = colDef(name = "Summary Date", style = function(value){
                  if(is.na(value) | value == "No"){
                    color = "#FFFC1C"
                  }else{
                    color = "#FFFFFF"
                  }
                  list(backgroundColor = color, fontweight = "bold")
                }), 
                turnaround_days = colDef(name = "Turnaround (Days)")
              ),
              fullWidth = TRUE,
              selection = 'single', 
              searchable = TRUE,
              onClick = "select",
              selectionId = ns("ict_selected"),
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 25, 50),
              defaultPageSize = 10,
              height = 750,
              details = function(index){
                nest <- rv$all_ict_table()[rv$all_ict_table_db()$inlet_conveyance_uid == rv$all_ict_table_db()$inlet_conveyance_uid[index], ][15]
                htmltools::div(style = "padding:16px", 
                               reactable(nest, 
                                         columns = list(notes = colDef(name = "Notes")))
                )
              }
    )
    )
  
  #select row in full ict table
  observeEvent(input$ict_selected, {
    
    #set all inputs to null
    updateSelectInput(session, "system_id", selected = "")
    updateSelectInput(session, "work_number", selected = "")
    updateSelectInput(session, "site_name", selected = "")
    
    #check for system id, then work number, then site name
    if(!is.na(rv$all_ict_table_db()$system_id[input$ict_selected])){
    updateSelectInput(session, "system_id", selected = rv$all_ict_table_db()$system_id[input$ict_selected])
    }else if(!is.na(rv$all_ict_table_db()$work_number[input$ict_selected])){
    updateSelectInput(session, "work_number", selected = rv$all_ict_table_db()$work_number[input$ict_selected])
    }else if(!is.na(rv$all_ict_table_db()$site_name[input$ict_selected]) > 0){
    updateSelectInput(session, "site_name", selected = rv$all_ict_table_db()$site_name[input$ict_selected])
    }
    
    updateTabsetPanel(session = parent_session, "inTabset", selected = "ict_tab")
    updateReactable("all_future_ict_table", selected = NA)
  })
  
  observeEvent(rv$ict_table_db(), {
    if(length(input$ict_selected) > 0){
      ict_row <- which(rv$ict_table_db()$inlet_conveyance_uid == rv$all_ict_table_db()$inlet_conveyance_uid[input$ict_selected], arr.ind = TRUE)
      dataTableProxy('ict_table') %>% selectRows(ict_row)
    }
  })
  
  #View Future ICTs
  
  rv$all_future_query <- reactive(paste0("SELECT * FROM fieldwork.future_inlet_conveyance_full ORDER BY field_test_priority_lookup_uid DESC"))
  rv$all_future_ict_table_db <- reactive(dbGetQuery(poolConn, rv$all_future_query()))
  rv$all_future_ict_table <- reactive(rv$all_future_ict_table_db() %>% 
                                 dplyr::select("system_id", "project_name", "component_id", "phase",
                                               "calculated_flow_rate_cfm", "field_test_priority", "notes"))
  
  #show table of ICTs
  output$all_future_ict_table <- renderReactable(
    reactable(rv$all_future_ict_table()[, 1:6], 
              columns = list(
                system_id = colDef(name = "System ID"),
                project_name = colDef(name = "Project Name"),
                component_id = colDef(name = "Component ID"),
                phase = colDef(name = "Phase"),
                calculated_flow_rate_cfm = colDef(name = "Calculated Flow Rate (CFM)"),
                field_test_priority = colDef(name = "Priority")
              ),
              fullWidth = TRUE,
              selection = 'single', 
              searchable = TRUE,
              onClick = "select",
              selectionId = ns("future_ict_selected"),
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 25, 50),
              defaultPageSize = 10,
              height = 750,
              details = function(index){
                nest <- rv$all_future_ict_table()[rv$all_future_ict_table_db()$future_inlet_conveyance_uid == rv$all_future_ict_table_db()$future_inlet_conveyance_uid[index], ][7]
                htmltools::div(style = "padding:16px", 
                               reactable(nest, 
                                         columns = list(notes = colDef(name = "Notes")))
                )
              }
    )
  )
  
  #select fow in full ict table
  observeEvent(input$future_ict_selected, {
    
    #set all inputs to null
    updateSelectInput(session, "system_id", selected = "")
    updateSelectInput(session, "work_number", selected = "")
    updateSelectInput(session, "site_name", selected = "")
    
    #check for system id, then work number, then site name
    if(!is.na(rv$all_future_ict_table_db()$system_id[input$future_ict_selected])){
      updateSelectInput(session, "system_id", selected = rv$all_future_ict_table_db()$system_id[input$future_ict_selected])
    }else if(!is.na(rv$all_future_ict_table_db()$work_number[input$future_ict_selected])){
      updateSelectInput(session, "work_number", selected = rv$all_future_ict_table_db()$work_number[input$future_ict_selected])
    }else if(!is.na(rv$all_future_ict_table_db()$site_name[input$future_ict_selected]) > 0){
      updateSelectInput(session, "site_name", selected = rv$all_future_ict_table_db()$site_name[input$future_ict_selected])
    }
    
    updateTabsetPanel(session = parent_session, "inTabset", selected = "ict_tab")
    updateReactable("all_ict_table", selected = NA)
  })
  
  observeEvent(rv$future_ict_table_db(), {
    if(length(input$future_ict_selected) > 0){
      future_ict_row <- which(rv$future_ict_table_db()$future_inlet_conveyance_uid == rv$all_future_ict_table_db()$future_inlet_conveyance_uid[input$future_ict_selected], arr.ind = TRUE)
      dataTableProxy('future_ict_table') %>% selectRows(future_ict_row)
    }
  })
  
}
