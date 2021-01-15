#Capture Efficiency Test (CET) tabs
#This has a tab dropdown with two tabs, one for adding SRTs and one for viewing all SRTs

capture_efficiencyUI <- function(id, label = "capture_efficiency", sys_id, high_flow_type, priority, html_req, con_phase, future_req, cet_asset_type){
  ns <- NS(id)
  navbarMenu("Capture Efficiency",
             tabPanel("Add/Edit Capture Efficiency Test", value = "cet_tab", 
                      useShinyjs(),
                      titlePanel("Add Capture Efficiency Test"), 
                      sidebarPanel(
                        selectInput(ns("system_id"), future_req(html_req("System ID")), choices = c("", sys_id), selected = NULL), 
                        selectInput(ns("cet_comp_id"),  future_req(html_req("Component ID")), choices = c(""), selected = NULL),
                        textInput(ns("cet_comp_id_custom"),  future_req(html_req("Custom Component ID"))),
                        conditionalPanel(condition = "input.cet_comp_id_custom", 
                                         ns = ns, 
                                         selectInput(ns("user_input_asset_type"), "Asset Type", 
                                                     choices = c("", cet_asset_type$asset_type), selected = NULL)),
                        disabled(textInput(ns("facility_id"), future_req(html_req("Facility ID")))),
                        fluidRow(
                          column(6, dateInput(ns("cet_date"), html_req("Test Date"), value = as.Date(NA))), 
                          column(6, selectInput(ns("con_phase"), html_req("Construction Phase"), choices = c("", con_phase$phase), selected = NULL))),
                        fluidRow(
                        column(6, selectInput(ns("low_flow_bypass"), html_req("Low Flow Bypass Observed"), choices = c("", "Yes" = "1", "No" = "0"), selected = NULL)), 
                        column(6, numericInput(ns("low_flow_efficiency"), html_req("Low Flow Efficiency %"), value = NA, min = 0, max = 100))),
                        fluidRow(
                        column(6, selectInput(ns("est_high_flow_efficiency"), "Estimated High Flow Efficiency ", 
                                    choices = c("", high_flow_type$est_high_flow_efficiency), selected = NULL)),
                        column(6, numericInput(ns("high_flow_efficiency"), "High Flow Efficiency %", value = NA, min = 0, max = 100))), 
                        conditionalPanel(condition = "input.cet_date === null", 
                                         ns = ns, 
                                         selectInput(ns("priority"), "Future Test Priority", 
                                                     choices = c("", priority$field_test_priority), selected = NULL)),
                        textAreaInput(ns("cet_notes"), "Notes", height = '90px'), 
                        conditionalPanel(condition = "input.cet_date === null", 
                                         ns = ns, 
                                         actionButton(ns("future_cet"), "Add Future Capture Efficiency Test")),
                        actionButton(ns("add_cet"), "Add Capture Efficiency Test"), 
                        actionButton(ns("clear_cet"), "Clear All Fields"),
                        fluidRow(
                          HTML(paste(html_req(""), " indicates required field for complete tests. ", future_req(""), " indicates required field for future tests.")))
                      ), 
                      mainPanel(
                        conditionalPanel(condition = "input.system_id", 
                                         ns  = ns, 
                        h4(textOutput(ns("future_header"))), #("Capture Efficiency Tests at this SMP"), 
                                DTOutput(ns("future_cet_table")),
                                h4(textOutput(ns("header"))),
                                DTOutput(ns("cet_table"))),
                                h6("1) Low flow using truck water tank, flow estimated at 2-5 CFM \n"),
                                h6("2) If no hydrant is nearby to test high flow, efficiency is simply predicted"), 
                                h6("3) High flow uisng nearby hydrant, flow estimated at 20-25 CFM"))
             ),
             tabPanel("View Capture Efficiency Tests", value = "view_cet", 
                      titlePanel("All Capture Efficiency Tests"), 
                      DTOutput(ns("all_cet_table"))
             ), 
             tabPanel("View Future Capture Efficiency Tests", value = "view_future_cet", 
                      titlePanel("All Future Capture Efficiency Tests"), 
                      DTOutput(ns("all_future_cet_table")))
  )
}

capture_efficiency <- function(input, output, session, parent_session, poolConn, high_flow_type, con_phase, cet_asset_type, deploy){
  
  #define ns to use in modals
  ns <- session$ns
  
  rv <- reactiveValues()
  
  #upon clicking through dialogue box after doing future deployment/premonitoring inspection 
  #update selected system based on deployment
  observeEvent(deploy$refresh_cet(), {
    if(deploy$refresh_cet() > 0){
      updateSelectInput(session, "system_id", selected = NULL)
      updateSelectInput(session, "system_id", selected = deploy$system_id())
    }
  })
  
  #Get the Project name, combine it with System ID, and create a reactive header
  rv$sys_and_name_step <- reactive(odbc::dbGetQuery(poolConn, paste0("select system_id, project_name from project_names where system_id = '", input$system_id, "'")))
  
  rv$sys_and_name <- reactive(paste(rv$sys_and_name_step()$system_id[1], rv$sys_and_name_step()$project_name[1]))
  
  output$header <- renderText(
    paste("Capture Efficiency Tests at", rv$sys_and_name())
  )
  
  output$future_header <- renderText(
    paste("Future Capture Efficiency Tests at", rv$sys_and_name())
  )
  
  #adjust query to accurately target NULL values once back on main server
  rv$component_and_asset_query <- reactive(paste0("SELECT component_id, asset_type FROM smpid_facilityid_componentid_inlets_limited WHERE system_id = '", input$system_id, "' AND component_id != 'NULL'"))
  rv$component_and_asset <- reactive(odbc::dbGetQuery(poolConn, rv$component_and_asset_query()))
  
  rv$asset_comp <- reactive(rv$component_and_asset() %>% 
                              mutate("asset_comp_code" = paste(component_id, asset_type, sep = " | ")))
  
  rv$asset_combo <- reactive(rv$asset_comp()$asset_comp_code)
  
  observe(updateSelectInput(session, "cet_comp_id", choices = c("", rv$asset_combo())))
  
  #get the row/case that has the selected asset_comp_code
  rv$select_combo_row <- reactive(rv$asset_comp() %>% 
                                 dplyr::filter(asset_comp_code == input$cet_comp_id))
  
  #get component id from the chosen asset_comp_code, then use to get facility id
  rv$select_component_id <- reactive(rv$select_combo_row() %>% 
                                    dplyr::select(component_id) %>%
                                    dplyr::pull() %>% 
                                      dplyr::first())
  
  #get facility ID. Either use SMP footprint (for an unknown component) or the facility ID of the existing component
  rv$facility_id <- reactive(if(input$cet_comp_id != ""){
        odbc::dbGetQuery(poolConn, paste0(
          "SELECT facility_id from smpid_facilityid_componentid_inlets_limited WHERE component_id = '", rv$select_component_id(), "'"))[1,1]
  }else if(input$system_id != ""){
    odbc::dbGetQuery(poolConn, paste0("SELECT facility_id from smpid_facilityid_componentid_inlets_limited WHERE component_id is NULL and system_id = '", input$system_id, "' LIMIT 1"))
  }else{
    ""
  }
  )
  
  observe(updateSelectInput(session, "facility_id", selected = rv$facility_id()))

  #lookup priority uid
  rv$priority_lookup_uid_query <- reactive(paste0("select field_test_priority_lookup_uid from fieldwork.field_test_priority_lookup where field_test_priority = '", input$priority, "'"))
  rv$priority_lookup_uid_step <- reactive(dbGetQuery(poolConn, rv$priority_lookup_uid_query()))
  rv$priority_lookup_uid <- reactive(if(nchar(input$priority) == 0) "NULL" else paste0("'", rv$priority_lookup_uid_step(), "'"))
  
  #get the table of CETs
  rv$cet_table_query <- reactive(paste0("SELECT * FROM fieldwork.capture_efficiency_full WHERE system_id = '", input$system_id, "'"))
  rv$cet_table_db <- reactive(dbGetQuery(poolConn, rv$cet_table_query()))
  rv$cet_table <- reactive(rv$cet_table_db() %>% mutate_at("test_date", as.Date) %>% 
                             mutate_at(vars(one_of("low_flow_bypass_observed")), 
                                       funs(case_when(. == 1 ~ "Yes", 
                                                      . == 0 ~ "No"))) %>% 
                             dplyr::select(-1) %>% 
                             dplyr::select(-"facility_id", -"project_name", -"system_id", -"public"))
  
  #enable/disable buttons 
  #conditions need to be set up this way; changing them all to nchar, or all to length, or all to input != "" won't work
  observe(toggleState(id = "add_cet", condition = nchar(input$system_id) > 0 & length(input$cet_date) > 0 & 
                        (nchar(input$cet_comp_id) >0 | nchar(input$cet_comp_id_custom) > 0) & nchar(input$con_phase) > 0) &
            length(input$low_flow_bypass) > 0 & length(input$low_flow_efficiency) > 0)
  
  #if one of the component id's is filled out, disable the other
  observe(toggleState(id = "cet_comp_id_custom", condition = input$cet_comp_id == ""))
  
  observe(toggleState(id = "cet_comp_id", condition = input$cet_comp_id_custom == ""))
  
  #toggle state for future srt
  observe(toggleState(id = "future_cet", condition = nchar(input$system_id) > 0 & (nchar(input$cet_comp_id) > 0 | nchar(input$cet_comp_id_custom) > 0)))
  
  #change type from text to uid
  rv$est_high_flow_efficiency <- reactive(high_flow_type %>% dplyr::filter(est_high_flow_efficiency == input$est_high_flow_efficiency) %>% 
                                            select(est_high_flow_efficiency_lookup_uid) %>% pull())
  
  rv$phase <- reactive(con_phase %>% dplyr::filter(phase == input$con_phase) %>% 
                         select(con_phase_lookup_uid) %>% pull())
  
  rv$phase_null <- reactive(if(nchar(input$con_phase) == 0) "NULL" else paste0("'", rv$phase(), "'"))
  
  rv$user_input_asset_type <- reactive(if(nchar(input$cet_comp_id_custom) > 0 & nchar(input$user_input_asset_type) > 0){
    paste0("'", input$user_input_asset_type, "'")
  }else{
    "NULL"
  }
  )
  
  #toggle state for data depending on whether a test date is included
  observe(toggleState(id = "low_flow_bypass", condition = length(input$cet_date) > 0))
  observe(toggleState(id = "low_flow_efficiency", condition = length(input$cet_date) > 0))
  #toggle state based on test date and whether est OR real is filled out
  observe(toggleState(id = "est_high_flow_efficiency", condition = length(input$cet_date) > 0 & is.na(input$high_flow_efficiency)))
  observe(toggleState(id = "high_flow_efficiency", condition = length(input$cet_date) > 0 &  nchar(input$est_high_flow_efficiency) == 0))
  
  output$cet_table <- renderDT(
    datatable(rv$cet_table(), 
              colnames = c('Test Date', 'Component ID', 'Construction Phase', 'Low Flow Bypass Observed', 
                           'Low Flow<span style="color:DodgerBlue"><sup>1</sup></span style="color:DodgerBlue"> Efficiency %', 
                           'Est. High Flow<span style="color:DodgerBlue"><sup>2</sup></span style="color:DodgerBlue"> Efficiency', 
                           'High Flow<span style="color:DodgerBlue"><sup>3</sup></span style="color:DodgerBlue"> Efficiency %', 'Asset Type', 'Notes'),
              selection = 'single', 
              style = 'bootstrap',
              class = 'table-responsive, table-hover', 
              escape = FALSE,
              options = list(
                columnDefs = list(list(className = 'dt-left', targets = "_all"))
              )
    ))
  
  #query future CETs
  future_cet_table_query <- reactive(paste0("SELECT * FROM fieldwork.future_capture_efficiency_full WHERE system_id = '", input$system_id, "' order by field_test_priority_lookup_uid"))
  rv$future_cet_table_db <- reactive(odbc::dbGetQuery(poolConn, future_cet_table_query()))
  
  rv$future_cet_table <- reactive(rv$future_cet_table_db() %>% 
                                    dplyr::select("system_id", "component_id", "phase", "field_test_priority", "notes"))
  
  output$future_cet_table <- renderDT(
    rv$future_cet_table(), 
    selection = 'single', 
    style = 'bootstrap', 
    class = 'table-responsive, table-hover', 
    colnames = c('System ID', 'Component ID', 'Phase', 'Priority', 'Notes'), 
    options = list(
      columnDefs = list(list(className = 'dt-left', targets = "_all"))
    ),
  )
  
  #on click
  observeEvent(input$future_cet_table_rows_selected, {
    #deselect from other table
    dataTableProxy('cet_table') %>% selectRows(NULL)
    
    #update inputs
    #update to values from selected row
    updateSelectInput(session, 'con_phase', selected = rv$future_cet_table()$phase[input$future_cet_table_rows_selected])
    
    updateSelectInput(session, "priority", selected = rv$future_cet_table()$field_test_priority[input$future_cet_table_rows_selected])
    updateTextAreaInput(session, "cet_notes", value = rv$future_cet_table()$notes[input$future_cet_table_rows_selected])
    
    #get facility id
    rv$future_fac <- rv$future_cet_table_db()$facility_id[input$future_cet_table_rows_selected]
    future_comp_id_query <- paste0("select distinct component_id from smpid_facilityid_componentid_inlets_limited where facility_id = '", rv$future_fac, "' 
        AND component_id IS NOT NULL")
    
    f_comp_id_step <- odbc::dbGetQuery(poolConn, future_comp_id_query) %>% pull()
    #determine whether component id exists and is useful
    f_comp_id_click <- if(length(f_comp_id_step) > 0) f_comp_id_step else "NA"
    
    if(nchar(f_comp_id_click) > 2){
      f_asset_type_click <- dplyr::filter(rv$asset_comp(), component_id == f_comp_id_click) %>% select(asset_type) %>% pull()
      #combine asset type, ow, and component id
      rv$f_asset_comp_code_click = paste(f_comp_id_click, f_asset_type_click,  sep = " | ")
      updateSelectInput(session, "cet_comp_id_custom", selected = "")
      updateSelectInput(session, "cet_comp_id", selected = rv$f_asset_comp_code_click)
      updateSelectInput(session, "user_input_asset_type", selected = "")
    }else{
      updateSelectInput(session, "cet_comp_id", selected = "")
      updateSelectInput(session, "cet_comp_id_custom", selected = rv$future_cet_table()[input$future_cet_table_rows_selected, 2])
      updateSelectInput(session, "user_input_asset_type", selected = rv$future_cet_table()$asset_type[input$future_cet_table_rows_selected])
    }
    
    reset("cet_date")
    reset("low_flow_bypass")
    reset("low_flow_efficiency")
    reset("est_high_flow_efficiency")
    reset("high_flow_efficiency")
  })
  
  
  observeEvent(input$cet_table_rows_selected, {
    
    dataTableProxy('future_cet_table') %>% selectRows(NULL)
    #reset("cet_comp_id")
    #reset("cet_comp_id_custom")
    
    #get facility id from table
    rv$fac <- (rv$cet_table_db()[input$cet_table_rows_selected, 5])
    #get component id
    comp_id_query <- paste0("select distinct component_id from smpid_facilityid_componentid_inlets_limited where facility_id = '", rv$fac, "' 
        AND component_id IS NOT NULL")
    comp_id_step <- odbc::dbGetQuery(poolConn, comp_id_query) %>% pull()
    #determine whether component id exists and is useful
    comp_id_click <- if(length(comp_id_step) > 0) comp_id_step else "NA"
    
    #get asset type - base on component id (if exists)
    
    if(nchar(comp_id_click) > 2){
    asset_type_click <- dplyr::filter(rv$asset_comp(), component_id == comp_id_click) %>% select(asset_type) %>% pull()
      #combine asset type, ow, and component id
      rv$asset_comp_code_click = paste(comp_id_click, asset_type_click,  sep = " | ")
      updateSelectInput(session, "cet_comp_id_custom", selected = "")
      updateSelectInput(session, "cet_comp_id", selected = rv$asset_comp_code_click)
      updateSelectInput(session, "user_input_asset_type", selected = "")
    }else{
      updateSelectInput(session, "cet_comp_id", selected = "")
      updateSelectInput(session, "cet_comp_id_custom", selected = rv$cet_table_db()[input$cet_table_rows_selected, 4])
      updateSelectInput(session, "user_input_asset_type", selected = rv$cet_table_db()$asset_type[input$cet_table_rows_selected])
    }
    
    # updateSelectInput(session, "system_id", selected = rv$cet_table_db()[input$cet_table_rows_selected, 2])
    #updateSelectInput(session, "cet_comp_id", selected = rv$cet_table_db()[input$cet_table_rows_selected, 4])
    
    updateDateInput(session, "cet_date", value = rv$cet_table_db()$test_date[input$cet_table_rows_selected])
    updateSelectInput(session, "con_phase", selected = rv$cet_table_db()$phase[input$cet_table_rows_selected])
    updateSelectInput(session, "low_flow_bypass", selected = rv$cet_table_db()$low_flow_bypass_observed[input$cet_table_rows_selected])
    updateNumericInput(session, "low_flow_efficiency", value = rv$cet_table_db()$low_flow_efficiency_pct[input$cet_table_rows_selected])
    updateSelectInput(session, "est_high_flow_efficiency", selected = rv$cet_table()$est_high_flow_efficiency[input$cet_table_rows_selected])
    updateNumericInput(session, "high_flow_efficiency", value = rv$cet_table_db()$high_flow_efficiency_pct[input$cet_table_rows_selected])
    updateTextAreaInput(session, "cet_notes", value = rv$cet_table_db()$notes[input$cet_table_rows_selected])
  })
  
  
  #set inputs to reactive values so "NULL" can be entered
  #important to correctly place quotations
  #replace quotes with double quotes to protect against SQL injections
  rv$est_hfe <- reactive(if(length(rv$est_high_flow_efficiency()) == 0) "NULL" else paste0("'", rv$est_high_flow_efficiency(), "'"))
  rv$cet_notes_step <- reactive(gsub('\'', '\'\'', input$cet_notes))
  rv$cet_notes <- reactive(if(nchar(rv$cet_notes_step()) == 0) "NULL" else paste0("'", rv$cet_notes_step(), "'"))
  
  rv$low_flow_bypass <- reactive(if(nchar(input$low_flow_bypass) == 0 | input$low_flow_bypass == "N/A") "NULL" else paste0("'", input$low_flow_bypass, "'"))
  rv$low_flow_efficiency <- reactive(if(is.na(input$low_flow_efficiency)) "NULL" else paste0("'", input$low_flow_efficiency, "'"))
  rv$high_flow_efficiency <- reactive(if(is.na(input$high_flow_efficiency)) "NULL" else paste0("'", input$high_flow_efficiency, "'"))
  
  #pick a component ID
  #protect against SQL injection
  rv$cet_comp_id_custom_step <- reactive(gsub('\'', '\'\'', input$cet_comp_id_custom))
  rv$cet_comp_id_custom <- reactive(if(nchar(rv$cet_comp_id_custom_step()) == 0) "NULL" else paste0("'", rv$cet_comp_id_custom_step(), "'"))
  
  rv$cet_comp_id <- reactive(if(nchar(input$cet_comp_id) > 0){
    paste0("'",rv$select_component_id(), "'")
  }else{
     rv$cet_comp_id_custom()
  })
  
  #add/edit button toggle
  rv$label <- reactive(if(length(input$cet_table_rows_selected) == 0) "Add New" else "Edit Selected")
  observe(updateActionButton(session, "add_cet", label = rv$label()))
  
  rv$future_label <- reactive(if(length(input$future_cet_table_rows_selected) == 0) "Add Future Capture Efficiency Test" else "Edit Selected Future CET")
  observe(updateActionButton(session, "future_cet", label = rv$future_label()))
  
  #set efficiency = 100 if bypass = no, and disable field
  observeEvent(input$low_flow_bypass, {
    if(input$low_flow_bypass == 0){
      updateNumericInput(session, "low_flow_efficiency", value = 100)
      disable("low_flow_efficiency")
    }else if(nchar(input$low_flow_bypass) == 0){
      disable("low_flow_efficiency")
    }else{
      enable("low_flow_efficiency")
      reset("low_flow_efficiency")
    }
  })
  
  #add and edit future capture efficiency records
  observeEvent(input$future_cet, {
    if(length(input$future_cet_table_rows_selected) == 0){
      add_future_cet_query <- paste0("INSERT INTO fieldwork.future_capture_efficiency (system_id, component_id, 
                                     facility_id, con_phase_lookup_uid, notes, field_test_priority_lookup_uid, user_input_asset_type) 
                                     VALUES ('", input$system_id, "',", rv$cet_comp_id(), ", '", rv$facility_id(), "', ", rv$phase_null(), "
                                     ,", rv$cet_notes(), ", ", rv$priority_lookup_uid(), ", ", rv$user_input_asset_type(), ")")
      odbc::dbGetQuery(poolConn, add_future_cet_query)
    }else{
      edit_future_cet_query <- paste0("UPDATE fieldwork.future_capture_efficiency SET component_id = ", rv$cet_comp_id(), ", 
                               facility_id = '",rv$facility_id(), "',
                               con_phase_lookup_uid = ", rv$phase_null(), ", 
                               notes = ", rv$cet_notes(), ",
                               field_test_priority_lookup_uid = ", rv$priority_lookup_uid(), ", 
                               user_input_asset_type = ", rv$user_input_asset_type(), "
                                      WHERE future_capture_efficiency_uid = '", rv$future_cet_table_db()[input$future_cet_table_rows_selected, 1], "'")
      
      odbc::dbGetQuery(poolConn, edit_future_cet_query)
    }
    
    #query updated table
    rv$future_cet_table_db <- reactive(odbc::dbGetQuery(poolConn, future_cet_table_query()))
    rv$all_future_cet_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$all_future_cet_query))
    
    reset("cet_comp_id")
    reset("cet_comp_id_custom")
    reset("cet_date")
    reset("phase")
    reset("low_flow_bypass")
    reset("low_flow_efficiency")
    reset("est_high_flow_efficiency")
    reset("high_flow_efficiency")
    reset("cet_notes")
    reset("priority")
  })
  
  #add and edit capture efficiency records
  observeEvent(input$add_cet, {
    if(length(input$cet_table_rows_selected) == 0){
      #add to capture efficiency
      add_cet_query <- paste0("INSERT INTO fieldwork.capture_efficiency (system_id, test_date, component_id,
      facility_id, con_phase_lookup_uid, low_flow_bypass_observed,
      low_flow_efficiency_pct, est_high_flow_efficiency_lookup_uid, high_flow_efficiency_pct, notes, user_input_asset_type)
    	                  VALUES ('", input$system_id, "','",  input$cet_date, "',", rv$cet_comp_id(), ",'", rv$facility_id(), "','", rv$phase(), "', 
    	                        ", rv$low_flow_bypass(), ",
                              ", rv$low_flow_efficiency(), ", ", rv$est_hfe(), ", ", rv$high_flow_efficiency(), ", 
                              ", rv$cet_notes(), ", ", rv$user_input_asset_type(), ")")
      
      odbc::dbGetQuery(poolConn, add_cet_query)
    }else{
      #edit capture efficiency
      edit_cet_query <- paste0("UPDATE fieldwork.capture_efficiency SET system_id = '", input$system_id, "', 
                               component_id = ", rv$cet_comp_id(), ", 
                               facility_id = '",rv$facility_id(), "',
                               test_date = '", input$cet_date, "', 
                               con_phase_lookup_uid = '", rv$phase(), "',
                               low_flow_bypass_observed = ", rv$low_flow_bypass(), ", 
                               low_flow_efficiency_pct = ", rv$low_flow_efficiency(), ", 
                               est_high_flow_efficiency_lookup_uid = ", rv$est_hfe(), ", 
                               high_flow_efficiency_pct = ", rv$high_flow_efficiency(), ", 
                               notes = ", rv$cet_notes(), ", 
                               user_input_asset_type = ", rv$user_input_asset_type(), "
                               WHERE capture_efficiency_uid = '", rv$cet_table_db()[input$cet_table_rows_selected, 1], "'")
      
      dbGetQuery(poolConn, edit_cet_query)
    }
    
    if(length(input$future_cet_table_rows_selected) > 0){
      odbc::dbGetQuery(poolConn, paste0("DELETE FROM fieldwork.future_capture_efficiency 
                                        WHERE future_capture_efficiency_uid = '", rv$future_cet_table_db()[input$future_cet_table_rows_selected, 1], "'"))
    }
    
    rv$future_cet_table_db <- reactive(odbc::dbGetQuery(poolConn, future_cet_table_query()))
    rv$cet_table_db <- reactive(dbGetQuery(poolConn, rv$cet_table_query()))
    rv$all_cet_table_db <- reactive(dbGetQuery(poolConn, rv$all_query()))
    rv$all_future_cet_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$all_future_cet_query))
    
    
    reset("cet_comp_id")
    reset("cet_comp_id_custom")
    reset("cet_date")
    reset("phase")
    reset("low_flow_bypass")
    reset("low_flow_efficiency")
    reset("low_flow_efficiency")
    reset("est_high_flow_efficiency")
    reset("high_flow_efficiency")
    reset("cet_notes")
    reset("priority")
  })
  
  observeEvent(input$clear_cet, {
    showModal(modalDialog(title = "Clear All Fields", 
                          "Are you sure you want to clear all fields on this tab?", 
                          modalButton("No"), 
                          actionButton(ns("confirm_clear_cet"), "Yes")))
  })
  
  observeEvent(input$confirm_clear_cet, {
    reset("cet_comp_id")
    reset("cet_comp_id_custom")
    reset("system_id")
    reset("cet_date")
    reset("phase")
    reset("low_flow_bypass")
    reset("low_flow_efficiency")
    reset("low_flow_efficiency")
    reset("est_high_flow_efficiency")
    reset("high_flow_efficiency")
    reset("cet_notes")
    reset("priority")
    removeModal()
  })
  
  # View all CETs ----
  
  rv$all_query <- reactive(paste0("SELECT * FROM fieldwork.capture_efficiency_full ORDER BY test_date DESC"))
  rv$all_cet_table_db <- reactive(dbGetQuery(poolConn, rv$all_query())) 
  rv$all_cet_table <- reactive(rv$all_cet_table_db() %>% 
                                 mutate_at("test_date", as.Date) %>% 
                                 mutate_at(vars(one_of("low_flow_bypass_observed")),
                                           funs(case_when(. == 1 ~ "Yes", 
                                                          . == 0 ~ "No"))) %>% 
                                 dplyr::select("system_id", "project_name", "test_date", "component_id", "phase", 
                                               "low_flow_bypass_observed", "low_flow_efficiency_pct", "est_high_flow_efficiency", "high_flow_efficiency_pct", "asset_type", "notes"))
  
  output$all_cet_table <- renderDT(
    datatable(
    rv$all_cet_table(),
    selection = 'single', 
    style = 'bootstrap',
    class = 'table-responsive, table-hover',
    colnames = c('System ID', 'Project Name', 'Test Date', 'Component ID', 'Construction Phase', 'Low Flow Bypass Observed',
                 'Low Flow Efficiency %', 'Est. High Flow Efficiency', 'High Flow Efficiency %', 'Asset Type', 'Notes'),
    options = list(
      columnDefs = list(list(className = 'dt-left', targets = "_all"))
    ), 
    rownames = FALSE
    )
  )
  
  observeEvent(input$all_cet_table_rows_selected, {
    updateSelectInput(session, "system_id", selected = "")
    updateSelectInput(session, "system_id", selected = rv$all_cet_table()$system_id[input$all_cet_table_rows_selected])
    updateTabsetPanel(session = parent_session, "inTabset", selected = "cet_tab")
  })
  
  observeEvent(rv$cet_table_db(), {
    if(length(input$all_cet_table_rows_selected) > 0){
      cet_row <- which(rv$cet_table_db()$capture_efficiency_uid == rv$all_cet_table_db()$capture_efficiency_uid[input$all_cet_table_rows_selected], arr.ind = TRUE)
      dataTableProxy('cet_table') %>% selectRows(cet_row)
    }
  })
  
  #View all Future CETs
  #unclear why some things need to have () / be reactive and some don't but this works
  rv$all_future_cet_query <- "SELECT * FROM fieldwork.future_capture_efficiency_full order by field_test_priority_lookup_uid"
  rv$all_future_cet_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$all_future_cet_query))
  rv$all_future_cet_table <- reactive(rv$all_future_cet_table_db() %>% 
                                        dplyr::select("system_id", "project_name", "component_id", "phase", "asset_type", "field_test_priority",  "notes"))
  
  output$all_future_cet_table <- renderDT(
    datatable(
    rv$all_future_cet_table(), 
    selection = 'single', 
    style = 'bootstrap', 
    class = 'table-responsive, table-hover', 
    colnames = c('System ID', 'Project Name', 'Component ID', 'Construction Phase', 
                 'Asset Type', 'Priority', 'Notes'), 
    options = list(
      columnDefs = list(list(className = 'dt-left', targets = 0:5))
      
    ), 
    rownames = FALSE
    )
  )
  
  #update system id and change tabs
  observeEvent(input$all_future_cet_table_rows_selected, {
    updateSelectInput(session, "system_id", selected = "")
    updateSelectInput(session, "system_id", selected = rv$all_future_cet_table()$system_id[input$all_future_cet_table_rows_selected])
    updateTabsetPanel(session = parent_session, "inTabset", selected = "cet_tab")
  })
  
  #then once the system id is updated, select a test from a table
  #this is broken into two steps so they happen in order
  observeEvent(rv$future_cet_table_db(), {
    if(length(input$all_future_cet_table_rows_selected)){
      future_cet_row <- which(rv$future_cet_table_db()$future_capture_efficiency_uid == rv$all_future_cet_table_db()$future_capture_efficiency_uid[input$all_future_cet_table_rows_selected], arr.ind = TRUE)
      dataTableProxy('future_cet_table') %>% selectRows(future_cet_row)
    }
  })
  
}