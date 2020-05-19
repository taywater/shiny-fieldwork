capture_efficiencyUI <- function(id, label = "capture_efficiency", sys_id, high_flow_type, html_req, con_phase){
  ns <- NS(id)
  navbarMenu("Capture Efficiency",
             tabPanel("Add/Edit Capture Efficiency Test", value = "cet_tab", 
                      useShinyjs(),
                      titlePanel("Add Capture Efficiency Test"), 
                      sidebarPanel(
                        selectInput(ns("cet_system_id"), html_req("System ID"), choices = c("", sys_id), selected = NULL), 
                        selectInput(ns("cet_comp_id"), "Component ID", choices = c(""), selected = NULL), 
                        textInput(ns("cet_comp_id_custom"), "Custom Component ID"),
                        disabled(textInput(ns("facility_id"), html_req("Facility ID"))),
                        dateInput(ns("cet_date"), html_req("Test Date"), value = as.Date(NA)), 
                        selectInput(ns("con_phase"), html_req("Construction Phase"), choices = c("", con_phase$phase), selected = NULL),
                        fluidRow(
                        column(6, selectInput(ns("low_flow_bypass"), "Low Flow Bypass Observed", choices = c("", "Yes" = "1", "No" = "0"), selected = NULL)), 
                        column(6, numericInput(ns("low_flow_efficiency"), "Low Flow Efficiency %", value = NA, min = 0, max = 100))),
                        fluidRow(
                        column(6, selectInput(ns("est_high_flow_efficiency"), "Estimated High Flow Efficiency ", 
                                    choices = c("", high_flow_type$est_high_flow_efficiency), selected = NULL)),
                        column(6, numericInput(ns("high_flow_efficiency"), "High Flow Efficiency %", value = NA, min = 0, max = 100))), 
                        textAreaInput(ns("cet_notes"), "Notes", height = '90px'), 
                        actionButton(ns("add_cet"), "Add Capture Efficiency Test"), 
                        actionButton(ns("clear_cet"), "Clear All Fields"),
                      ), 
                      mainPanel(h4("Capture Efficiency Tests at this SMP"), 
                                h6("1) Low flow using truck water tank, flow estimated at 2-5 CFM \n"),
                                h6("2) If no hydrant is nearby to test high flow, efficiency is simply predicted"), 
                                h6("3) High flow uisng nearby hydrant, flow estimated at 20-25 CFM"),
                                DTOutput(ns("cet_table")))
             ),
             tabPanel("View Capture Efficiency Tests", value = "view_cet", 
                      titlePanel("All Capture Efficiency Tests"), 
                      DTOutput(ns("all_cet_table"))
             )
  )
}

capture_efficiency <- function(input, output, session, parent_session, poolConn, high_flow_type, con_phase){
  
  #define ns to use in modals
  ns <- session$ns
  
  rv <- reactiveValues()
  #adjust query to accurately target NULL values once back on main server
  rv$component_and_asset_query <- reactive(paste0("SELECT component_id, asset_type FROM smpid_facilityid_componentid_inlets WHERE system_id = '", input$cet_system_id, "' AND component_id != 'NULL'"))
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
  
  #get facility ID. Either use SMP footprint (for a new well) or the facility ID of the existing component
  rv$facility_id <- reactive(if(input$cet_comp_id != ""){
        odbc::dbGetQuery(poolConn, paste0(
          "SELECT facility_id from smpid_facilityid_componentid_inlets WHERE component_id = '", rv$select_component_id(), "'"))[1,1]
  }else if(input$cet_system_id != ""){
    odbc::dbGetQuery(poolConn, paste0("SELECT facility_id from smpid_facilityid_componentid_inlets WHERE component_id is NULL and system_id = '", input$cet_system_id, "' LIMIT 1"))
  }else{
    ""
  }
  )

  observe(updateSelectInput(session, "facility_id", selected = rv$facility_id()))
  
  rv$cet_table_query <- reactive(paste0("SELECT * FROM fieldwork.capture_efficiency_full WHERE system_id = '", input$cet_system_id, "'"))
  rv$cet_table_db <- reactive(dbGetQuery(poolConn, rv$cet_table_query()))
  rv$cet_table <- reactive(rv$cet_table_db() %>% mutate_at("test_date", as.character) %>% 
                             mutate_at(vars(one_of("low_flow_bypass_observed")), 
                                       funs(case_when(. == 1 ~ "Yes", 
                                                      . == 0 ~ "No"))) %>% 
                             dplyr::select(-1))
  
  #conditions need to be set up this way; changing them all to nchar, or all to length, or all to input != "" won't work
  observe(toggleState(id = "add_cet", condition = nchar(input$cet_system_id) > 0 & length(input$cet_date) > 0 & 
                        (nchar(input$cet_comp_id) >0 | nchar(input$cet_comp_id_custom) > 0) & nchar(input$con_phase) > 0))
  
  observe(toggleState(id = "cet_comp_id_custom", condition = input$cet_comp_id == ""))
  
  observe(toggleState(id = "cet_comp_id", condition = input$cet_comp_id_custom == ""))
  
  #change type from text to uid
  rv$est_high_flow_efficiency <- reactive(high_flow_type %>% dplyr::filter(est_high_flow_efficiency == input$est_high_flow_efficiency) %>% 
                                            select(est_high_flow_efficiency_lookup_uid) %>% pull())
  
  rv$phase <- reactive(con_phase %>% dplyr::filter(phase == input$con_phase) %>% 
                         select(con_phase_lookup_uid) %>% pull())
  
  output$cet_table <- renderDT(
    datatable(rv$cet_table(), 
              colnames = c('System ID', 'Test Date', 'Component ID', 'Facility ID', 'Construction Phase', 'Low Flow Bypass Observed', 
                           'Low Flow<span style="color:DodgerBlue"><sup>1</sup></span style="color:DodgerBlue"> Efficiency %', 
                           'Est. High Flow<span style="color:DodgerBlue"><sup>2</sup></span style="color:DodgerBlue"> Efficiency', 
                           'High Flow<span style="color:DodgerBlue"><sup>3</sup></span style="color:DodgerBlue"> Efficiency %', 'Asset Type', 'Notes'),
              selection = 'single', 
              style = 'bootstrap',
              class = 'table-responsive, table-hover', 
              escape = FALSE
    ))
  
  observeEvent(input$cet_table_rows_selected, {
    #reset("cet_comp_id")
    #reset("cet_comp_id_custom")
    
    #get facility id from table
    rv$fac <- (rv$cet_table_db()[input$cet_table_rows_selected, 5])
    #get component id
    comp_id_query <- paste0("select distinct component_id from smpid_facilityid_componentid_inlets where facility_id = '", rv$fac, "' 
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
    }else{
      updateSelectInput(session, "cet_comp_id", selected = "")
      updateSelectInput(session, "cet_comp_id_custom", selected = rv$cet_table_db()[input$cet_table_rows_selected, 4])
    }
    
    # updateSelectInput(session, "cet_system_id", selected = rv$cet_table_db()[input$cet_table_rows_selected, 2])
    #updateSelectInput(session, "cet_comp_id", selected = rv$cet_table_db()[input$cet_table_rows_selected, 4])
    
    updateDateInput(session, "cet_date", value = rv$cet_table_db()[input$cet_table_rows_selected, 3])
    updateSelectInput(session, "con_phase", selected = rv$cet_table_db()[input$cet_table_rows_selected, 6])
    updateSelectInput(session, "low_flow_bypass", selected = rv$cet_table_db()[input$cet_table_rows_selected, 7])
    updateNumericInput(session, "low_flow_efficiency", value = rv$cet_table_db()[input$cet_table_rows_selected, 8])
    updateSelectInput(session, "est_high_flow_efficiency", selected = rv$cet_table()[input$cet_table_rows_selected, 9])
    updateNumericInput(session, "high_flow_efficiency", value = rv$cet_table_db()[input$cet_table_rows_selected, 10])
    updateTextAreaInput(session, "cet_notes", value = rv$cet_table_db()[input$cet_table_rows_selected, 12])
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
  
  #add and edit capture efficiency records
  observeEvent(input$add_cet, {
    if(length(input$cet_table_rows_selected) == 0){
      #add to capture efficiency
      add_cet_query <- paste0("INSERT INTO fieldwork.capture_efficiency (system_id, test_date, component_id,
      facility_id, con_phase_lookup_uid, low_flow_bypass_observed,
      low_flow_efficiency_pct, est_high_flow_efficiency_lookup_uid, high_flow_efficiency_pct, notes)
    	                  VALUES ('", input$cet_system_id, "','",  input$cet_date, "',", rv$cet_comp_id(), ",'", rv$facility_id(), "','", rv$phase(), "', 
    	                        ", rv$low_flow_bypass(), ",
                              ", rv$low_flow_efficiency(), ", ", rv$est_hfe(), ", ", rv$high_flow_efficiency(), ", 
                              ", rv$cet_notes(), ")")
      
      odbc::dbGetQuery(poolConn, add_cet_query)
    }else{
      #edit capture efficiency
      edit_cet_query <- paste0("UPDATE fieldwork.capture_efficiency SET system_id = '", input$cet_system_id, "', 
                               component_id = ", rv$cet_comp_id(), ", 
                               facility_id = '",rv$facility_id(), "',
                               test_date = '", input$cet_date, "', 
                               con_phase_lookup_uid = '", rv$phase(), "',
                               low_flow_bypass_observed = ", rv$low_flow_bypass(), ", 
                               low_flow_efficiency_pct = ", rv$low_flow_efficiency(), ", 
                               est_high_flow_efficiency_lookup_uid = ", rv$est_hfe(), ", 
                               high_flow_efficiency_pct = ", rv$high_flow_efficiency(), ", 
                               notes = ", rv$cet_notes(), "
                               WHERE capture_efficiency_uid = '", rv$cet_table_db()[input$cet_table_rows_selected, 1], "'")
      
      dbGetQuery(poolConn, edit_cet_query)
    }
    rv$cet_table_db <- reactive(dbGetQuery(poolConn, rv$cet_table_query()))
    rv$all_cet_table_db <- reactive(dbGetQuery(poolConn, rv$all_query()))
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
    reset("cet_system_id")
    reset("cet_date")
    reset("phase")
    reset("low_flow_bypass")
    reset("low_flow_efficiency")
    reset("low_flow_efficiency")
    reset("est_high_flow_efficiency")
    reset("high_flow_efficiency")
    reset("cet_notes")
    removeModal()
  })
  
  # View all CETs ---
  
  rv$all_query <- reactive(paste0("SELECT * FROM fieldwork.capture_efficiency_full ORDER BY test_date DESC"))
  rv$all_cet_table_db <- reactive(dbGetQuery(poolConn, rv$all_query())) 
  rv$all_cet_table <- reactive(rv$all_cet_table_db() %>% 
                                 mutate_at("test_date", as.character) %>% 
                                 mutate_at(vars(one_of("low_flow_bypass_observed")),
                                           funs(case_when(. == 1 ~ "Yes", 
                                                          . == 0 ~ "No"))) %>% 
                                 dplyr::select(-1))
  
  output$all_cet_table <- renderDT(
    rv$all_cet_table(),
    selection = 'single', 
    style = 'bootstrap',
    class = 'table-responsive, table-hover',
    colnames = c('System ID', 'Test Date', 'Component ID', 'Facility ID', 'Construction Phase', 'Low Flow Bypass Observed',
                 'Low Flow Efficiency %', 'Est. High Flow Efficiency', 'High Flow Efficiency %', 'Asset Type', 'Notes')
  )
  
  observeEvent(input$all_cet_table_rows_selected, {
    updateSelectInput(session, "cet_system_id", selected = "")
    updateSelectInput(session, "cet_system_id", selected = rv$all_cet_table()$system_id[input$all_cet_table_rows_selected])
    updateTabsetPanel(session = parent_session, "inTabset", selected = "cet_tab")
  })
  
  observeEvent(rv$cet_table_db(), {
    if(length(input$all_cet_table_rows_selected) > 0){
      cet_row <- which(rv$cet_table_db()$capture_efficiency_uid == rv$all_cet_table_db()$capture_efficiency_uid[input$all_cet_table_rows_selected], arr.ind = TRUE)
      dataTableProxy('cet_table') %>% selectRows(cet_row)
    }
  })
}