porous_pavementUI <- function(id, label = "porous_pavement", smp_id, html_req, surface_type, con_phase){
  ns <- NS(id)
  navbarMenu("Porous Pavement", 
             tabPanel("Add/Edit Porous Pavement Test", value = "ppt_tab", 
                      titlePanel("Add Porous Pavement Test"), 
                      sidebarPanel(selectInput(ns("smp_id"), html_req("SMP ID"), choices = c("", smp_id), selected = NULL), 
                                   dateInput(ns("date"), html_req("Test Date"), value = as.Date(NA)), 
                                   selectInput(ns("surface_type"), html_req("Surface Type"), choices = c("", surface_type$surface_type), selected = NULL), 
                                   selectInput(ns("con_phase"), html_req("Construction Phase"), choices = c("", con_phase$phase), selected = NULL),
                                   textInput(ns("location"), "Test Location"), 
                                   selectInput(ns("data"), "Data in Spreadsheet", choices = c("","Yes" = "1", "No" = "0"), selected = NULL), 
                                   selectInput(ns("folder"), "Test Location Map in Site Folder", choices = c("","Yes" = "1", "No" = "0"), selected = NULL),
                                   actionButton(ns("add_ppt"), "Add Porous Pavement Test"), 
                                   actionButton(ns("clear_ppt"), "Clear All Fields")
                      ), 
                      mainPanel(h4("Porous Pavement Tests at this SMP"), 
                                DTOutput(ns("ppt_table")))
             ), 
             tabPanel("View Porous Pavement Tests", value = ns("view_ppt"), 
                      titlePanel("All Porous Pavement Tests"), 
                      DTOutput(ns("all_ppt_table"))
             )
  )
  
  
}

porous_pavement <- function(input, output, session, parent_session, surface_type, poolConn, con_phase){
  
  #define ns to use in modals
  ns <- session$ns
  
  #initialize porous pavement testing (ppt) reactiveValues
  rv <- reactiveValues()
  
  #query full porous pavement view
  rv$query <- reactive(paste0("SELECT * FROM fieldwork.porous_pavement_full WHERE smp_id = '", input$smp_id, "'"))
  
  rv$ppt_table_db <- reactive(dbGetQuery(poolConn, rv$query()))
  
  #adjust table for viewing
  rv$ppt_table <- reactive(rv$ppt_table_db() %>% mutate_at("test_date", as.character) %>% dplyr::select(2:6))
  
  
  #toggle state (enable/disable) buttons based on whether system id, test date, and type are selected (this is shinyjs)
  observe(toggleState(id = "add_ppt", condition = nchar(input$smp_id) > 0 & length(input$date) > 0 & 
                        nchar(input$surface_type) >0 & nchar(input$con_phase) > 0))
  
  #render datatable  for porous pavement
  output$ppt_table <- renderDT(
    rv$ppt_table(), 
    selection = 'single', 
    style = 'bootstrap', 
    class = 'table-responsive, table-hover', 
    colnames = c('Test Date', 'SMP ID', 'Surface Type', 'Construction Phase', 'Test Location'),
    options = list(dom = 't')
  )
  
  observeEvent(input$ppt_table_rows_selected,{ 
    
    #update to values from selected row
    updateDateInput(session, "date", value = rv$ppt_table_db()[input$ppt_table_rows_selected, 2])
    updateSelectInput(session, "surface_type", selected = rv$ppt_table_db()[input$ppt_table_rows_selected, 4])
    updateSelectInput(session, "con_phase", selected = rv$ppt_table_db()[input$ppt_table_rows_selected, 5])
    updateNumericInput(session, "location", value = rv$ppt_table_db()[input$ppt_table_rows_selected, 6])
    updateTextAreaInput(session, "ppt_summary", value = rv$ppt_table_db()[input$ppt_table_rows_selected, 9])
    
    #update metadata values
    updateSelectInput(session, "data", selected = rv$ppt_table_db()[input$ppt_table_rows_selected, 7])
    updateSelectInput(session, "folder", selected = rv$ppt_table_db()[input$ppt_table_rows_selected, 8])
    
  })
  
  #change type from text to uid
  rv$surface_type <- reactive(surface_type %>% dplyr::filter(surface_type == input$surface_type) %>% 
                                select(surface_type_lookup_uid) %>% pull())
  
  rv$phase <- reactive(con_phase %>% dplyr::filter(phase == input$con_phase) %>% 
                         select(con_phase_lookup_uid) %>% pull())
  
  #set inputs to reactive values so "NULL" can be entered
  #important to correctly place quotations
  rv$type <- reactive(if(nchar(rv$surface_type()) == 0) "NULL" else paste0("'", rv$surface_type(), "'"))
  rv$location_step <- reactive(gsub('\'', '\'\'', input$location))
  rv$test_location <- reactive(if(nchar(rv$location_step()) == 0) "NULL" else paste0("'", rv$location_step(), "'"))
  rv$data <- reactive(if(nchar(input$data) == 0 | input$data == "N/A") "NULL" else paste0("'", input$data, "'"))
  rv$folder <- reactive(if(nchar(input$folder) == 0 | input$folder == "N/A") "NULL" else paste0("'", input$folder, "'"))
  
  #add/edit button toggle
  rv$label <- reactive(if(length(input$ppt_table_rows_selected) == 0) "Add New" else "Edit Selected")
  observe(updateActionButton(session, "add_ppt", label = rv$label()))
  
  #on click
  observeEvent(input$add_ppt, {
    
    if(length(input$ppt_table_rows_selected) == 0){
      #add to porous_pavement
      add_ppt_query <- paste0("INSERT INTO fieldwork.porous_pavement (test_date, smp_id, surface_type_lookup_uid, con_phase_lookup_uid,
                        test_location)
    	                  VALUES ('", input$date, "','", input$smp_id, "',",  rv$type(), ",", rv$phase(), ",", rv$test_location(), ")") 
      
      #add to porous_pavement_metadata
      add_ppt_meta_query <- paste0("INSERT INTO fieldwork.porous_pavement_metadata (porous_pavement_uid, data_in_spreadsheet, map_in_site_folder)
                                VALUES ((SELECT MAX(porous_pavement_uid) FROM fieldwork.porous_pavement), ", rv$data(), ",", rv$folder(), ")")
      
      odbc::dbGetQuery(poolConn, add_ppt_query)
      odbc::dbGetQuery(poolConn, add_ppt_meta_query)
      
    }else{
      edit_ppt_query <- paste0(
        "UPDATE fieldwork.porous_pavement SET smp_id = '", input$smp_id, "', test_date = '", input$date, 
        "', surface_type_lookup_uid = ",  rv$type(),
        ", con_phase_lookup_uid = '", rv$phase(),
        "', test_location = ", rv$test_location(),"
        WHERE porous_pavement_uid = '", rv$ppt_table_db()[input$ppt_table_rows_selected, 1], "'")
      
      edit_ppt_meta_query <- paste0("UPDATE fieldwork.porous_pavement_metadata  SET data_in_spreadsheet = ", rv$data(), 
                                    ", map_in_site_folder = ", rv$folder(), 
                                    " WHERE porous_pavement_uid = '", rv$ppt_table_db()[input$ppt_table_rows_selected, 1], "'")
      
      dbGetQuery(poolConn, edit_ppt_query)
      dbGetQuery(poolConn, edit_ppt_meta_query)
    }
    
    #re-run query
    rv$ppt_table_db <- reactive(dbGetQuery(poolConn, rv$query()))
    
    rv$all_ppt_table_db <- reactive(dbGetQuery(poolConn, rv$all_query())) 
    
    #clear fields
    reset("date")
    reset("surface_type")
    reset("con_phase")
    reset("location")
    reset("data")
    reset("folder")
  })
  
  #clear fields on click
  observeEvent(input$clear_ppt, {
    showModal(modalDialog(title = "Clear All Fields", 
                          "Are you sure you want to clear all fields on this tab?", 
                          modalButton("No"), 
                          actionButton(ns("confirm_clear_ppt"), "Yes")))
  })
  
  observeEvent(input$confirm_clear_ppt, {
    reset("smp_id")
    reset("date")
    reset("surface_type")
    reset("con_phase")
    reset("location")
    reset("data")
    reset("folder")
    removeModal()
  })
  
  #View all Porous Pavement
  
  #query full porous pavement view
  rv$all_query <- reactive(paste0("SELECT * FROM fieldwork.porous_pavement_full ORDER BY test_date DESC"))
  
  rv$all_ppt_table_db <- reactive(dbGetQuery(poolConn, rv$all_query())) 
  
  rv$all_ppt_table <- reactive(rv$all_ppt_table_db() %>% mutate_at("test_date", as.character) %>% 
                                 mutate_at(vars(one_of("data_in_spreadsheet", "map_in_site_folder")), 
                                           funs(case_when(. == 1 ~ "Yes", 
                                                          . == 0 ~ "No"))) %>% 
                                 dplyr::select(2:8))
  
  output$all_ppt_table <- renderDT(
    rv$all_ppt_table(), 
    selection = 'single', 
    style = 'bootstrap', 
    class = 'table-responsive, table-hover', 
    colnames = c('Test Date', 'SMP ID', 'Surface Type', 'Construction Phase', 'Test Location', 'Data in Spreadsheet', 'Map in Site Folder')
  )
  
  observeEvent(input$all_ppt_table_rows_selected, {
    # do not use shinyjs::reset() - it is too slow and will go after updating to the smp_id, resulting in a cleared field
    updateSelectInput(session, "smp_id", selected = NULL)
    updateSelectInput(session, "smp_id", selected = rv$all_ppt_table()$smp_id[input$all_ppt_table_rows_selected])
    updateTabsetPanel(session = parent_session, "inTabset", selected = "ppt_tab")
  })
  
  observeEvent(rv$ppt_table_db(), {
    if(length(input$all_ppt_table_rows_selected) > 0){
      ppt_row <- which(rv$ppt_table_db()$porous_pavement_uid == rv$all_ppt_table_db()$porous_pavement_uid[input$all_ppt_table_rows_selected], arr.ind = TRUE)
      dataTableProxy('ppt_table') %>% selectRows(ppt_row)
    }
  })
  
}