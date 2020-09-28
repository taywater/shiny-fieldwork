#Porous pavement tabs
#This has a tab dropdown with three tabs, one for adding PPTs and one for viewing all PPTs and one for viewing future PPTs

porous_pavementUI <- function(id, label = "porous_pavement", smp_id, html_req, surface_type, priority, con_phase){
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
                                   conditionalPanel(condition = "input.date === null", 
                                                    ns = ns, 
                                                    selectInput(ns("priority"), "Future Test Priority", 
                                                                choices = c("", priority$field_test_priority[1:3]), selected = NULL)),
                                   conditionalPanel(condition = "input.date === null", 
                                                    ns= ns, 
                                                    actionButton(ns("future_ppt"), "Add Future Porous Pavement Test")),
                                   actionButton(ns("add_ppt"), "Add Porous Pavement Test"), 
                                   actionButton(ns("clear_ppt"), "Clear All Fields")
                      ), 
                      mainPanel(
                        conditionalPanel(condition = "input.smp_id",
                                         ns = ns, 
                                         h4(textOutput(ns("future_header"))),
                                         DTOutput(ns("future_ppt_table")),
                                         h4(textOutput(ns("header"))), 
                                         DTOutput(ns("ppt_table"))))
             ), 
             tabPanel("View Porous Pavement Tests", value = ns("view_ppt"), 
                      titlePanel("All Porous Pavement Tests"), 
                      DTOutput(ns("all_ppt_table"))
             ), 
             tabPanel("View Future Porous Pavement Tests", value = ns("view_future_ppt"), 
                      titlePanel("All Future Porous Pavement Tests"), 
                      DTOutput(ns("all_future_ppt_table")))
  )
  
}

porous_pavement <- function(input, output, session, parent_session, surface_type, poolConn, con_phase){
  
  #define ns to use in modals
  ns <- session$ns
  
  #initialize porous pavement testing (ppt) reactiveValues
  rv <- reactiveValues()
  
  #Get the Project name, combine it with SMP ID, and create a reactive header
  rv$smp_and_name_step <- reactive(odbc::dbGetQuery(poolConn, paste0("select smp_id, project_name from project_names where smp_id = '", input$smp_id, "'")))
  
  rv$smp_and_name <- reactive(paste(rv$smp_and_name_step()$smp_id[1], rv$smp_and_name_step()$project_name[1]))
  
  output$header <- renderText(
    paste("Porous Pavement Tests at", rv$smp_and_name())
  )
  
  output$future_header <- renderText(
    paste("Future Porous Pavement Tests at", rv$smp_and_name())
  )
  
  #query full porous pavement view
  rv$query <- reactive(paste0("SELECT * FROM fieldwork.porous_pavement_full WHERE smp_id = '", input$smp_id, "'"))
  
  rv$ppt_table_db <- reactive(dbGetQuery(poolConn, rv$query()))
  
  #adjust table for viewing
  rv$ppt_table <- reactive(rv$ppt_table_db() %>% mutate_at("test_date", as.character) %>% dplyr::select("test_date", "surface_type", "phase", "test_location"))
  
  
  #toggle state (enable/disable) buttons based on whether system id, test date, and type are selected (this is shinyjs)
  observe(toggleState(id = "add_ppt", condition = nchar(input$smp_id) > 0 & length(input$date) > 0 & 
                        nchar(input$surface_type) >0 & nchar(input$con_phase) > 0))
  
  #toggle state for future cet depending on whether smp_id is selected
  observe(toggleState(id = "future_ppt", condition = nchar(input$smp_id) > 0))
  
  #toggle state for metadata depending on whether a test date is included
  observe(toggleState(id = "data", condition = length(input$date) > 0))
  observe(toggleState(id = "folder", condition = length(input$date) > 0))
  
  #render datatable  for porous pavement
  output$ppt_table <- renderDT(
    rv$ppt_table(), 
    selection = 'single', 
    style = 'bootstrap', 
    class = 'table-responsive, table-hover', 
    colnames = c('Test Date', 'Surface Type', 'Construction Phase', 'Test Location')
  )
  
  #query future PPTs
  future_ppt_table_query <- reactive(paste0("SELECT * FROM fieldwork.future_porous_pavement_full 
                                            WHERE smp_id = '", input$smp_id, "' 
                                            order by field_test_priority_lookup_uid"))
  rv$future_ppt_table_db <- reactive(odbc::dbGetQuery(poolConn, future_ppt_table_query()))
  
  rv$future_ppt_table <- reactive(rv$future_ppt_table_db() %>% 
                                    dplyr::select("smp_id", "surface_type", "phase", "test_location", "field_test_priority"))
  
  rv$priority_lookup_uid_query <- reactive(paste0("select field_test_priority_lookup_uid from fieldwork.field_test_priority_lookup where field_test_priority = '", input$priority, "'"))
  rv$priority_lookup_uid_step <- reactive(dbGetQuery(poolConn, rv$priority_lookup_uid_query()))
  rv$priority_lookup_uid <- reactive(if(nchar(input$priority) == 0) "NULL" else paste0("'", rv$priority_lookup_uid_step(), "'"))
  
  output$future_ppt_table <- renderDT(
    rv$future_ppt_table(), 
    selection = 'single', 
    style = 'bootstrap', 
    class = 'table-responsive, table-hover', 
    colnames = c('SMP ID', 'Surface Type', 'Construction Phase', 'Test Location', 'Priority') 
  )
  
  observeEvent(input$future_ppt_table_rows_selected, {
    #deselect other table
    dataTableProxy('ppt_table') %>% selectRows(NULL)
   
    #update to values from selected row
    updateSelectInput(session, "surface_type", selected = rv$future_ppt_table()[input$future_ppt_table_rows_selected, 2])
    updateSelectInput(session, "con_phase", selected = rv$future_ppt_table()[input$future_ppt_table_rows_selected, 3])
    updateNumericInput(session, "location", value = rv$future_ppt_table()[input$future_ppt_table_rows_selected, 4])
    updateSelectInput(session, "priority", selected = rv$future_ppt_table()[input$future_ppt_table_rows_selected, 5])
    
    reset("date")
    reset("data")
    reset("folder")
    
  })
  
  observeEvent(input$ppt_table_rows_selected,{ 
    
    #deselect other table
    dataTableProxy('future_ppt_table') %>% selectRows(NULL)
    
    #update to values from selected row
    updateDateInput(session, "date", value = rv$ppt_table_db()[input$ppt_table_rows_selected, 2])
    updateSelectInput(session, "surface_type", selected = rv$ppt_table_db()[input$ppt_table_rows_selected, 4])
    updateSelectInput(session, "con_phase", selected = rv$ppt_table_db()[input$ppt_table_rows_selected, 5])
    updateNumericInput(session, "location", value = rv$ppt_table_db()[input$ppt_table_rows_selected, 6])
    
    #update metadata values
    updateSelectInput(session, "data", selected = rv$ppt_table_db()[input$ppt_table_rows_selected, 7])
    updateSelectInput(session, "folder", selected = rv$ppt_table_db()[input$ppt_table_rows_selected, 8])
    
  })
  
  #change type from text to uid
  rv$surface_type <- reactive(surface_type %>% dplyr::filter(surface_type == input$surface_type) %>% 
                                select(surface_type_lookup_uid) %>% pull())
  
  rv$phase <- reactive(con_phase %>% dplyr::filter(phase == input$con_phase) %>% 
                         select(con_phase_lookup_uid) %>% pull())
  
  rv$phase_null <- reactive(if(nchar(input$con_phase) == 0) "NULL" else paste0("'", rv$phase(), "'"))
  
  #set inputs to reactive values so "NULL" can be entered
  #important to correctly place quotations
  rv$type <- reactive(if(length(rv$surface_type()) == 0) "NULL" else paste0("'", rv$surface_type(), "'"))
  rv$location_step <- reactive(gsub('\'', '\'\'', input$location))
  rv$test_location <- reactive(if(nchar(rv$location_step()) == 0) "NULL" else paste0("'", rv$location_step(), "'"))
  rv$data <- reactive(if(nchar(input$data) == 0 | input$data == "N/A") "NULL" else paste0("'", input$data, "'"))
  rv$folder <- reactive(if(nchar(input$folder) == 0 | input$folder == "N/A") "NULL" else paste0("'", input$folder, "'"))
  
  #add/edit button toggle
  rv$label <- reactive(if(length(input$ppt_table_rows_selected) == 0) "Add New" else "Edit Selected")
  observe(updateActionButton(session, "add_ppt", label = rv$label()))
  
  rv$future_label <- reactive(if(length(input$future_ppt_table_rows_selected) == 0) "Add Future Porous Pavement Test" else "Edit Selected Future PPT")
  observe(updateActionButton(session, "future_ppt", label = rv$future_label()))
  
  #on click 
  observeEvent(input$future_ppt, {
    if(length(input$future_ppt_table_rows_selected) == 0){
      #add to future_porous_pavement
      add_future_ppt_query <- paste0("INSERT INTO fieldwork.future_porous_pavement (smp_id, surface_type_lookup_uid, 
                                      con_phase_lookup_uid, test_location, field_test_priority_lookup_uid)
                                     VALUES ('", input$smp_id, "', ", rv$type(), ", ", rv$phase_null(), ", ", rv$test_location(), ", ", 
                                     rv$priority_lookup_uid(), ")")
      
      odbc::dbGetQuery(poolConn, add_future_ppt_query)
    }else{
      edit_future_ppt_query <- paste0("UPDATE fieldwork.future_porous_pavement SET smp_id = '", input$smp_id, "', 
                                       surface_type_lookup_uid = ", rv$type(),
                                      ", con_phase_lookup_uid = ", rv$phase_null(),
                                      ", test_location = ", rv$test_location(),
                                      ", field_test_priority_lookup_uid = ", rv$priority_lookup_uid(), "
        WHERE future_porous_pavement_uid = '", rv$future_ppt_table_db()[input$future_ppt_table_rows_selected, 1], "'")
      
      odbc::dbGetQuery(poolConn, edit_future_ppt_query)
    }
    
    rv$future_ppt_table_db <- reactive(odbc::dbGetQuery(poolConn, future_ppt_table_query()))
    rv$all_future_ppt_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$all_future_ppt_query))
    
    reset("date")
    reset("surface_type")
    reset("con_phase")
    reset("location")
    reset("data")
    reset("priority")
    reset("folder")
  })
  
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
    
    if(length(input$future_ppt_table_rows_selected) > 0){
      odbc::dbGetQuery(poolConn, paste0("DELETE FROM fieldwork.future_porous_pavement 
                                        WHERE future_porous_pavement_uid = '", rv$future_ppt_table_db()[input$future_ppt_table_rows_selected, 1], "'"))
    }
    
    #re-run query
    rv$ppt_table_db <- reactive(dbGetQuery(poolConn, rv$query()))
    
    rv$all_ppt_table_db <- reactive(dbGetQuery(poolConn, rv$all_query())) 
    rv$future_ppt_table_db <- reactive(odbc::dbGetQuery(poolConn, future_ppt_table_query()))
    rv$all_future_ppt_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$all_future_ppt_query))
    
    #clear fields
    reset("date")
    reset("surface_type")
    reset("con_phase")
    reset("location")
    reset("data")
    reset("priority")
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
    reset("priority")
    removeModal()
  })
  
  #View all Porous Pavement ------
  
  #query full porous pavement view
  rv$all_query <- reactive(paste0("SELECT * FROM fieldwork.porous_pavement_full ORDER BY test_date DESC"))
  
  rv$all_ppt_table_db <- reactive(dbGetQuery(poolConn, rv$all_query())) 
  
  rv$all_ppt_table <- reactive(rv$all_ppt_table_db() %>% mutate_at("test_date", as.character) %>% 
                                 mutate_at(vars(one_of("data_in_spreadsheet", "map_in_site_folder")), 
                                           funs(case_when(. == 1 ~ "Yes", 
                                                          . == 0 ~ "No"))) %>% 
                                 dplyr::select("test_date", "smp_id", "project_name", "surface_type", "phase", "test_location", "data_in_spreadsheet", "map_in_site_folder"))
  
  output$all_ppt_table <- renderDT(
    rv$all_ppt_table(), 
    selection = 'single', 
    style = 'bootstrap', 
    class = 'table-responsive, table-hover', 
    colnames = c('Test Date', 'SMP ID', 'Project Name', 'Surface Type', 'Construction Phase', 'Test Location', 'Data in Spreadsheet', 'Map in Site Folder'), 
    rownames = FALSE
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
  
  
  #View all Future Porous Pavement ----
  rv$all_future_ppt_query <- "SELECT * FROM fieldwork.future_porous_pavement_full order by field_test_priority_lookup_uid"
  rv$all_future_ppt_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$all_future_ppt_query))
  rv$all_future_ppt_table <- reactive(rv$all_future_ppt_table_db() %>% 
                                        dplyr::select("smp_id", "project_name", "surface_type", "phase", "test_location", "field_test_priority"))
  
  output$all_future_ppt_table <- renderDT(
    rv$all_future_ppt_table(), 
    selection = 'single', 
    style = 'bootstrap', 
    class = 'table-responsive, table-hover', 
    colnames = c('SMP ID', 'Project Name',  'Surface Type', 'Construction Phase', 'Test Location', 'Priority'), 
    rownames = FALSE
  )
  
  #update smp id and change tabs
  observeEvent(input$all_future_ppt_table_rows_selected, {
    updateSelectInput(session, "smp_id", selected = "")
    updateSelectInput(session, "smp_id", selected = rv$all_future_ppt_table()$smp_id[input$all_future_ppt_table_rows_selected])
    updateTabsetPanel(session = parent_session, "inTabset", selected = "ppt_tab")
  })
  
  #then once the smp id is updated, select a test from a table
  #this is broken into two steps so they happen in order
  observeEvent(rv$future_ppt_table_db(), {
    if(length(input$all_future_ppt_table_rows_selected)){
      future_ppt_row <- which(rv$future_ppt_table_db()$future_porous_pavement_uid == rv$all_future_ppt_table_db()$future_porous_pavement_uid[input$all_future_ppt_table_rows_selected], arr.ind = TRUE)
      dataTableProxy('future_ppt_table') %>% selectRows(future_ppt_row)
    }
  })
  
  
}