SRTUI <- function(id, label = "srt", sys_id, srt_types, con_phase, html_req){
  ns <- NS(id)
  navbarMenu("SRT", 
             tabPanel("Add/Edit SRT", value = "srt_tab", 
                      titlePanel("Add SRT"), 
                      fluidRow(
                        column(width = 5,
                               #split layout with left and right
                               sidebarPanel(width = 12, 
                                              selectInput(ns("system_id"), html_req("System ID"), choices = c("", sys_id), selected = NULL), 
                                            splitLayout(
                                              dateInput(ns("srt_date"), html_req("Test Date"), value = as.Date(NA)), 
                                              selectInput(ns("flow_data_rec"), "Flow Data Recorded", choices = c("","Yes" = "1", "No" = "0"), selected = NULL)),
                                            splitLayout(
                                              selectInput(ns("con_phase"), html_req("Construction Phase"), choices = c("", con_phase$phase), selected = NULL),
                                              selectInput(ns("water_level_rec"), "Water Level Recorded", 
                                                          choices = c("","Yes" = "1", "No" = "0"), selected = NULL)),
                                            splitLayout(
                                              selectInput(ns("srt_type"), html_req("SRT Type"), choices = c("", srt_types$type), selected = NULL),
                                              selectInput(ns("photos_uploaded"), "Photos Uploaded", choices = c("","Yes" = "1", "No" = "0"), selected = NULL)), 
                                            splitLayout(
                                              numericInput(ns("test_volume"), "Test Volume (cf)",  value = NA, min = 0), 
                                              dateInput(ns("sensor_collect_date"), "Sensor Collection Date", value = as.Date(NA))),
                                            splitLayout(
                                              numericInput(ns("dcia"), "Impervious Drainage Area (sf)", value = NA),
                                              selectInput(ns("qaqc_complete"), "QA/QC Complete", choices = c("","Yes" = "1", "No" = "0"), selected = NULL)),
                                            splitLayout(
                                              disabled(numericInput(ns("storm_size"), "Equivalent Storm Size (in)",  value = NA, min = 0)), 
                                              dateInput(ns("srt_summary_date"), "SRT Summary Report Sent", value = as.Date(NA))),
                                            textAreaInput(ns("srt_summary"), "Results Summary", height = '85px'), 
                                            actionButton(ns("add_srt"), "Add SRT"),
                                            actionButton(ns("clear_srt"), "Clear All Fields"), 
                                            tags$head(tags$style(HTML("
      .shiny-split-layout > div {
        overflow: visible;
      }
    ")))
                               )
                        ),
                        column(width = 7,
                               h4("SRTs at this System"), 
                               DTOutput(ns("srt_table"))
                        )
                      )
             ),
             tabPanel("View SRTs", value = "view_SRT", 
                      titlePanel("All Simulated Runoff Tests"),
                      reactableOutput(ns("all_srt_table"))
             )
  )
}

SRT <- function(input, output, session, parent_session, poolConn, srt_types, con_phase){
  
  #define ns to use in modals
  ns <- session$ns
  
  #use reactive values to read in table, and see which wells already exist at the SMP
  rv <- reactiveValues()
  # srt_table_query <- reactive(paste0("SELECT srt.srt_uid, srt.system_id, srt.srt_date, srt.srt_type_lookup_uid, srt.srt_volume_ft3, srt.dcia_ft2,
  #                                    srt.srt_stormsize_in, srt.srt_summary, md.srt_metadata_uid, md.flow_data_recorded, md.water_level_recorded, 
  #                                    md.photos_uploaded, md.sensor_collection_date, md.qaqc_complete, md.srt_summary_date FROM fieldwork.srt srt LEFT JOIN fieldwork.srt_metadata md ON srt.srt_uid = md.srt_uid WHERE system_id = '", input$system_id, "'"))

  srt_table_query <- reactive(paste0("SELECT * FROM fieldwork.srt_full WHERE system_id = '", input$system_id, "'"))
  rv$srt_table_db <- reactive(odbc::dbGetQuery(poolConn, srt_table_query()))
  
  rv$srt_table <- reactive(rv$srt_table_db() %>% 
                             #left_join(srt_types, by = "srt_type_lookup_uid") %>%
                             mutate("test_date" = as.character(test_date), 
                                    "srt_stormsize_in" = round(srt_stormsize_in, 2)) %>% 
                             dplyr::select("srt_uid", "system_id", "test_date", "phase", "type", "srt_volume_ft3", "dcia_ft2", "srt_stormsize_in", "srt_summary"))
  
  rv$srt_metadata <- reactive(rv$srt_table_db() %>% mutate("srt_summary_date" = as.character(srt_summary_date)))
  
  rv$type <- reactive(srt_types %>% dplyr::filter(type == input$srt_type) %>% 
                        select(srt_type_lookup_uid) %>% pull())
  
  rv$phase <- reactive(con_phase %>% dplyr::filter(phase == input$con_phase) %>% 
                         select(con_phase_lookup_uid) %>% pull())
  
  #toggle state (enable/disable) buttons based on whether system id, test date, and srt type are selected (this is shinyjs)
  observe(toggleState(id = "add_srt", condition = nchar(input$system_id) > 0 & length(input$srt_date) > 0 &
                        nchar(input$srt_type) >0 & nchar(input$con_phase) > 0))
  
  #update Impervous Drainage Area
  srt_dcia_query <- reactive(paste0("SELECT sys_impervda_ft2 FROM public.greenit_systembestdatacache WHERE system_id = '", input$system_id, "'"))
  rv$dcia_x <- reactive(odbc::dbGetQuery(poolConn, srt_dcia_query()) %>% dplyr::pull())
  rv$dcia <- reactive(as.numeric(rv$dcia_x()))
  observe(updateNumericInput(session, "dcia", value = rv$dcia()))
  
  #update Equivalent Storm Size 
  #DCIA (ft2) * 1/12 (ft/in)
  rv$one_inch_storm_vol_cf <- reactive(rv$dcia()*1/12)
  rv$eq_storm_size_in <- reactive(input$test_volume/rv$one_inch_storm_vol_cf())
  observe(updateNumericInput(session, "storm_size", value = rv$eq_storm_size_in()))
  
  output$srt_table <- renderDT(
    rv$srt_table(), 
    selection = 'single',
    style = 'bootstrap', 
    class = 'table-responsive, table-hover', 
    colnames = c('SRT UID', 'System ID', 'Test Date', 'Phase', 'Type', 'Volume (cf)', 'DCIA (sf)', 'Storm Size (in)', 'Results Summary'), 
    options = list(dom = 't')
  )
  
  observeEvent(input$srt_table_rows_selected,{ 
    #rv_ow$fac <- (rv$srt_table()[input$srt_table_rows_selected, 4])
    updateDateInput(session, "srt_date", value = rv$srt_table()[input$srt_table_rows_selected, 3])
    
    #update to values from selected row
    updateSelectInput(session, "con_phase", selected = rv$srt_table()[input$srt_table_rows_selected, 4])
    updateSelectInput(session, "srt_type", selected = rv$srt_table()[input$srt_table_rows_selected, 5])
    updateNumericInput(session, "test_volume", value = rv$srt_table()[input$srt_table_rows_selected, 6])
    #updateNumericInput(session, "storm_size", value = rv$srt_table()[input$srt_table_rows_selected, 7])
    updateTextAreaInput(session, "srt_summary", value = rv$srt_table()[input$srt_table_rows_selected, 9])
    
    #update metadata values
    updateSelectInput(session, "flow_data_rec", selected = rv$srt_metadata()[input$srt_table_rows_selected, 9])
    updateSelectInput(session, "water_level_rec", selected = rv$srt_metadata()[input$srt_table_rows_selected, 10])
    updateSelectInput(session, "photos_uploaded", selected = rv$srt_metadata()[input$srt_table_rows_selected, 11])
    updateTextInput(session, "sensor_collect_date", value = rv$srt_metadata()[input$srt_table_rows_selected, 12])
    updateSelectInput(session, "qaqc_complete", selected = rv$srt_metadata()[input$srt_table_rows_selected, 13])
    updateDateInput(session, "srt_summary_date", value = rv$srt_metadata()[input$srt_table_rows_selected, 14])
    
  })
  
  rv$label <- reactive(if(length(input$srt_table_rows_selected) == 0) "Add New" else "Edit Selected")
  observe(updateActionButton(session, "add_srt", label = rv$label()))
  
  #set inputs to reactive values so "NULL" can be entered
  #important to correctly place quotations
  rv$test_volume <- reactive(if(is.na(input$test_volume)) "NULL" else paste0("'", input$test_volume, "'"))
  rv$dcia_write <- reactive(if(is.na(input$dcia)) "NULL" else paste0("'", input$dcia, "'"))
  rv$storm_size <- reactive(if(is.na(input$storm_size)) "NULL" else paste0("'", input$storm_size, "'"))
  rv$srt_summary_step <- reactive(gsub('\'', '\'\'', input$srt_summary))
  rv$srt_summary <- reactive(if(nchar(rv$srt_summary_step()) == 0) "NULL" else paste0("'", rv$srt_summary_step(), "'"))
  
  rv$flow_data_rec <- reactive(if(nchar(input$flow_data_rec) == 0 | input$flow_data_rec == "N/A") "NULL" else paste0("'", input$flow_data_rec, "'"))
  rv$water_level_rec <- reactive(if(nchar(input$water_level_rec) == 0 | input$water_level_rec == "N/A") "NULL" else paste0("'", input$water_level_rec, "'"))
  rv$photos_uploaded <- reactive(if(nchar(input$photos_uploaded) == 0 | input$photos_uploaded == "N/A") "NULL" else paste0("'", input$photos_uploaded, "'"))
  rv$sensor_collect_date <- reactive(if(length(input$sensor_collect_date) == 0) "NULL" else paste0("'", input$sensor_collect_date, "'"))
  rv$qaqc_complete <- reactive(if(nchar(input$qaqc_complete) == 0 | input$qaqc_complete == "N/A") "NULL" else paste0("'", input$qaqc_complete, "'"))
  rv$srt_summary_date <- reactive(if(length(input$srt_summary_date) == 0) "NULL" else paste0("'", input$srt_summary_date, "'"))
  
  #assure that the summary date does not precede the srt date
  observe(updateDateInput(session, "srt_summary_date", min = input$srt_date))
  observe(updateDateInput(session, "sensor_collect_date", min = input$srt_date))
  
  #when button is clicked
  #add to srt table
  #then add to the srt_metadata table
  #use the MAX(srt_uid) from srt table to get the SRT UID of the most recent addition to the table (calculated by SERIAL), which is the current addition
  observeEvent(input$add_srt, {
    if(length(input$srt_table_rows_selected) == 0){
      add_srt_query <- paste0("INSERT INTO fieldwork.srt (system_id, test_date, con_phase_lookup_uid, srt_type_lookup_uid, 
                      srt_volume_ft3, dcia_ft2, srt_stormsize_in, srt_summary) 
  	                  VALUES ('", input$system_id, "','", input$srt_date, "','", rv$phase(), "', ",  rv$type(), ",", rv$test_volume(), ",", 
                              rv$dcia_write(), ", ", rv$storm_size(), ",", rv$srt_summary(), ")")
      
      add_srt_meta_query <- paste0("INSERT INTO fieldwork.srt_metadata (srt_uid, flow_data_recorded, water_level_recorded, photos_uploaded, 
                              sensor_collection_date, qaqc_complete, srt_summary_date)
                              VALUES ((SELECT MAX(srt_uid) FROM fieldwork.srt), ", rv$flow_data_rec(), ",", rv$water_level_rec(), ",",  
                                   rv$photos_uploaded(), ",", rv$sensor_collect_date(), ",", rv$qaqc_complete(), ",", rv$srt_summary_date(), ")")
      
      odbc::dbGetQuery(poolConn, add_srt_query)
      odbc::dbGetQuery(poolConn, add_srt_meta_query)
      #else update srt table
    }else{
      edit_srt_query <- paste0(
        "UPDATE fieldwork.srt SET system_id = '", input$system_id, "', test_date = '", input$srt_date, 
        "', con_phase_lookup_uid = '", rv$phase(),
        "', srt_type_lookup_uid = '",  rv$type(),
        "', srt_volume_ft3 = ", rv$test_volume(),
        ", dcia_ft2 = " , rv$dcia_write(),
        ", srt_stormsize_in = ", rv$storm_size(), 
        ", srt_summary = ", rv$srt_summary(), "
        WHERE srt_uid = '", rv$srt_table()[input$srt_table_rows_selected, 1], "'")
      
      edit_srt_meta_query <- paste0("UPDATE fieldwork.srt_metadata  SET flow_data_recorded = ", rv$flow_data_rec(), 
                                    ", water_level_recorded = ", rv$water_level_rec(), 
                                    ", photos_uploaded = ", rv$photos_uploaded(), 
                                    ", sensor_collection_date = ", rv$sensor_collect_date(),
                                    ", qaqc_complete = ", rv$qaqc_complete(),
                                    ", srt_summary_date = ", rv$srt_summary_date(), 
                                    " WHERE srt_uid = '", rv$srt_table()[input$srt_table_rows_selected, 1], "'")
      
      dbGetQuery(poolConn, edit_srt_query)
      dbGetQuery(poolConn, edit_srt_meta_query)
    }
    #update ow_table with new srt
    rv$srt_table_db <- reactive(odbc::dbGetQuery(poolConn, srt_table_query()))
    
    #update srt view with new/edited srt
    rv$all_srt_table_db <- reactive(dbGetQuery(poolConn, all_srt_table_query))
    
    reset("srt_date")
    reset("con_phase")
    reset("srt_type")
    reset("test_volume")
    reset("storm_size")
    reset("srt_summary")
    reset("flow_data_rec")
    reset("water_level_rec")
    reset("photos_uploaded")
    reset("sensor_collect_date")
    reset("qaqc_complete")
    reset("srt_summary_date")
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
    reset("sensor_collect_date")
    reset("qaqc_complete")
    reset("srt_summary_date")
    removeModal()
  })
  
  #query srt table
  all_srt_table_query <- "SELECT * FROM fieldwork.srt_full ORDER BY srt_uid DESC"
  rv$all_srt_table_db <- reactive(dbGetQuery(poolConn, all_srt_table_query))
  
  rv$all_srt_table <- reactive(rv$all_srt_table_db() %>% 
                                 mutate_at(c("test_date", "srt_summary_date", "sensor_collection_date"), as.character) %>% 
                                 mutate("srt_stormsize_in" = round(srt_stormsize_in, 2)) %>% 
                                 mutate_at(vars(one_of("flow_data_recorded", "water_level_recorded", "photos_uploaded", "qaqc_complete")), 
                                           funs(case_when(. == 1 ~ "Yes", 
                                                          . == 0 ~ "No"))))
  
  output$all_srt_table <- renderReactable(
    reactable(rv$all_srt_table()[, 1:15], 
              columns = list(
                srt_uid = colDef(name = "SRT UID"),
                system_id  = colDef(name = "System ID"),
                test_date  = colDef(name = "Test Date"),
                phase = colDef(name = "Phase"),
                type = colDef(name = "Type"),
                srt_volume_ft3  = colDef(name = "Volume (cf)"),
                dcia_ft2  = colDef(name = "DCIA (sf)"),
                srt_stormsize_in = colDef(name = "Storm Size (in)"),
                flow_data_recorded = colDef(name = "Flow Data Recorded"),
                water_level_recorded = colDef(name = "Water Level Recorded"),
                photos_uploaded = colDef(name = "Photos Uploaded"),
                sensor_collection_date  = colDef(name = "Sensor Collection Date"),
                qaqc_complete = colDef(name = "QA/QC Complete"),
                srt_summary_date = colDef(name = "Summary Date"),
                turnaround_days = colDef(name = "Turnaround (days)")
              ),
              fullWidth = FALSE,
              selection = "single",
              onClick = "select",
              selectionId = ns("srt_selected"),
              #searchable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 25, 50),
              defaultPageSize = 10,
              height = 750,
              details = function(index){
                nest <- rv$all_srt_table()[rv$all_srt_table()$srt_uid == rv$all_srt_table()$srt_uid[index], ][16]
                htmltools::div(style = "padding:16px", 
                               reactable(nest, 
                                         columns = list(srt_summary = colDef(name = "Results Summary")))
                )
              })
  )
  
  #this isn't working on laptop, could be reactable version?
  observeEvent(input$srt_selected, {
    updateSelectInput(session, "system_id", selected = "")
    updateSelectInput(session, "system_id", selected = rv$all_srt_table()$system_id[input$srt_selected])
    updateTabsetPanel(session = parent_session, "inTabset", selected = "srt_tab")
  })
  
  
  observeEvent(rv$srt_table_db(), {
    if(length(input$srt_selected) > 0){
      srt_row <- which(rv$srt_table_db()$srt_uid == rv$all_srt_table()$srt_uid[input$srt_selected], arr.ind = TRUE)
      dataTableProxy('srt_table') %>% selectRows(srt_row)
    }
  })
  
}