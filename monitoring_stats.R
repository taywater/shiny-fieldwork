#Monitoring Stats
#Exports a csv for each table
#use reactive variables to create a new query for each combination

m_statsUI <- function(id, label = "stats", current_fy, years){
  ns <- NS(id)
  tabPanel("Stats",
             titlePanel("Monitoring Stats"),
              sidebarPanel(
                selectInput(ns("date_range"), "Date Range", choices = c("To-Date", "Select Range")),
                conditionalPanel(condition = "input.date_range == 'Select Range'", 
                                 ns = ns, 
                        fluidRow(column(6,
                        selectInput(ns("start_fy"), "Start FY", choices = years)),
                        column(6,selectInput(ns("start_quarter"), "Start Quarter", choices = c("Q1" = "1/1", "Q2" = "4/1", "Q3" = "7/1", "Q4" = "10/1")))),
                        fluidRow(column(6,
                          selectInput(ns("end_fy"), "End FY", choices = years)),
                          column(6,selectInput(ns("end_quarter"), "End Quarter", choices = c("Q1" = "3/31", "Q2" = "6/30", "Q3" = "9/30", "Q4" = "12/31"))))
                        ), 
                selectInput(ns("phase"), "Construction Phase", choices = c("Construction", "Post-Construction"), selected = "Post-Construction"),
                checkboxInput(ns("cet_checkbox"), "Capture Efficiency Test Options"), 
                #show options for capture efficiency if the box is checked
                conditionalPanel(condition = "input.cet_checkbox", 
                                 ns = ns, 
                                 selectInput(ns("inlet_type"), "Inlets", choices = c("All", "Inlets", "Curbcuts")), 
                                 selectInput(ns("unique_inlets"), "Unique Inlets", choices = c("Include All Tests", "Most Recent for Each Inlet"))),
                fluidRow(column(8,
                        actionButton(ns("table_button"), "Generate System and SMP Stats")),
                        column(4, 
                        shinyjs::disabled(downloadButton(ns("download_table"), "Download")))),
                fluidRow(column(8,
                          actionButton(ns("postcon_button"), "Generate Post-Construction Stats")),
                         column(4, 
                                disabled(downloadButton(ns("download_postcon"), "Download")))),
                #show options for capture efficiency if the box is checked
                conditionalPanel(condition = "input.cet_checkbox", 
                                 ns = ns, 
                                 fluidRow(column(8, 
                                          actionButton(ns("cet_button"), "Generate Capture Efficiency Stats")), 
                                          column(4,
                                          disabled(downloadButton(ns("download_cet"), "Download")))))
                
              ), 
           mainPanel(
             h4(textOutput(ns("table_name"))), 
                DTOutput(ns("table")), 
             h4(textOutput(ns("postcon"))),
                DTOutput(ns("postcon_table")),
             h4(textOutput(ns("cet_name"))), 
                DTOutput(ns("cet_table"))
             )
           
                           
           
  )
}

m_stats <- function(input, output, session, parent_session, current_fy, poolConn){
  
  #define ns to use in modals
  ns <- session$ns
  
  observe(updateSelectInput(session, "end_fy", choices = current_fy:input$start_fy))
  observe(updateActionButton(session, "postcon_button", label = paste("Generate", input$phase, "Stats")))
  
  rv <- reactiveValues()
  
  sf <- lubridate::stamp("March 1, 1999")
  
  rv$start_date <- reactive(lubridate::mdy(paste0(input$start_quarter, "/", input$start_fy))%m-% months(6))
  rv$end_date <- reactive(lubridate::mdy(paste0(input$end_quarter, "/", input$end_fy))%m-% months(6))
  
  #system and smp
  observeEvent(input$table_button, {
    enable("download_table")
    if(input$date_range == "To-Date"){
      rv$table_name <- "CWL To Date"
      output$table_name <- renderText(rv$table_name)
      rv$cwl_to_date_table <- bind_rows(
        rv$public_systems_monitored_to_date(),
        rv$private_systems_monitored_to_date()
      )
      output$table <- renderDT(
        rv$cwl_to_date_table,
        options = list(dom = 't')
      )
    }else{
      rv$table_name <- paste("CWL", sf(rv$start_date()), "to", sf(rv$end_date()))
        output$table_name <- renderText(rv$table_name)
        rv$cwl_table <- bind_rows(
          rv$public_systems_monitored(),
          rv$public_smps_monitored(),
          rv$private_systems_monitored(),
          rv$long_term_systems_monitored(),
          rv$short_term_systems_monitored(),
          rv$new_systems_monitored(),
          rv$new_smps_monitored(),
          rv$hobos_deployed()
        )
      output$table <- renderDT(
        rv$cwl_table,
        options = list(dom = 't')
      )
       }
    }
  )
  
  rv$select_range_tests_title_text <- reactive(if(input$date_range == "Select Range"){
    paste(sf(rv$start_date()), "to", sf(rv$end_date()))
  }else{
    paste("To Date")
  })
  
  #post con
  observeEvent(input$postcon_button, {
    enable("download_postcon")
    rv$postcon_name <- paste(input$phase, "Tests", rv$select_range_tests_title_text())
      output$postcon <- renderText(rv$postcon_name)
      rv$postcon_table <- bind_rows(
        rv$pre_inspection(), 
        rv$performance_srts(),
        rv$cctv_dye_test(), 
        rv$systems_tested_srt(),
        rv$systems_tested_ppt(), 
        rv$systems_tested_cet()
      )
      output$postcon_table <- renderDT(
        rv$postcon_table, 
        options = list(dom = 't')
      )
  })

  rv$unique_inlet_title_text <- reactive(if(input$unique_inlets == "Include All Tests"){ 
    paste("All")
  }else if(input$unique_inlets == "Most Recent for Each Inlet"){
    paste("Unique")
  })
  
  #cet
  observeEvent(input$cet_button, {
    #print("starting_cet")
    enable("download_cet")
    rv$cet_name <- paste(rv$unique_inlet_title_text(), input$phase, input$inlet_type, "Capture Efficiency Stats", rv$select_range_tests_title_text())
    output$cet_name <- renderText(rv$cet_name)
    rv$cet_table <- bind_rows(
      rv$cet_lf(),
      rv$cet_hf(),
      rv$cet_no_lf_bypass(),
      rv$cet_no_hf_bypass(),
      rv$cet_10pct_lf_bypass(),
      rv$cet_10pct_hf_bypass()
    )
    output$cet_table <- renderDT(
      rv$cet_table,
      options = list(dom = 't')
    )
  } )
  
  output$download_table <- downloadHandler(
    filename = function(){
      paste(rv$table_name, "_", Sys.Date(), ".csv", sep = "")
    }, 
    content = function(file){
      write.csv(rv$cwl_table, file, row.names = FALSE)
    }
  )
  
  output$download_postcon <- downloadHandler(
    filename = function(){
      paste(rv$postcon_name, "_", Sys.Date(), ".csv", sep = "")
    }, 
    content = function(file){
      write.csv(rv$postcon_table, file, row.names = FALSE)
    }
  )
  
  output$download_cet <- downloadHandler(
    filename = function(){
      paste(rv$cet_name, "_", Sys.Date(), ".csv", sep = "")
    }, 
    content = function(file){
      write.csv(rv$cet_table, file, row.names = FALSE)
    }
  )
  
# System ----------------------------------------------------------
  
  #systems
  # No. of public systems monitored
  rv$public_systems_monitored_q <- reactive(paste0("SELECT COUNT(DISTINCT smp_to_system(ow.smp_id)) FROM ow_leveldata_raw lvl 
                                                LEFT JOIN fieldwork.ow ow on lvl.ow_uid = ow.ow_uid
                                                LEFT JOIN fieldwork.ow_ownership own on lvl.ow_uid = own.ow_uid
                                                WHERE dtime_est >= '", rv$start_date(), "'
                                                AND dtime_est <= '", rv$end_date(), "'
                                                AND own.public = TRUE"))
  
  rv$public_systems_monitored_value <- reactive(dbGetQuery(poolConn, rv$public_systems_monitored_q()))
  rv$public_systems_monitored <- reactive(data.frame(Metric = as.character("Public Systems Monitored"), Count = rv$public_systems_monitored_value()))
  
  #No. of longterm systems monitored
  rv$long_term_systems_monitored_q <- reactive(paste0("SELECT COUNT(DISTINCT smp_to_system(ow.smp_id)) FROM ow_leveldata_raw lvl 
                                                LEFT JOIN fieldwork.ow ow on lvl.ow_uid = ow.ow_uid
                                                LEFT JOIN fieldwork.ow_ownership own on lvl.ow_uid = own.ow_uid
                                                LEFT JOIN fieldwork.deployment de on lvl.ow_uid = de.ow_uid
                                                LEFT JOIN fieldwork.long_term_lookup lt on de.long_term_lookup_uid = lt.long_term_lookup_uid
                                                WHERE dtime_est >= '", rv$start_date(), "'
                                                AND dtime_est <= '", rv$end_date(), "'
                                                AND lt.type = 'Long'"))
  
  rv$long_term_systems_monitored_value <- reactive(dbGetQuery(poolConn, rv$long_term_systems_monitored_q()))
  rv$long_term_systems_monitored <- reactive(data.frame(Metric = as.character("Long Term Systems Monitored"), Count = rv$long_term_systems_monitored_value()))
  
  #No. of short term systems monitored
  rv$short_term_systems_monitored_q <- reactive(paste0("SELECT COUNT(DISTINCT smp_to_system(ow.smp_id)) FROM ow_leveldata_raw lvl 
                                                LEFT JOIN fieldwork.ow ow on lvl.ow_uid = ow.ow_uid
                                                LEFT JOIN fieldwork.ow_ownership own on lvl.ow_uid = own.ow_uid
                                                LEFT JOIN fieldwork.deployment de on lvl.ow_uid = de.ow_uid
                                                LEFT JOIN fieldwork.long_term_lookup lt on de.long_term_lookup_uid = lt.long_term_lookup_uid
                                                WHERE dtime_est >= '", rv$start_date(), "'
                                                AND dtime_est <= '", rv$end_date(), "'
                                                AND lt.type = 'Short'"))
  
  rv$short_term_systems_monitored_value <- reactive(dbGetQuery(poolConn, rv$short_term_systems_monitored_q()))
  rv$short_term_systems_monitored <- reactive(data.frame(Metric = as.character("Short Term Systems Monitored"), Count = rv$short_term_systems_monitored_value()))
  
  #No. of new systems monitored
  rv$new_systems_monitored_q <- reactive(paste0("SELECT count(distinct smp_to_system(dada.smp_id)) FROM 
                                              (SELECT smp_id FROM ow_leveldata_raw lvl 
                                                LEFT JOIN fieldwork.ow ow on lvl.ow_uid = ow.ow_uid
                                                GROUP BY ow.smp_id 
                                                HAVING min(lvl.dtime_est) >= '", rv$start_date(), "'
                                                AND min(lvl.dtime_est) <= '", rv$end_date(), "') dada"))
  
  rv$new_systems_monitored_value <- reactive(dbGetQuery(poolConn, rv$new_systems_monitored_q()))
  rv$new_systems_monitored <- reactive(data.frame(Metric = as.character("New Systems Monitored"), Count = rv$new_systems_monitored_value()))
  
  #No. of public systems monitored to date
  rv$public_systems_monitored_to_date_q <- reactive(paste0("SELECT COUNT(DISTINCT smp_to_system(ow.smp_id)) FROM ow_leveldata_raw lvl 
                                                LEFT JOIN fieldwork.ow ow on lvl.ow_uid = ow.ow_uid
                                                LEFT JOIN fieldwork.ow_ownership own on lvl.ow_uid = own.ow_uid
                                                WHERE own.public = TRUE"))
  
  rv$public_systems_monitored_to_date_value <- reactive(dbGetQuery(poolConn, rv$public_systems_monitored_to_date_q()))
  rv$public_systems_monitored_to_date <- reactive(data.frame(Metric = as.character("Public Systems Monitored"), 
                                                                                   to_date_count = rv$public_systems_monitored_to_date_value()))
  
  #private systems
  # No. of private systems monitored
  rv$private_systems_monitored_q <- reactive(paste0("SELECT COUNT(DISTINCT smp_to_system(ow.smp_id)) FROM ow_leveldata_raw lvl 
                                                LEFT JOIN fieldwork.ow ow on lvl.ow_uid = ow.ow_uid
                                                LEFT JOIN fieldwork.ow_ownership own on lvl.ow_uid = own.ow_uid
                                                WHERE dtime_est >= '", rv$start_date(), "'
                                                AND dtime_est <= '", rv$end_date(), "'
                                                AND own.public = FALSE"))
  
  rv$private_systems_monitored_value <- reactive(dbGetQuery(poolConn, rv$private_systems_monitored_q()))
  rv$private_systems_monitored <- reactive(data.frame(Metric = as.character("Private Systems Monitored"), Count = rv$private_systems_monitored_value()))
  
  #No. of private systems monitored to date
  rv$private_systems_monitored_to_date_q <- reactive(paste0("SELECT COUNT(DISTINCT smp_to_system(ow.smp_id)) FROM ow_leveldata_raw lvl 
                                                LEFT JOIN fieldwork.ow ow on lvl.ow_uid = ow.ow_uid
                                                LEFT JOIN fieldwork.ow_ownership own on lvl.ow_uid = own.ow_uid
                                                WHERE own.public = FALSE"))
  
  rv$private_systems_monitored_to_date_value <- reactive(dbGetQuery(poolConn, rv$private_systems_monitored_to_date_q()))
  rv$private_systems_monitored_to_date <- reactive(data.frame(Metric = as.character("Private Systems Monitored"), 
                                                             to_date_count = rv$private_systems_monitored_to_date_value()))
  #smps -----
  
  # No. of public smps monitored
  rv$public_smps_monitored_q <- reactive(paste0("SELECT COUNT(DISTINCT ow.smp_id) FROM ow_leveldata_raw lvl 
                                                LEFT JOIN fieldwork.ow ow on lvl.ow_uid = ow.ow_uid
                                                LEFT JOIN fieldwork.ow_ownership own on lvl.ow_uid = own.ow_uid
                                                WHERE dtime_est >= '", rv$start_date(), "'
                                                AND dtime_est <= '", rv$end_date(), "'
                                                AND own.public = TRUE"))
  
  rv$public_smps_monitored_value <- reactive(dbGetQuery(poolConn, rv$public_smps_monitored_q()))
  rv$public_smps_monitored <- reactive(data.frame(Metric = as.character("Public SMPs Monitored"), Count = rv$public_smps_monitored_value()))
  

  #No. of new smps monitored
  rv$new_smps_monitored_q <- reactive(paste0("SELECT count(dada.smp_id) FROM 
                                              (SELECT smp_id FROM ow_leveldata_raw lvl 
                                                LEFT JOIN fieldwork.ow ow on lvl.ow_uid = ow.ow_uid
                                                GROUP BY ow.smp_id 
                                                HAVING min(lvl.dtime_est) >= '", rv$start_date(), "'
                                                AND min(lvl.dtime_est) <= '", rv$end_date(), "') dada"))
  
  rv$new_smps_monitored_value <- reactive(dbGetQuery(poolConn, rv$new_smps_monitored_q()))
  rv$new_smps_monitored <- reactive(data.frame(Metric = as.character("New SMPs Monitored"), Count = rv$new_smps_monitored_value()))
  

# sensors -----------------------------------------------------------------

  
  #hobos deployed
  rv$hobos_deployed_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.deployment 
                                         WHERE deployment_dtime_est <= '", rv$end_date(), "'
                                                AND (collection_dtime_est >= '", rv$start_date(), "' 
                                         OR collection_dtime_est IS NULL)"))
  
  rv$hobos_deployed_value <- reactive(dbGetQuery(poolConn, rv$hobos_deployed_q()))
  rv$hobos_deployed <- reactive(data.frame(Metric = "Sensors Deployed", 
                                           Count = rv$hobos_deployed_value()))
  

  # post-con / con testing stats ----------------------------------------------------------------
  
  rv$select_range_tests_query_text <- reactive(if(input$date_range == "Select Range"){
    paste("AND test_date >= '", rv$start_date(), "' AND
                                         test_date <= '", rv$end_date(), "'")
  }else{
    paste("")
  })
  
  #SRTs (by selected date)
  #Pre-Inspection SRTs
  rv$pre_inspection_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE phase = '", input$phase, "' AND
                                         type = 'Pre-Inspection Dye Test'", rv$select_range_tests_query_text()))
  
  rv$pre_inspection_value <- reactive(dbGetQuery(poolConn, rv$pre_inspection_q()))
  
  rv$pre_inspection <- reactive(data.frame(Metric = "Pre-Inspection Dye Tests", 
                                           Count = rv$pre_inspection_value()))
  
  
  #Performance SRTs
  rv$performance_srts_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE phase = '", input$phase, "' AND
                                         type = 'Performance Test'", rv$select_range_tests_query_text()))
  
  rv$performance_srts_value <- reactive(dbGetQuery(poolConn, rv$performance_srts_q()))
  
  rv$performance_srts <- reactive(data.frame(Metric = "Performance SRTs", 
                                             Count = rv$performance_srts_value()))
  
  #CCTV/Dye Tests
  rv$cctv_dye_test_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE phase = '", input$phase, "' AND
                                         type = 'CCTV Dye Test'", rv$select_range_tests_query_text()))
  
  rv$cctv_dye_test_value <- reactive(dbGetQuery(poolConn, rv$cctv_dye_test_q()))
  
  rv$cctv_dye_test <- reactive(data.frame(Metric = "CCTV Dye Tests", 
                                          Count = rv$cctv_dye_test_value()))
  
  #systems tested: SRT
  rv$systems_tested_srt_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.srt_full 
        WHERE phase = '", input$phase, "'", rv$select_range_tests_query_text()))
  
  rv$systems_tested_srt_value <- reactive(dbGetQuery(poolConn, rv$systems_tested_srt_q()))
  
  rv$systems_tested_srt <- reactive(data.frame(Metric = "Systems Tested - SRT", 
                                               Count = rv$systems_tested_srt_value()))
  
  #systems tested: porous pavement
  rv$systems_tested_ppt_q <- reactive(paste0("SELECT COUNT(distinct smp_to_system(smp_id)) FROM fieldwork.porous_pavement_full 
        WHERE phase = '", input$phase, "'", rv$select_range_tests_query_text()))
  
  rv$systems_tested_ppt_value <- reactive(dbGetQuery(poolConn, rv$systems_tested_ppt_q()))
  
  rv$systems_tested_ppt <- reactive(data.frame(Metric = "Systems Tested - Porous Pavement", 
                                               Count = rv$systems_tested_ppt_value()))
  
  #systems tested: CET
  rv$systems_tested_cet_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.capture_efficiency_full 
        WHERE phase = '", input$phase, "'", rv$select_range_tests_query_text()))
  
  rv$systems_tested_cet_value <- reactive(dbGetQuery(poolConn, rv$systems_tested_cet_q()))
  
  rv$systems_tested_cet <- reactive(data.frame(Metric = "Systems Tested - Capture Efficiency", 
                                               Count = rv$systems_tested_cet_value()))

  # CET ------------------------------------------------------------
  
  rv$cet_inlet_query_test <- reactive(if(input$inlet_type == "All"){
    paste("")
  }else if(input$inlet_type == "Inlets"){
    paste("AND 
        asset_type IN ('Inlet', 'Fitting')")
  }else if(input$inlet_type == "Curbcuts"){
    paste("AND 
        asset_type IN ('CURBCUT', 'TrenchDrain')")
  }
  )
  
  rv$cet_unique_query_text <- reactive(if(input$unique_inlets == "Include All Tests"){ 
    paste("fieldwork.capture_efficiency_full")
  }else if(input$unique_inlets == "Most Recent for Each Inlet"){
    paste("fieldwork.capture_efficiency_full_unique_inlets")
  })
  
  #capture efficiency tests (pc)
  rv$cet_lf_q <- reactive(paste0("SELECT COUNT(*) FROM ", rv$cet_unique_query_text(), " 
        WHERE phase = '", input$phase, "'", rv$select_range_tests_query_text(), rv$cet_inlet_query_test(), "
        AND low_flow_bypass_observed IS NOT NULL"))
  
  rv$cet_lf_value <- reactive(dbGetQuery(poolConn, rv$cet_lf_q()))
  
  rv$cet_lf <- reactive(data.frame(Metric = "Low Flow Capture Efficiency Tests", 
                                   Count = rv$cet_lf_value()))
  
  #capture efficiency tests (pc)
  rv$cet_hf_q <- reactive(paste0("SELECT COUNT(*) FROM ", rv$cet_unique_query_text(), " 
        WHERE phase = '", input$phase, "'", rv$select_range_tests_query_text(), rv$cet_inlet_query_test(), "
        AND high_flow_efficiency_pct IS NOT NULL"))
  
  rv$cet_hf_value <- reactive(dbGetQuery(poolConn, rv$cet_hf_q()))
  
  rv$cet_hf <- reactive(data.frame(Metric = "High Flow Capture Efficiency Tests", 
                                   Count = rv$cet_hf_value()))
  
  #capture efficiency tests with no low flow bypass (pc)
  rv$cet_no_lf_bypass_q <- reactive(paste0("SELECT COUNT(*) FROM ", rv$cet_unique_query_text(), " 
        WHERE phase = '", input$phase, "' AND 
        low_flow_efficiency_pct = 100 ", rv$select_range_tests_query_text(), rv$cet_inlet_query_test()))
  
  rv$cet_no_lf_bypass_value <- reactive(dbGetQuery(poolConn, rv$cet_no_lf_bypass_q()))
  
  rv$cet_no_lf_bypass <- reactive(data.frame(Metric = "Capture Efficiency Tests with No Low Flow Bypass", 
                                             Count = rv$cet_no_lf_bypass_value()))
  
  #capture efficiency tests with no high flow bypass (pc)
  rv$cet_no_hf_bypass_q <- reactive(paste0("SELECT COUNT(*) FROM ", rv$cet_unique_query_text(), " 
        WHERE phase = '", input$phase, "' AND 
        high_flow_efficiency_pct = 100 ", rv$select_range_tests_query_text(), rv$cet_inlet_query_test()))
  
  rv$cet_no_hf_bypass_value <- reactive(dbGetQuery(poolConn, rv$cet_no_hf_bypass_q()))
  
  rv$cet_no_hf_bypass <- reactive(data.frame(Metric = "Capture Efficiency Tests with No High Flow Bypass", 
                                             Count = rv$cet_no_hf_bypass_value()))
  
  #capture efficiency tests with > 10% low flow bypass (pc)
  rv$cet_10pct_lf_bypass_q <- reactive(paste0("SELECT COUNT(*) FROM ", rv$cet_unique_query_text(), " 
        WHERE phase = '", input$phase, "' AND 
        low_flow_efficiency_pct < 90 ", rv$select_range_tests_query_text(), rv$cet_inlet_query_test()))
  
  rv$cet_10pct_lf_bypass_value <- reactive(dbGetQuery(poolConn, rv$cet_10pct_lf_bypass_q()))
  
  rv$cet_10pct_lf_bypass <- reactive(data.frame(Metric = "Capture Efficiency Tests with > 10% Low Flow Bypass", 
                                                Count = rv$cet_10pct_lf_bypass_value()))
  
  #capture efficiency tests with > 10% high flow bypass (pc)
  rv$cet_10pct_hf_bypass_q <- reactive(paste0("SELECT COUNT(*) FROM ", rv$cet_unique_query_text(), " 
        WHERE phase = '", input$phase, "' AND 
        high_flow_efficiency_pct < 90 ", rv$select_range_tests_query_text(), rv$cet_inlet_query_test()))
  
  rv$cet_10pct_hf_bypass_value <- reactive(dbGetQuery(poolConn, rv$cet_10pct_hf_bypass_q()))
  
  rv$cet_10pct_hf_bypass <- reactive(data.frame(Metric = "Capture Efficiency Tests with > 10% High Flow Bypass", 
                                                Count = rv$cet_10pct_hf_bypass_value()))
}