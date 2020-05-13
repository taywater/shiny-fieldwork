library(shiny)
library(pool)
library(odbc)
library(tidyverse)
library(shinythemes)
library(lubridate)
library(shinyjs)
library(DT)
library(reactable)

options(DT.options = list(dom = 't'))

source("setup.R")

#add collapsible checkboxes for each breakdown 
#select all or certain stats to be generated
#table needs to be broken up sensibly

m_statsUI <- function(id, label = "stats"){
  useShinyjs()
  ns <- NS(id)
  tabPanel("Monitoring Stats",
             titlePanel("Monitoring Stats"),
              sidebarPanel(
                fluidRow(column(6,
                selectInput(ns("start_fy"), "Start FY", choices = years)),
                column(6,selectInput(ns("start_quarter"), "Start Quarter", choices = c("Q1" = "1/1", "Q2" = "4/1", "Q3" = "7/1", "Q4" = "10/1")))),
                fluidRow(column(6,
                  selectInput(ns("end_fy"), "End FY", choices = years)),
                  column(6,selectInput(ns("end_quarter"), "End Quarter", choices = c("Q1" = "3/31", "Q2" = "6/30", "Q3" = "9/30", "Q4" = "12/31")))), 
                actionButton(ns("table_button"), "Generate Defined System and SMP Stats"), 
                actionButton(ns("ca_button"), "Generate Defined Contruction Stats"),
                actionButton(ns("postcon_button"), "Generate Defined Post-Construction Stats"),
                actionButton(ns("to_date_button"), "Generate To-Date System and SMP Stats"), 
                actionButton(ns("to_date_ca_button"), "Generate To-Date Contruction Stats"),
                actionButton(ns("to_date_postcon_button"), "Generate To-Date Post-Construction Stats")
              ), 
           mainPanel(
             h4(textOutput(ns("table_name"))), 
                DTOutput(ns("table")), 
             h4(textOutput(ns("ca"))), 
                DTOutput(ns("ca_table")),
             h4(textOutput(ns("postcon"))),
                DTOutput(ns("postcon_table")),
             h4(textOutput(ns("to_date_table_name"))),
                DTOutput(ns("to_date_table")), 
             h4(textOutput(ns("to_date_ca"))),
                DTOutput(ns("to_date_ca_table")),
             h4(textOutput(ns("to_date_postcon"))), 
                DTOutput(ns("to_date_postcon_table"))
             )
                           
           
  )
}

m_stats <- function(input, output, session){
  
  #define ns to use in modals
  ns <- session$ns
  
  observe(updateSelectInput(session, "end_fy", choices = current_fy:input$start_fy))
  
  rv <- reactiveValues()
  
  sf <- lubridate::stamp("March 1, 1999")
  
  rv$start_date <- reactive(lubridate::mdy(paste0(input$start_quarter, "/", input$start_fy))%m-% months(6))
  rv$end_date <- reactive(lubridate::mdy(paste0(input$end_quarter, "/", input$end_fy))%m-% months(6))
  
  #system and smp
  observeEvent(input$table_button, {
    print("starting")
    if(rv$start_date() > rv$end_date()){
      print("start date is later than end date")
    }else{
      output$table_name <- renderText(paste("CWL", sf(rv$start_date()), "to", sf(rv$end_date())))
      
    output$table <- renderDT(
      bind_rows(
        rv$public_systems_monitored(),
        rv$public_smps_monitored(),
        rv$private_systems_monitored(),
        rv$new_systems_monitored(),
        rv$new_smps_monitored(),
        rv$hobos_deployed()
      )
    )
     }
  }
  )
  
  #system and smp to date
  observeEvent(input$to_date_button, {
    print("starting_to_date")
    output$to_date_table_name <- renderText("CWL To Date")
    output$to_date_table <- renderDT(
      bind_rows(
        rv$public_systems_monitored_to_date(),
        rv$private_systems_monitored_to_date()#,
        #rv$hobos_deployed()
      )
    )
  }
  )
  
  #post con
  observeEvent(input$postcon_button, {
    output$postcon <- renderText(paste("Post-Con Tests", sf(rv$start_date()), "to", sf(rv$end_date())))
    output$postcon_table <- renderDT(
      bind_rows(
        rv$pc_pre_inspection(), 
        rv$pc_performance_srts(),
        rv$pc_cctv_dye_test(), 
        rv$pc_systems_tested_srt(),
        rv$pc_systems_tested_ppt(), 
        rv$pc_systems_tested_cet()
      )
    )
  })
  
  observeEvent(input$to_date_postcon_button, {
    output$to_date_postcon <- renderText("Post-Con Tests To Date")
    output$to_date_postcon_table <- renderDT(
      bind_rows(
        rv$pc_pre_inspection_to_date(), 
        rv$pc_performance_srts_to_date(),
        rv$pc_cctv_dye_test_to_date(), 
        rv$pc_systems_tested_srt_to_date(),
        rv$pc_systems_tested_ppt_to_date(), 
        rv$pc_systems_tested_cet_to_date()
      )
    )
  }
  )
  
  #construction
  observeEvent(input$ca_button, {
    output$ca <- renderText(paste("Construction Tests", sf(rv$start_date()), "to", sf(rv$end_date())))
    output$ca_table <- renderDT(
      bind_rows(
        rv$ca_pre_inspection(), 
        rv$ca_performance_srts(),
        rv$ca_cctv_dye_test(), 
        rv$ca_systems_tested_srt(),
        rv$ca_systems_tested_ppt(),
        rv$ca_systems_tested_cet()
      )
    )
  })
  
  #construction
  observeEvent(input$to_date_ca_button, {
    output$to_date_ca <- renderText("Construction Tests to Date")
    output$to_date_ca_table <- renderDT(
      bind_rows(
        rv$ca_pre_inspection_to_date(), 
        rv$ca_performance_srts_to_date(),
        rv$ca_cctv_dye_test_to_date(), 
        rv$ca_systems_tested_srt_to_date(), 
        rv$ca_systems_tested_ppt_to_date(),
        rv$ca_systems_tested_cet_to_date()
      )
    )
  })
  

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
  rv$public_systems_monitored <- reactive(data.frame(metric = as.character("Public Systems Monitored"), count = rv$public_systems_monitored_value()))
  
  #No. of new systems monitored
  rv$new_systems_monitored_q <- reactive(paste0("SELECT count(distinct smp_to_system(dada.smp_id)) FROM 
                                              (SELECT smp_id FROM ow_leveldata_raw lvl 
                                                LEFT JOIN fieldwork.ow ow on lvl.ow_uid = ow.ow_uid
                                                GROUP BY ow.smp_id 
                                                HAVING min(lvl.dtime_est) >= '", rv$start_date(), "'
                                                AND min(lvl.dtime_est) <= '", rv$end_date(), "') dada"))
  
  rv$new_systems_monitored_value <- reactive(dbGetQuery(poolConn, rv$new_systems_monitored_q()))
  rv$new_systems_monitored <- reactive(data.frame(metric = as.character("New Systems Monitored"), count = rv$new_systems_monitored_value()))
  
  #No. of public systems monitored to date
  rv$public_systems_monitored_to_date_q <- reactive(paste0("SELECT COUNT(DISTINCT smp_to_system(ow.smp_id)) FROM ow_leveldata_raw lvl 
                                                LEFT JOIN fieldwork.ow ow on lvl.ow_uid = ow.ow_uid
                                                LEFT JOIN fieldwork.ow_ownership own on lvl.ow_uid = own.ow_uid
                                                WHERE own.public = TRUE"))
  
  rv$public_systems_monitored_to_date_value <- reactive(dbGetQuery(poolConn, rv$public_systems_monitored_to_date_q()))
  rv$public_systems_monitored_to_date <- reactive(data.frame(metric = as.character("Public Systems Monitored"), 
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
  rv$private_systems_monitored <- reactive(data.frame(metric = as.character("Private Systems Monitored"), count = rv$private_systems_monitored_value()))
  
  #No. of private systems monitored to date
  rv$private_systems_monitored_to_date_q <- reactive(paste0("SELECT COUNT(DISTINCT smp_to_system(ow.smp_id)) FROM ow_leveldata_raw lvl 
                                                LEFT JOIN fieldwork.ow ow on lvl.ow_uid = ow.ow_uid
                                                LEFT JOIN fieldwork.ow_ownership own on lvl.ow_uid = own.ow_uid
                                                WHERE own.public = FALSE"))
  
  rv$private_systems_monitored_to_date_value <- reactive(dbGetQuery(poolConn, rv$private_systems_monitored_to_date_q()))
  rv$private_systems_monitored_to_date <- reactive(data.frame(metric = as.character("Private Systems Monitored"), 
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
  rv$public_smps_monitored <- reactive(data.frame(metric = as.character("Public SMPs Monitored"), count = rv$public_smps_monitored_value()))
  

  #No. of new smps monitored
  rv$new_smps_monitored_q <- reactive(paste0("SELECT count(dada.smp_id) FROM 
                                              (SELECT smp_id FROM ow_leveldata_raw lvl 
                                                LEFT JOIN fieldwork.ow ow on lvl.ow_uid = ow.ow_uid
                                                GROUP BY ow.smp_id 
                                                HAVING min(lvl.dtime_est) >= '", rv$start_date(), "'
                                                AND min(lvl.dtime_est) <= '", rv$end_date(), "') dada"))
  
  rv$new_smps_monitored_value <- reactive(dbGetQuery(poolConn, rv$new_smps_monitored_q()))
  rv$new_smps_monitored <- reactive(data.frame(metric = as.character("New SMPs Monitored"), count = rv$new_smps_monitored_value()))
  

# sensors -----------------------------------------------------------------

  
  #hobos deployed
  rv$hobos_deployed_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.deployment 
                                         WHERE deployment_dtime_est <= '", rv$end_date(), "'
                                                AND (collection_dtime_est >= '", rv$start_date(), "' 
                                         OR collection_dtime_est IS NULL)"))
  
  rv$hobos_deployed_value <- reactive(dbGetQuery(poolConn, rv$hobos_deployed_q()))
  rv$hobos_deployed <- reactive(data.frame(metric = "Sensors Deployed", 
                                           count = rv$hobos_deployed_value()))
  

# post-con ----------------------------------------------------------------

  #SRTs (by selected date)
  #Pre-Inspection SRTs
  rv$pc_pre_inspection_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE phase = 'Post-Construction' AND
                                         type = 'Pre-Inspection Dye Test' AND
                                         srt_date >= '", rv$start_date(), "' AND
                                         srt_date <= '", rv$end_date(), "'"))
  
  rv$pc_pre_inspection_value <- reactive(dbGetQuery(poolConn, rv$pc_pre_inspection_q()))
  
  rv$pc_pre_inspection <- reactive(data.frame(metric = "Pre-Inspection Dye Tests", 
                                              count = rv$pc_pre_inspection_value()))
  
  #pre-inspection SRT to date
  rv$pc_pre_inspection_to_date_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE phase = 'Post-Construction' AND
                                         type = 'Pre-Inspection Dye Test' "))
  
  rv$pc_pre_inspection_to_date_value <- reactive(dbGetQuery(poolConn, rv$pc_pre_inspection_to_date_q()))
  
  rv$pc_pre_inspection_to_date <- reactive(data.frame(metric = "Pre-Inspection Dye Tests", 
                                                      count = rv$pc_pre_inspection_to_date_value()))
  
  
  #Performance SRTs
  rv$pc_performance_srts_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE phase = 'Post-Construction' AND
                                         type = 'Performance Test' AND
                                         srt_date >= '", rv$start_date(), "' AND
                                         srt_date <= '", rv$end_date(), "'"))
  
  rv$pc_performance_srts_value <- reactive(dbGetQuery(poolConn, rv$pc_performance_srts_q()))
  
  rv$pc_performance_srts <- reactive(data.frame(metric = "Performance SRTs", 
                                                count = rv$pc_performance_srts_value()))
  
  #performance srt to date
  rv$pc_performance_srts_to_date_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE phase = 'Post-Construction' AND
                                         type = 'Performance Test'"))
  
  rv$pc_performance_srts_to_date_value <- reactive(dbGetQuery(poolConn, rv$pc_performance_srts_to_date_q()))
  
  rv$pc_performance_srts_to_date <- reactive(data.frame(metric = "Performance SRTs", 
                                                        count = rv$pc_performance_srts_to_date_value()))
  
  #CCTV/Dye Tests
  rv$pc_cctv_dye_test_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE phase = 'Post-Construction' AND
                                         type = 'CCTV Dye Test' AND
                                         srt_date >= '", rv$start_date(), "' AND
                                         srt_date <= '", rv$end_date(), "'"))
  
  rv$pc_cctv_dye_test_value <- reactive(dbGetQuery(poolConn, rv$pc_cctv_dye_test_q()))
  
  rv$pc_cctv_dye_test <- reactive(data.frame(metric = "CCTV Dye Tests", 
                                             count = rv$pc_cctv_dye_test_value()))
  
  #CCTV to date
  rv$pc_cctv_dye_test_to_date_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE phase = 'Post-Construction' AND
                                         type = 'CCTV Dye Test' "))
  
  rv$pc_cctv_dye_test_to_date_value <- reactive(dbGetQuery(poolConn, rv$pc_cctv_dye_test_to_date_q()))
  
  rv$pc_cctv_dye_test_to_date <- reactive(data.frame(metric = "CCTV Dye Tests", 
                                                     count = rv$pc_cctv_dye_test_to_date_value()))
  
  #systems tested: SRT
  rv$pc_systems_tested_srt_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.srt_full 
        WHERE phase = 'Post-Construction' AND 
        srt_date >= '", rv$start_date(), "' AND
                                         srt_date <= '", rv$end_date(), "'"))
  
  rv$pc_systems_tested_srt_value <- reactive(dbGetQuery(poolConn, rv$pc_systems_tested_srt_q()))
  
  rv$pc_systems_tested_srt <- reactive(data.frame(metric = "Systems Tested - SRT", 
                                                  count = rv$pc_systems_tested_srt_value()))
  
  #systems tested - srt to date
  rv$pc_systems_tested_srt_to_date_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.srt_full 
        WHERE phase = 'Post-Construction'"))
  
  rv$pc_systems_tested_srt_to_date_value <- reactive(dbGetQuery(poolConn, rv$pc_systems_tested_srt_to_date_q()))
  
  rv$pc_systems_tested_srt_to_date <- reactive(data.frame(metric = "Systems Tested - SRT", 
                                                          count = rv$pc_systems_tested_srt_to_date_value()))

  #systems tested: porous pavement
  rv$pc_systems_tested_ppt_q <- reactive(paste0("SELECT COUNT(distinct smp_to_system(smp_id)) FROM fieldwork.porous_pavement_full 
        WHERE phase = 'Post-Construction' AND 
        test_date >= '", rv$start_date(), "' AND
                                         test_date <= '", rv$end_date(), "'"))
  
  rv$pc_systems_tested_ppt_value <- reactive(dbGetQuery(poolConn, rv$pc_systems_tested_ppt_q()))
  
  rv$pc_systems_tested_ppt <- reactive(data.frame(metric = "Systems Tested - Porous Pavement", 
                                                  count = rv$pc_systems_tested_ppt_value()))
  
  #systems tested - porous pavement to date
  rv$pc_systems_tested_ppt_to_date_q <- reactive(paste0("SELECT COUNT(distinct smp_to_system(smp_id)) FROM fieldwork.porous_pavement_full 
        WHERE phase = 'Post-Construction'"))
  
  rv$pc_systems_tested_ppt_to_date_value <- reactive(dbGetQuery(poolConn, rv$pc_systems_tested_ppt_to_date_q()))
  
  rv$pc_systems_tested_ppt_to_date <- reactive(data.frame(metric = "Systems Tested - Porous Pavement", 
                                                          count = rv$pc_systems_tested_ppt_to_date_value()))
  
  #systems tested: CET
  rv$pc_systems_tested_cet_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.capture_efficiency_full 
        WHERE phase = 'Post-Construction' AND 
        test_date >= '", rv$start_date(), "' AND
                                         test_date <= '", rv$end_date(), "'"))
  
  rv$pc_systems_tested_cet_value <- reactive(dbGetQuery(poolConn, rv$pc_systems_tested_cet_q()))
  
  rv$pc_systems_tested_cet <- reactive(data.frame(metric = "Systems Tested - Capture Efficiency", 
                                                  count = rv$pc_systems_tested_cet_value()))
  
  #systems tested - CET to date
  rv$pc_systems_tested_cet_to_date_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.capture_efficiency_full 
        WHERE phase = 'Post-Construction'"))
  
  rv$pc_systems_tested_cet_to_date_value <- reactive(dbGetQuery(poolConn, rv$pc_systems_tested_cet_to_date_q()))
  
  rv$pc_systems_tested_cet_to_date <- reactive(data.frame(metric = "Systems Tested - Capture Efficiency", 
                                                          count = rv$pc_systems_tested_cet_to_date_value()))
  
# construction  (ca) --------------------------------------------
  #Pre-Inspection SRTs
  rv$ca_pre_inspection_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE phase = 'Construction' AND
                                         type = 'Pre-Inspection Dye Test' AND
                                         srt_date >= '", rv$start_date(), "' AND
                                         srt_date <= '", rv$end_date(), "'"))
  
  rv$ca_pre_inspection_value <- reactive(dbGetQuery(poolConn, rv$ca_pre_inspection_q()))
  
  rv$ca_pre_inspection <- reactive(data.frame(metric = "Pre-Inspection Dye Tests", 
                                              count = rv$ca_pre_inspection_value()))
  
  #pre-inspection SRT to date
  rv$ca_pre_inspection_to_date_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE phase = 'Construction' AND
                                         type = 'Pre-Inspection Dye Test' "))
  
  rv$ca_pre_inspection_to_date_value <- reactive(dbGetQuery(poolConn, rv$ca_pre_inspection_to_date_q()))
  
  rv$ca_pre_inspection_to_date <- reactive(data.frame(metric = "Pre-Inspection Dye Tests", 
                                                      count = rv$ca_pre_inspection_to_date_value()))
  
  
  #Performance SRTs
  rv$ca_performance_srts_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE phase = 'Construction' AND
                                         type = 'Performance Test' AND
                                         srt_date >= '", rv$start_date(), "' AND
                                         srt_date <= '", rv$end_date(), "'"))
  
  rv$ca_performance_srts_value <- reactive(dbGetQuery(poolConn, rv$ca_performance_srts_q()))
  
  rv$ca_performance_srts <- reactive(data.frame(metric = "Performance SRTs", 
                                                count = rv$ca_performance_srts_value()))
  
  #performance srt to date
  rv$ca_performance_srts_to_date_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE phase = 'Construction' AND
                                         type = 'Performance Test'"))
  
  rv$ca_performance_srts_to_date_value <- reactive(dbGetQuery(poolConn, rv$ca_performance_srts_to_date_q()))
  
  rv$ca_performance_srts_to_date <- reactive(data.frame(metric = "Performance SRTs", 
                                                        count = rv$ca_performance_srts_to_date_value()))
  
  #CCTV/Dye Tests
  rv$ca_cctv_dye_test_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE phase = 'Construction' AND
                                         type = 'CCTV Dye Test' AND
                                         srt_date >= '", rv$start_date(), "' AND
                                         srt_date <= '", rv$end_date(), "'"))
  
  rv$ca_cctv_dye_test_value <- reactive(dbGetQuery(poolConn, rv$ca_cctv_dye_test_q()))
  
  rv$ca_cctv_dye_test <- reactive(data.frame(metric = "CCTV Dye Tests", 
                                             count = rv$ca_cctv_dye_test_value()))
  
  #CCTV to date
  rv$ca_cctv_dye_test_to_date_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE phase = 'Construction' AND
                                         type = 'CCTV Dye Test' "))
  
  rv$ca_cctv_dye_test_to_date_value <- reactive(dbGetQuery(poolConn, rv$ca_cctv_dye_test_to_date_q()))
  
  rv$ca_cctv_dye_test_to_date <- reactive(data.frame(metric = "CCTV Dye Tests", 
                                                     count = rv$ca_cctv_dye_test_to_date_value()))
  
  #systems tested: SRT
  rv$ca_systems_tested_srt_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.srt_full 
        WHERE phase = 'Construction' AND 
        srt_date >= '", rv$start_date(), "' AND
                                         srt_date <= '", rv$end_date(), "'"))
  
  rv$ca_systems_tested_srt_value <- reactive(dbGetQuery(poolConn, rv$ca_systems_tested_srt_q()))
  
  rv$ca_systems_tested_srt <- reactive(data.frame(metric = "Systems Tested - SRT", 
                                                          count = rv$ca_systems_tested_srt_value()))
  
  #systems tested - srt to date
  rv$ca_systems_tested_srt_to_date_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.srt_full 
        WHERE phase = 'Construction'"))
  
  rv$ca_systems_tested_srt_to_date_value <- reactive(dbGetQuery(poolConn, rv$ca_systems_tested_srt_to_date_q()))
  
  rv$ca_systems_tested_srt_to_date <- reactive(data.frame(metric = "Systems Tested - SRT", 
                                             count = rv$ca_systems_tested_srt_to_date_value()))
  
  #systems tested: porous pavement
  rv$ca_systems_tested_ppt_q <- reactive(paste0("SELECT COUNT(distinct smp_to_system(smp_id)) FROM fieldwork.porous_pavement_full 
        WHERE phase = 'Construction' AND 
        test_date >= '", rv$start_date(), "' AND
                                         test_date <= '", rv$end_date(), "'"))
  
  rv$ca_systems_tested_ppt_value <- reactive(dbGetQuery(poolConn, rv$ca_systems_tested_ppt_q()))
  
  rv$ca_systems_tested_ppt <- reactive(data.frame(metric = "Systems Tested - Porous Pavement", 
                                                  count = rv$ca_systems_tested_ppt_value()))
  
  #systems tested - porous pavement to date
  rv$ca_systems_tested_ppt_to_date_q <- reactive(paste0("SELECT COUNT(distinct smp_to_system(smp_id)) FROM fieldwork.porous_pavement_full 
        WHERE phase = 'Construction'"))
  
  rv$ca_systems_tested_ppt_to_date_value <- reactive(dbGetQuery(poolConn, rv$ca_systems_tested_ppt_to_date_q()))
  
  rv$ca_systems_tested_ppt_to_date <- reactive(data.frame(metric = "Systems Tested - Porous Pavement", 
                                                          count = rv$ca_systems_tested_ppt_to_date_value()))
  
  #systems tested: CET
  rv$ca_systems_tested_cet_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.capture_efficiency_full 
        WHERE phase = 'Post-Construction' AND 
        test_date >= '", rv$start_date(), "' AND
                                         test_date <= '", rv$end_date(), "'"))
  
  rv$ca_systems_tested_cet_value <- reactive(dbGetQuery(poolConn, rv$ca_systems_tested_cet_q()))
  
  rv$ca_systems_tested_cet <- reactive(data.frame(metric = "Systems Tested - Capture Efficiency", 
                                                  count = rv$ca_systems_tested_cet_value()))
  
  #systems tested - CET to date
  rv$ca_systems_tested_cet_to_date_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.capture_efficiency_full 
        WHERE phase = 'Post-Construction'"))
  
  rv$ca_systems_tested_cet_to_date_value <- reactive(dbGetQuery(poolConn, rv$ca_systems_tested_cet_to_date_q()))
  
  rv$ca_systems_tested_cet_to_date <- reactive(data.frame(metric = "Systems Tested - Capture Efficiency", 
                                                          count = rv$ca_systems_tested_cet_to_date_value()))
  
  
}


ui <- navbarPage("Fieldwork", theme = shinytheme("cerulean"), id = "inTabset",
                 m_statsUI("stats")
)

server <- function(input, output, session) {
  callModule(m_stats, "stats")
}

shinyApp(ui, server)