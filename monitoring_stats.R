library(shiny)
library(pool)
library(odbc)
library(tidyverse)
library(shinythemes)
library(lubridate)
library(shinyjs)
library(DT)
library(reactable)

options(DT.options = list(pageLength = 15))

folder <- "C:/Users/richard.schaeffer/Documents/R/MARS Fieldwork/"

source(paste0(folder, "setup.R"))

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
                actionButton(ns("print_check"), "Print start date")
              ), 
           mainPanel(DTOutput(ns("table")))
                           
           
  )
}

m_stats <- function(input, output, session){
  
  #define ns to use in modals
  ns <- session$ns
  
  observe(updateSelectInput(session, "end_fy", choices = current_fy:input$start_fy))
  
  rv <- reactiveValues()
  
  
  rv$start_date <- reactive(lubridate::mdy(paste0(input$start_quarter, "/", input$start_fy))%m-% months(6))
  rv$end_date <- reactive(lubridate::mdy(paste0(input$end_quarter, "/", input$end_fy))%m-% months(6))
  
  observeEvent(input$print_check, {
    print("starting")
    if(rv$start_date() > rv$end_date()){
      print("start date is later than end date")
    }else{
      # print(str(rv$public_systems_monitored()))
      # print(str(rv$new_systems_monitored()))
     # print(str(rv$public_systems_monitored_to_date()))
print(str(rv$hobos_deployed()))
    # rv$df <- rbind(
    #   rv$public_systems_monitored(), 
    #   rv$new_systems_monitored()
    #   ) %>% 
    #   column_to_rownames("metric") %>% 
    #   rename(!!paste(rv$start_date(), "to", rv$end_date()) := "count")
    # print(rv$df)
    output$table <- renderDT(
      rv$hobos_deployed()
    )
     }
  }
  )
  
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
  
  #hobos deployed
  rv$hobos_deployed_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.deployment 
                                         WHERE deployment_dtime_est <= '", rv$end_date(), "'
                                                AND (collection_dtime_est >= '", rv$start_date(), "' 
                                         OR collection_dtime_est IS NULL)"))
  
  rv$hobos_deployed_value <- reactive(dbGetQuery(poolConn, rv$hobos_deployed_q()))
  rv$hobos_deployed <- reactive(data.frame(metric = "Sensors Deployed", 
                                           count = rv$hobos_deployed_value()))
  
  #SRTs (by selected date)
  #Pre-Inspection SRTs
  rv$pre_inspection_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE type = 'Pre-Inspection Dye Test' AND
                                         srt_date >= '", rv$start_date(), "' AND
                                         srt_date <= '", rv$end_date(), "'"))
  
  rv$pre_inspection_value <- reactive(dbGetQuery(poolConn, rv$pre_inspection_q()))
  
  rv$pre_inspection <- reactive(data.frame(metric = "Pre-Inspection Dye Tests", 
                                           count = rv$pre_inspection_value))
  
  #Performance SRTs
  rv$performance_srts_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE type = 'Performance Test' AND
                                         srt_date >= '", rv$start_date(), "' AND
                                         srt_date <= '", rv$end_date(), "'"))
  
  rv$performance_srts_value <- reactive(dbGetQuery(poolConn, rv$performance_srts_q()))
  
  rv$performance_srts <- reactive(data.frame(metric = "Performance SRTs", 
                                           count = rv$performance_srts_value))
  
  #CCTV/Dye Tests
  rv$cctv_dye_test_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full 
                                         WHERE type = 'CCTV Dye Test' AND
                                         srt_date >= '", rv$start_date(), "' AND
                                         srt_date <= '", rv$end_date(), "'"))
  
  rv$cctv_dye_test_value <- reactive(dbGetQuery(poolConn, rv$cctv_dye_test_q()))
  
  rv$cctv_dye_test <- reactive(data.frame(metric = "CCTV Dye Tests", 
                                             count = rv$cctv_dye_test_value))
  
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
  #----
}


ui <- navbarPage("Fieldwork", theme = shinytheme("cerulean"), id = "inTabset",
                 m_statsUI("stats")
)

server <- function(input, output, session) {
  callModule(m_stats, "stats")
}

shinyApp(ui, server)