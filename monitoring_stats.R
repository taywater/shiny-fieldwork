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
              )
                           
           
  )
}

m_stats <- function(input, output, session){
  
  #define ns to use in modals
  ns <- session$ns
  
  observe(updateSelectInput(session, "end_fy", choices = current_fy:input$start_fy))
  
  rv <- reactiveValues()
  
  
  rv$start_date_string <- reactive(lubridate::mdy(paste0(input$start_quarter, "/", input$start_fy))%m-% months(6))
  rv$end_date_string <- reactive(lubridate::mdy(paste0(input$end_quarter, "/", input$end_fy))%m-% months(6))
  
  observeEvent(input$print_check, {
    print(rv$new_smps_monitored())
  }
  )
  
  # No. of public systems monitored
  rv$public_smps_monitored_q <- reactive(paste0("SELECT COUNT(DISTINCT ow.smp_id) FROM ow_leveldata_raw lvl 
                                                LEFT JOIN fieldwork.ow ow on lvl.ow_uid = ow.ow_uid
                                                LEFT JOIN fieldwork.ow_ownership own on lvl.ow_uid = own.ow_uid
                                                WHERE dtime_est >= '", rv$start_date_string(), "'
                                                AND dtime_est <= '", rv$end_date_string(), "'
                                                AND own.public = TRUE"))
  
  rv$public_smps_monitored <- reactive(dbGetQuery(poolConn, rv$public_smps_monitored_q()))
  
  #No. of new systems monitored
  rv$new_smps_monitored_q <- reactive(paste0("SELECT count(dada.smp_id) FROM (SELECT smp_id FROM ow_leveldata_raw lvl 
                                                LEFT JOIN fieldwork.ow ow on lvl.ow_uid = ow.ow_uid
                                                GROUP BY ow.smp_id 
                                                HAVING min(lvl.dtime_est) >= '", rv$start_date_string(), "'
                                                AND min(lvl.dtime_est) <= '", rv$end_date_string(), "') dada"))
  
  rv$new_smps_monitored <- reactive(dbGetQuery(poolConn, rv$new_smps_monitored_q()))
}


ui <- navbarPage("Fieldwork", theme = shinytheme("cerulean"), id = "inTabset",
                 m_statsUI("stats")
)

server <- function(input, output, session) {
  callModule(m_stats, "stats")
}

shinyApp(ui, server)