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

######  
  #sourcing
  source(paste0(folder, "setup.R"))
  source(paste0(folder, "collection_calendar.R"))
  source(paste0(folder, "add_ow.R"))
  source(paste0(folder, "add_sensor.R"))
  source(paste0(folder, "deploy.R"))
  source(paste0(folder, "srt.R"))
  source(paste0(folder, "porous_pavement.R"))
  source(paste0(folder, "capture_efficiency.R"))
  source(paste0(folder, "documentation.R"))
    
  
  ui <- navbarPage("Fieldwork", theme = shinytheme("cerulean"), id = "inTabset",
                   collection_calendarUI("collection_calendar"), 
                   add_owUI("add_ow"),
                   add_sensorUI("add_sensor"), 
                   deployUI("deploy"),
                   SRTUI("srt"),
                   porous_pavementUI("porous_pavement"),
                  capture_efficiencyUI("capture_efficiency"),
                  documentationUI("documentation")
                 # useShinyjs()
  )
  
  server <- function(input, output, session) {
    collection_cal <- callModule(collection_calendar, "collection_calendar", parent_session = session, ow = ow, deploy = deploy)
    ow <- callModule(add_ow, "add_ow", parent_session = session)
    sensor <- callModule(add_sensor, "add_sensor", parent_session = session)
    deploy <- callModule(deploy, "deploy", parent_session = session, ow = ow, collect = collection_cal, sensor = sensor)
    callModule(SRT, "srt", parent_session = session)
    callModule(porous_pavement, "porous_pavement", parent_session = session)
    callModule(capture_efficiency, "capture_efficiency", parent_session = session)
    
  }
  
  shinyApp(ui, server)