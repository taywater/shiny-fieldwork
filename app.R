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
#set db connection
poolConn <- dbPool(odbc(), dsn = "mars_testing", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))

# set up (define non-reactive variables to be used throughout modules) ----------------------------------

#query all SMP IDs
smp_id <- odbc::dbGetQuery(poolConn, paste0("select distinct smp_id from smpid_facilityid_componentid")) %>% 
  dplyr::arrange(smp_id) %>% 
  dplyr::pull()

sys_id <- odbc::dbGetQuery(poolConn, paste0("select distinct system_id from smpid_facilityid_componentid")) %>% 
  dplyr::arrange(system_id) %>% 
  dplyr::pull()

#disconnect from db on stop 
#db connection may be replaced with POOL soon 
onStop(function(){
  poolClose(poolConn)
})

#Sensor Model Number options
hobo_options <- c("", "U20-001-01", "U20-001-04", "U20L-01", "U20L-04")

#Deployment purpose lookup table
deployment_lookup <- dbGetQuery(poolConn, "select * from fieldwork.deployment_lookup")

#srt_types & con phase
srt_types <- dbGetQuery(poolConn, "select * from fieldwork.srt_type_lookup")
con_phase <- dbGetQuery(poolConn, "select * from fieldwork.con_phase_lookup")

#porous pavement surface types
surface_type <- dbGetQuery(poolConn, "select * from fieldwork.surface_type_lookup")
# 
# #capture efficiency high flow types
high_flow_type <- dbGetQuery(poolConn, "select * from fieldwork.est_high_flow_efficiency_lookup")

html_req <- function(label){
  HTML(paste(label, tags$span(style="color:red", "*")))
}

#cat(file=stderr(), smp_id)
# #monitoring stats
# current_fy <- lubridate::today() %m+% months(6) %>% year()
# start_fy <- 2012
# years <- start_fy:current_fy %>% sort(decreasing = TRUE)
# 
# fy_quarters <- c("Q1", "Q2", "Q3", "Q4")
# quarter_starts <- c("7/1", "10/1", "1/1", "4/1")
# quarter_ends <- c("9/30", "12/31", "3/31", "6/30")
# 
# df_quarters <- data.frame(fy_quarters, quarter_starts, quarter_ends)


######  
  #source("setup.R")
  source("collection_calendar.R")
  source("add_ow.R")
  source("add_sensor.R")
  source("deploy.R")
  source("srt.R")
  source("porous_pavement.R")
  source("capture_efficiency.R")
  source("documentation.R")

  
  ui <- navbarPage("Fieldwork", theme = shinytheme("cerulean"), id = "inTabset",
                 collection_calendarUI("collection_calendar"),
                 add_owUI("add_ow", smp_id = smp_id, html_req = html_req),
                 add_sensorUI("add_sensor", hobo_options = hobo_options, html_req = html_req),
                 deployUI("deploy", smp_id = smp_id, html_req = html_req),
                 SRTUI("srt", sys_id = sys_id, srt_types = srt_types, html_req = html_req, con_phase = con_phase),
                 porous_pavementUI("porous_pavement", smp_id = smp_id, html_req = html_req, surface_type = surface_type, con_phase = con_phase),
                 capture_efficiencyUI("capture_efficiency", sys_id = sys_id, high_flow_type = high_flow_type, html_req = html_req, con_phase = con_phase),
                 documentationUI("documentation")
                 # useShinyjs()
  )
  
  server <- function(input, output, session) {
    collection_cal <- callModule(collection_calendar, "collection_calendar", parent_session = session, ow = ow, deploy = deploy, poolConn = poolConn)
    ow <- callModule(add_ow, "add_ow", parent_session = session, smp_id = smp_id, poolConn = poolConn)
    sensor <- callModule(add_sensor, "add_sensor", parent_session = session, poolConn = poolConn)
    deploy <- callModule(deploy, "deploy", parent_session = session, ow = ow, collect = collection_cal, sensor = sensor, poolConn = poolConn)
    callModule(SRT, "srt", parent_session = session, poolConn = poolConn, srt_types = srt_types, con_phase = con_phase)
    callModule(porous_pavement, "porous_pavement", parent_session = session, surface_type = surface_type, poolConn = poolConn, con_phase = con_phase)
    callModule(capture_efficiency, "capture_efficiency", parent_session = session, poolConn = poolConn, high_flow_type = high_flow_type, con_phase = con_phase)
    
  }
  
  shinyApp(ui, server)