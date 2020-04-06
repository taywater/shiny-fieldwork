# set up (define non-reactive variables to be used throughout modules) ----------------------------------

#set database connection
poolConn <- dbPool(odbc(), dsn = "mars_testing")

#query all SMP IDs
smp_id <- odbc::dbGetQuery(poolConn, paste0("select distinct smp_id from smpid_facilityid_componentid")) %>% 
  dplyr::arrange(smp_id) %>% 
  dplyr::pull()

sys_id <- odbc::dbGetQuery(poolConn, paste0("select distinct system_id from smpid_facilityid_componentid")) %>% 
  dplyr::arrange(system_id) %>% 
  dplyr::pull()

#disconnect from db on stop 
#db connection may be replaced with POOL soon 
# onStop(function(){
#   poolClose(poolConn)
# })

#Sensor Model Number options
hobo_options <- c("", "U20-001-01", "U20-001-04", "U20L-01", "U20L-04")

#Deployment purpose lookup table
deployment_lookup <- dbGetQuery(poolConn, "select * from fieldwork.deployment_lookup")

#srt_types
srt_types <- dbGetQuery(poolConn, "select * from fieldwork.srt_type_lookup")

#porous pavement surface types
surface_type <- dbGetQuery(poolConn, "select * from fieldwork.surface_type_lookup")

#capture efficiency high flow types
high_flow_type <- dbGetQuery(poolConn, "select * from fieldwork.est_high_flow_efficiency_lookup")

html_req <- function(label){
  HTML(paste(label, tags$span(style="color:red", "*")))
}