collection_calendarUI <- function(id, label = "collection_calendar"){
  #Collection Calendar ##
  ns <- NS(id)
  useShinyjs()
  #create a tabPanel for each tab
  tabPanel("Collection Calendar", value = "calendar_tab", 
           #tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
           
           titlePanel("Collection Calendar"), 
           useShinyjs(), #this function needs to be called anywhere in the UI to use any other Shinyjs() functions
           sidebarPanel(
             selectInput(ns("property_type"), "Property Type", choices = c("All" = .5, "Public" = 1, "Private" = 0)),
             selectInput(ns("interval_filter"), "Interval", choices = c("All" = 10, "5" = 5, "15" = 15)),
             selectInput(ns("capacity_used"), "Capacity Used", choices = c("All", "Less than 80%", "80% or more")), 
             selectInput(ns("purpose_filter"), "Sensor Purpose", choices = c("All" = 1.5, "BARO" = 1, "LEVEL" = 2))
           ), 
           mainPanel(
             DTOutput(ns("collection"))
           )
  )
}

collection_calendar <- function(input, output, session, parent_session, ow, deploy) {
  #define ns to use in modals
  ns <- session$ns
  
  rv <- reactiveValues()  
  
  #query the collection calendar and arrange by deployment_uid
  collect_query <- "select ac.*, own.public from fieldwork.active_deployments ac left join fieldwork.ow_ownership own on ac.ow_uid = own.ow_uid"
  rv$collect_table_db<- odbc::dbGetQuery(poolConn, collect_query)
  
  #upon editing a row in add_ow
  observeEvent(ow$refresh_collect(),{
    rv$collect_table_db <- odbc::dbGetQuery(poolConn, collect_query)
  })
  
  #upon adding or editing a deployment in deploy
  observeEvent(deploy$refresh_collect(),{
    rv$collect_table_db <- odbc::dbGetQuery(poolConn, collect_query)
  })
  
  #arrange and filtered the collection calendar
  rv$collect_table_filter <- reactive(rv$collect_table_db %>% 
                                        dplyr::arrange(deployment_uid) %>% 
                                        #force tz to assure that today() is being properly compared to dates, and trim to ymd for appearance in app
                                        #filter based on the less than 80% or more than 80% capacity
                                        mutate(deployment_dtime_est = lubridate::force_tz(deployment_dtime_est, "America/New_York") %>% lubridate::ymd(), 
                                               date_80percent = lubridate::force_tz(date_80percent, "America/New_York") %>% lubridate::ymd(),
                                               date_100percent = lubridate::force_tz(date_100percent, "America/New_York") %>% lubridate::ymd(), 
                                               filter_80 = case_when(input$capacity_used == "Less than 80%" & date_80percent > today() ~ 1, 
                                                                     input$capacity_used == "Less than 80%" & date_80percent < today() ~ 0, 
                                                                     input$capacity_used == "80% or more" & date_80percent < today() ~ 1, 
                                                                     input$capacity_used == "80% or more" & date_80percent > today() ~ 0, 
                                                                     TRUE ~ 1)) %>% 
                                        #use 1 or 0 for public or private, respectively, and 0.5 for both, with a tolerance of .51 
                                        #so if .5 is selected, 0 and 1 are picked up
                                        dplyr::filter(near(as.numeric(public), as.numeric(input$property_type), tol = 0.51) &
                                                        #use 5 or 15 minute intervals, with 10 for both, with a tolerance of 5.1 
                                                        #so if 10 is selected, 5 and 15 are picked up
                                                        near(interval_min, as.numeric(input$interval_filter), tol = 5.1) &
                                                        near(sensor_purpose, as.numeric(input$purpose_filter), tol = .51) &
                                                        filter_80 == 1))
  #select and rename columns to show in app
  rv$collect_table <- reactive(rv$collect_table_filter() %>% 
                                 dplyr::select(smp_id, ow_suffix, type, deployment_dtime_est,date_80percent,date_100percent)  %>% 
                                 rename("SMP ID" = "smp_id", "OW Suffix" = "ow_suffix", "Purpose" = "type", "Deploy Date" = "deployment_dtime_est", 
                                        "80% Full Date" = "date_80percent", "100% Full Date" = "date_100percent"))
  
  output$collection <- renderDT(
    DT::datatable(
      rv$collect_table(), 
      selection = "single", 
      style = 'bootstrap', 
      class = 'table-responsive, table-hover', 
      options = list(scroller = TRUE, 
                     scrollX = TRUE, 
                     scrollY = 550, 
                     order = list(6, 'asc'))) %>%
      formatStyle(
        '80% Full Date',
        backgroundColor = styleInterval(lubridate::today(), c('yellow', 'transparent')), 
        color = 'black'
      ) %>%
      formatStyle(
        '100% Full Date',
        backgroundColor = styleInterval(lubridate::today(), c('red', 'transparent')),
        color = styleInterval(lubridate::today(), c('white', 'black'))
      ) 
  )
  
  rv$deploy_refresh <- 0
  
  #this is a two-step observeEvent
  #when a line in the calendar is clicked, go toggle and update "smp_id_deploy", and switch tabs
  observeEvent(input$collection_rows_selected, {
    rv$deploy_refresh <- rv$deploy_refresh + 1
    #updateSelectInput(session, "smp_id_deploy", selected = "")
    #updateSelectInput(session, "smp_id_deploy", selected = rv$collect_table_filter()$smp_id[input$collection_rows_selected])
    updateTabsetPanel(session = parent_session, "inTabset", selected = "deploy_tab")
  })
  
  return(
    list(
      sensor_serial = reactive(rv$collect_table_db$sensor_serial),
      smp_id = reactive(rv$collect_table_filter()$smp_id[input$collection_rows_selected]),
      deploy_refresh = reactive(rv$deploy_refresh),
      rows_selected = reactive(input$collection_rows_selected),
      row = reactive(rv$collect_table_filter()$deployment_uid[input$collection_rows_selected])
      )
    )
  
}