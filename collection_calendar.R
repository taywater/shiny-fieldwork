collection_calendarUI <- function(id, label = "collection_calendar"){
  #Collection Calendar ##
  ns <- NS(id)
  useShinyjs()
  
  #create a tabPanel for each tab
  list(
    tabPanel("Collection Calendar", value = "calendar_tab", 
             #tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
             
             titlePanel("Collection Calendar"), 
             useShinyjs(), #this function needs to be called anywhere in the UI to use any other Shinyjs() functions
             sidebarPanel(
               selectInput(ns("property_type"), "Property Type", choices = c("All" = .5, "Public" = 1, "Private" = 0)),
               selectInput(ns("interval_filter"), "Interval", choices = c("All" = 10, "5" = 5, "15" = 15)),
               selectInput(ns("capacity_used"), "Capacity Used", choices = c("All", "Less than 80%", "80% or more")), 
               selectInput(ns("purpose_filter"), "Sensor Purpose", choices = c("All" = 1.5, "BARO" = 1, "LEVEL" = 2, "DATALOGGER" = 3)),
               selectInput(ns("term_filter"), "Term", choices = c("All" = 1.5, "Short" = 1, "Long"  = 2, "SRT" = 3, "Special" = 4)),
               selectInput(ns("research_filter"), "Research", choices = c("All" = 1.5, "USEPA STAR" = 1))
             ), 
             mainPanel(
               DTOutput(ns("collection"))
             )
    ), 
    tabPanel("Future Deployments", value = "future_tab", 
             
             titlePanel("Future Deployments"), 
                DTOutput(ns("future"))
    )
  )
  
}

collection_calendar <- function(input, output, session, parent_session, ow, deploy, poolConn) {
  #define ns to use in modals
  ns <- session$ns
  
  rv <- reactiveValues()  
  
  #query the collection calendar and arrange by deployment_uid
  collect_query <- "select * from fieldwork.active_deployments"
  rv$collect_table_db<- odbc::dbGetQuery(poolConn, collect_query)
  
  #query the future deployment table
  future_query <- "select * from fieldwork.future_deployments_full order by field_test_priority_lookup_uid"
  rv$future_table_db <- odbc::dbGetQuery(poolConn, future_query)
  
  #upon editing a row in add_ow
  observeEvent(ow$refresh_collect(),{
    rv$collect_table_db <- odbc::dbGetQuery(poolConn, collect_query)
    rv$future_table_db <- odbc::dbGetQuery(poolConn, future_query)
  })
  
  #upon adding or editing a deployment in deploy
  observeEvent(deploy$refresh_collect(),{
    rv$collect_table_db <- odbc::dbGetQuery(poolConn, collect_query)
    rv$future_table_db <- odbc::dbGetQuery(poolConn, future_query)
  })
  
  
  rv$term_filter <- reactive(if(input$term_filter == 1.5){c(0, 1, 2, 3, 4)} else {input$term_filter})
  rv$purpose_filter <- reactive(if(input$purpose_filter == 1.5){c(0, 1, 2, 3)} else {input$purpose_filter})
  
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
                                        mutate_at(vars(one_of("previous_download_error")),
                                                  funs(case_when(. == 1 ~ "Yes",
                                                                 . == 0 ~ "No"))) %>%
                                        #use 1 or 0 for public or private, respectively, and 0.5 for both, with a tolerance of .51 
                                        #so if .5 is selected, 0 and 1 are picked up
                                        dplyr::filter(near(as.numeric(public), as.numeric(input$property_type), tol = 0.51) &
                                                        #use 5 or 15 minute intervals, with 10 for both, with a tolerance of 5.1 
                                                        #so if 10 is selected, 5 and 15 are picked up
                                                        near(interval_min, as.numeric(input$interval_filter), tol = 5.1) &
                                                        sensor_purpose %in% rv$purpose_filter() &
                                                        long_term_lookup_uid %in% rv$term_filter() &
                                                        filter_80 == 1))
  
  rv$collect_table_filter2 <- reactive(if(input$research_filter == 1.5){
    rv$collect_table_filter()
    }else{
      rv$collect_table_filter() %>% 
        dplyr::filter(research_lookup_uid == input$research_filter)})
  
  #select and rename columns to show in app
  rv$collect_table <- reactive(rv$collect_table_filter2() %>% 
                                 dplyr::select(smp_id, ow_suffix, project_name, type, term, previous_download_error, #research, designation,
                                               deployment_dtime_est,date_80percent,date_100percent)  %>% 
                                 rename("SMP ID" = "smp_id", "OW Suffix" = "ow_suffix", "Project Name" = "project_name", "Purpose" = "type", 
                                        "Term" = "term", "Prev. DL Error" = "previous_download_error",
                                        #"Research" = "research", "Designation" = "designation",
                                        "Deploy Date" = "deployment_dtime_est", 
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
                     order = list(8, 'asc')), 
      rownames = FALSE) %>%
      formatStyle(
        '80% Full Date',
        backgroundColor = styleInterval(lubridate::today(), c('yellow', 'transparent')), 
        color = 'black'
      ) %>%
      formatStyle(
        '100% Full Date',
        backgroundColor = styleInterval(lubridate::today(), c('red', 'transparent')),
        color = styleInterval(lubridate::today(), c('white', 'black'))
      ) %>% 
      formatStyle(
        'Prev. DL Error',
        backgroundColor = styleEqual('Yes', 'yellow')
      )
  )
  
  rv$deploy_refresh <- 0
  rv$future_deploy_refresh <- 0
  
  #this is a two-step observeEvent
  #when a line in the calendar is clicked, go toggle and update "smp_id_deploy", and switch tabs
  observeEvent(input$collection_rows_selected, {
    rv$deploy_refresh <- rv$deploy_refresh + 1
    updateTabsetPanel(session = parent_session, "inTabset", selected = "deploy_tab")
  })
  
  ##Future table details
  rv$future_table <- reactive(rv$future_table_db %>% 
                                dplyr::select("smp_id", "ow_suffix", "project_name", "term", "field_test_priority", "notes") %>% 
                                dplyr::rename("SMP ID" = "smp_id", "OW Suffix" = "ow_suffix", "Project Name" = "project_name", 
                                              "Term" = "term", "Priority" = "field_test_priority","Notes" = "notes")
  )
  
  output$future <- renderDT(
    rv$future_table(),
      selection = "single", 
      style = 'bootstrap', 
      class = 'table-responsive, table-hover'
  )
  
  observeEvent(input$future_rows_selected, {
    rv$future_deploy_refresh <- rv$future_deploy_refresh + 1
    updateTabsetPanel(session = parent_session, "inTabset", selected = "deploy_tab")
  })
  
  return(
    list(
      sensor_serial = reactive(rv$collect_table_db$sensor_serial),
      smp_id = reactive(rv$collect_table_filter()$smp_id[input$collection_rows_selected]),
      site_name = reactive(rv$collect_table_filter()$site_name[input$collection_rows_selected]),
      deploy_refresh = reactive(rv$deploy_refresh),
      rows_selected = reactive(input$collection_rows_selected),
      row = reactive(rv$collect_table_filter()$deployment_uid[input$collection_rows_selected]), 
      future_smp_id = reactive(rv$future_table_db$smp_id[input$future_rows_selected]), 
      future_site_name = reactive(rv$future_table_db$site_name[input$future_rows_selected]), 
      future_deploy_refresh = reactive(rv$future_deploy_refresh), 
      future_rows_selected = reactive(input$future_rows_selected), 
      future_row = reactive(rv$future_table_db$future_deployment_uid[input$future_rows_selected])
      )
    )
  
}