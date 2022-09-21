#Current and Past Monitored Sites Module (Monitoring History -> History
#Show all monitored (CWL) sites, flicked into current and past. 
#When you click a row, it takes you to the deployment page for that SMP ID

#1.0 UI -------
  historyUI <- function(id, label = "history"){
    ns <- NS(id)
    navbarMenu("History",
      tabPanel("Current Monitoring Sites", value = "current_tab", 
               titlePanel("Current Continuous Water Level Monitoring Sites"), 
               DTOutput(ns("active_table")), 
    ), 
     tabPanel("Past Monitoring Sites", value = "past_tab", 
              titlePanel("Past Continuous Water Level Monitoring Sites"), 
              DTOutput(ns("past_table"))
     )
    )
  }

#2.0 server ---------
  cwl_historyServer <- function(id, parent_session, poolConn, deploy) {
    moduleServer(
      id,
      function(input, output, session){
    
        #initialize reactiveValues
        rv <- reactiveValues()
        
        #2.1 active table ----
          active_cwl_sites_query <- "select * from fieldwork.viw_active_cwl_sites order by smp_id, site_name"
          
          rv$active_cwl_sites_db <- reactive(dbGetQuery(poolConn, active_cwl_sites_query))
          
          #adjust active table for user experience
          rv$active_cwl_sites <- reactive(rv$active_cwl_sites_db() %>% 
                                            mutate_at("first_deployment_date", as.character) %>% 
                                            mutate_at(vars(one_of("public")),
                                                           funs(case_when(. == 1 ~ "Yes", 
                                                                          . == 0 ~ "No"))) %>% 
                                            dplyr::select("smp_id", "project_name", "component_id", "location_type", 
                                                          "type", "public", "first_deployment_date") %>% 
                                            dplyr::rename("SMP ID" = "smp_id", "Project Name" = "project_name", 
                                                          "Component ID" = "component_id", "Location Type" = "location_type", 
                                                          "Sensor Type" = "type",
                                                          "Public" = "public", "First Deployment Date" = "first_deployment_date"))
          
          #render active table
          output$active_table <- renderDT(
              rv$active_cwl_sites(),
              selection = 'single', 
              style = 'bootstrap',
              class = 'table-responsive, table-hover', 
              rownames = FALSE
          )
        
        #2.2 past table
          past_cwl_sites_query <- "select * from fieldwork.viw_previous_cwl_sites order by smp_id, site_name"
          
          rv$past_cwl_sites_db <- reactive(dbGetQuery(poolConn, past_cwl_sites_query))
          
          rv$past_cwl_sites <- reactive(rv$past_cwl_sites_db() %>% 
                                            mutate_at(c("first_deployment_date", "last_collection_date"), as.character) %>% 
                                            mutate_at(vars(one_of("public")),
                                                      funs(case_when(. == 1 ~ "Yes", 
                                                                     . == 0 ~ "No"))) %>% 
                                          dplyr::select("smp_id", "project_name", "component_id", "location_type", 
                                                        "type", "public", "first_deployment_date", "last_collection_date") %>% 
                                            dplyr::rename("SMP ID" = "smp_id", "Project Name" = "project_name", 
                                                          "Component ID" = "component_id", "Location Type" = "location_type", 
                                                          "Sensor Type" = "type",
                                                          "Public" = "public", "First Deployment Date" = "first_deployment_date",
                                                          "Last Collection Date" = "last_collection_date"))
          
          #render past table
          output$past_table <- renderDT(
            rv$past_cwl_sites(),
            selection = 'single', 
            style = 'bootstrap',
            class = 'table-responsive, table-hover', 
            rownames = FALSE
          )
        
        
        #2.3 Interact with other modules ----
          #refresh tickers to let other modules know when rows are clicked here
          rv$active_deploy_refresh <- 0
          rv$past_deploy_refresh <- 0
          
          #this is a two-step observeEvent
          #when a line in the table is clicked, update the refresh and switch tabs
          observeEvent(input$active_table_rows_selected, {
            rv$active_deploy_refresh <- rv$active_deploy_refresh + 1
            updateTabsetPanel(session = parent_session, "inTabset", selected = "deploy_tab")
          })
          
          observeEvent(input$past_table_rows_selected, {
            rv$past_deploy_refresh <- rv$past_deploy_refresh + 1
            updateTabsetPanel(session = parent_session, "inTabset", selected = "deploy_tab")
          })
          
          #upon adding or editing a deployment in deploy
          observeEvent(deploy$refresh_collect(),{
            rv$active_cwl_sites_db <- reactive(dbGetQuery(poolConn, active_cwl_sites_query))
            rv$past_cwl_sites_db <- reactive(dbGetQuery(poolConn, past_cwl_sites_query))
          })
          
          #return values to pass to other modules
          return(
            list(
              active_smp_id = reactive(rv$active_cwl_sites_db()$smp_id[input$active_table_rows_selected]),
              active_site_name = reactive(rv$active_cwl_sites_db()$site_name[input$active_table_rows_selected]),
              active_deploy_refresh = reactive(rv$active_deploy_refresh),
              past_smp_id = reactive(rv$past_cwl_sites_db()$smp_id[input$past_table_rows_selected]), 
              past_site_name = reactive(rv$past_cwl_sites_db()$site_name[input$past_table_rows_selected]), 
              past_deploy_refresh = reactive(rv$past_deploy_refresh)
            )
          )
      }
    )
    
  }