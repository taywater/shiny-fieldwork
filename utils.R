### This script is a centralized location for utility functions that will be
### used in multiple modules within the Fieldwork Application. It was created
### to contain the function used to write the log of INSERT and UPDATE queries
### sent to the MARS database created by Brian Cruice in 8/2024. It should be
### used as a place to store similar functions moving forward. Several functions
### originally housed in app.R have been moved to this page

#Not in logical
`%!in%` <- Negate(`%in%`)

#replace special characters with friendlier characters
special_char_replace <- function(note){
  
  note_fix <- note %>% 
    str_replace_all(c("•" = "-", "ï‚§" = "-", "“" = '"', '”' = '"'))
  
  return(note_fix)
  
}


#function to highlight datatables based on NAs/blanks
newstyleEqual <- function (levels, values, default = NULL) 
{
  n = length(levels)
  if (n != length(values)) 
    stop("length(levels) must be equal to length(values)")
  if (!is.null(default) && (!is.character(default) || length(default) != 
                            1)) 
    stop("default must be null or a string")
  if (n == 0) 
    return("''")
  levels = DT:::jsValues(levels)
  values = DT:::jsValues(values)
  js = ""
  for (i in seq_len(n)) {
    if(levels[i]=="\"NA\""){ # needed because jsValues converts NA to a string
      js = paste0(js, sprintf("isNaN(parseFloat(value)) ? %s : ",
                              values[i]))
      
    }else{
      js = paste0(js, sprintf("value == %s ? %s : ", levels[i], 
                              values[i]))
    }
    
  }
  default = if (is.null(default)) 
    "value"
  else jsValues(default)
  DT::JS(paste0(js, default))
}


# function to send log of the INSERT/UPDATE queries run within the app
insert.query.log <- function(poolConn, input_query, tab_name, session){
  log_query <- paste0("INSERT INTO log.tbl_fieldwork_app_queries (username, query_text, tab, date_time_sent)
                       VALUES ('", session$user,
                      "', '",gsub(input_query,pattern="'",replacement="''"),
                      "', '", tab_name, "', '",
                      Sys.time(),"')")
  
  odbc::dbGetQuery(poolConn, log_query)
  
}

