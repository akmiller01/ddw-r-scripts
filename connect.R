list.of.packages <- c("RPostgreSQL")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(RPostgreSQL)

cred <- readLines("ddw.txt")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv
                 ,dbname=cred[5]
                 ,host = cred[2]
                 ,port = as.numeric(cred[3])
                 ,user = cred[1]
                 ,password = cred[4]
                 )

# tables <- dbListTables(con)

schemaListTables <- function(schema){
  query <- paste0("SELECT table_name FROM information_schema.tables
  WHERE table_schema='",schema,"'")
  return(dbGetQuery(con,query)$table_name)
}

# facts <- schemaListTables("fact")

ddw <- function(schemaTable){
  split <- strsplit(schemaTable,".",fixed=TRUE)[[1]]
  if(length(split)==2){
    schema <- split[1]
    table <- split[2]
    schemaTables <- schemaListTables(schema)
    if(table %in% schemaTables){
      return(dbReadTable(con,table))
    }else{
      return(data.frame())
    }
  }else{
    allTables <- dbListTables(con)
    if(schemaTable %in% allTables){
      return(dbReadTable(con,schemaTable))
    }else{
      return(data.frame())
    }
  }
}

rm(cred,drv,list.of.packages,new.packages)

# dat <- dlTableIfExists(con,"fact.population_total")
# dbDisconnect(con)