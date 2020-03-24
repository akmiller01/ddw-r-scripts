library(openxlsx)
library(reshape)
library(utils)
library(data.table)


source("load_configs.R")
setwd(configs$wd)
source("connect.R")

wd <- configs$digital_platform
setwd(wd)
refMap <- list(
  "data_series.domestic"=list("reference.di_budget_type","reference.di_domestic_budget_level","reference.di_currency","dimension.domestic_metadata"),
  "data_series.domestic-sectors"=list("reference.di_budget_type","reference.di_domestic_budget_level","reference.di_currency","dimension.domestic_metadata"),
  "data_series.domestic-netlending"=list("reference.di_budget_type","reference.di_domestic_budget_level","reference.di_currency","dimension.domestic_metadata"),
  "data_series.intl-flows-donors"=list("reference.di_flow_type","reference.di_flow_name"),
  "data_series.intl-flows-recipients"=list("reference.di_flow_type","reference.di_flow_name"),
  "data_series.intl-flows-donors-wide"=list("reference.di_flow_type","reference.di_flow_name"),
  "data_series.intl-flows-recipients-wide"=list("reference.di_flow_type","reference.di_flow_name"),
  "data_series.largest-intl-flow"=list("reference.di_largest_intl_flow"),
  "data_series.fragile-states"=list("reference.di_fragile_state"),
  "data_series.long-term-debt"=list("reference.di_destination_institution_type","reference.di_financing_type"),
  "data_series.oda"=list("reference.di_sector","reference.di_oof_bundle","reference.di_channel"),
  "data_series.oof"=list("reference.di_sector","reference.di_oof_bundle","reference.di_channel"),
  "data_series.fdi-out"=list("reference.di_financing_type"),
  "data_series.dfis-out-dev"=list("reference.di_financing_type"),
  "data_series.ssc-out"=list("reference.di_financing_type")
)

excel_trunc = function(str){
  return(substr(str,1,31))
}

#Delete everything in user-data
unlink(dir(wd, full.names = TRUE),recursive=TRUE)

all.entities <- ddw("reference.di_entity")
all.entities <- all.entities[c("id","name")]
names(all.entities) <- c("di_id","entity_name")
entities = all.entities
all.entities.ug <- dbReadTable(con, c("spotlight_on_uganda_2017","ref_uganda_district"))
all.entities.ug <- all.entities.ug[c("id","name")]
names(all.entities.ug) <- c("district_id","district_name")
all.entities.ke <- dbReadTable(con, c("spotlight_on_kenya_2017","ref_kenya_district"))
all.entities.ke <- all.entities.ke[c("id","name")]
names(all.entities.ke) <- c("district_id","district_name")

userDat <- function(data,basename){
  #Read Data
  names <- colnames(data)
  formatted_basename = gsub("_","-",strsplit(basename,".",fixed=TRUE)[[1]][2],fixed=TRUE)
  fwd = paste0(wd,formatted_basename)
  
  #Add country names
  if("di_id" %in% names){
      data <- merge(
        all.entities
        ,data
        ,by=c("di_id")
        ,all.y=TRUE
      ) 
  }else if("district_id" %in% names){
    if(grepl("uganda",id)){
      data <- merge(
        all.entities.ug
        ,data
        ,by=c("district_id")
        ,all.y=TRUE
      ) 
    }else if(grepl("kenya",id)){
      data <- merge(
        all.entities.ke
        ,data
        ,by=c("district_id")
        ,all.y=TRUE
      ) 
    }
  }else{
    if("to_di_id" %in% names){
      names(entities) <- c("to_di_id","entity_to_name")
      data <- merge(
        entities
        ,data
        ,by=c("to_di_id")
        ,all.y=TRUE
      ) 
    }
    if("from_di_id" %in% names){
      names(entities) <- c("from_di_id","entity_from_name")
      data <- merge(
        entities
        ,data
        ,by=c("from_di_id")
        ,all.y=TRUE
      ) 
    }
  }
  
  #Try and sort by entity name, failing that: id, failing that: year, failing that, the first column.
  names <- colnames(data)
  if("entity_name" %in% names){
    if("year" %in% names){
      data <- data[order(data["entity_name"],data$year),]
    }else{
      data <- data[order(data["entity_name"]),]
    }
  }else if("district_name" %in% names){
    if("year" %in% names){
      data <- data[order(data["district_name"],data$year),]
    }else{
      data <- data[order(data["district_name"]),]
    }
  }else if("entity_to_name" %in% names){
    if("year" %in% names){
      data <- data[order(data["entity_to_name"],data$year),]
    }else{
      data <- data[order(data["entity_to_name"]),]
    }
  }else if("entity_from_name" %in% names){
    if("year" %in% names){
      data <- data[order(data["entity_from_name"],data$year),]
    }else{
      data <- data[order(data["entity_from_name"]),]
    }
  }else if("di_id" %in% names){
    if("year" %in% names){
      data <- data[order(data["di_id"],data$year),]
    }else{
      data <- data[order(data["di_id"]),]
    }
  }else if("district_id" %in% names){
    if("year" %in% names){
      data <- data[order(data["district_id"],data$year),]
    }else{
      data <- data[order(data["district_id"]),]
    }
  }else{
    if("year" %in% names){
      data <- data[data$year,]
    }else{
      data <- data[order(data[,1]),]
    }
  }
  
  #Create a folder for each indicator with sub-csv dir
  dir.create(fwd)
  setwd(fwd)
  cwd = paste(fwd,"csv",sep="/")
  dir.create(cwd)
  
  #Create workbook
  wb <- createWorkbook(formatted_basename)
  
  #Start notes sheet/csv
  concept = concepts[which(concepts$id==basename),]
  notesList <- c(
    paste("Name:",formatted_basename)
    ,paste("Description:",concept$description)
    ,paste("Units of measure:",concept$uom)
    ,paste("Source:",concept[,"source"])
    ,if(!is.na(concept[,"source_link"])) c(paste("Source-link:",concept[,"source_link"]),"") else ""
    ,"Notes:"
  )
  if("estimate" %in% names){
    notesList<-c(
      notesList
      ,"This data contains information that may be a projection. Projected datapoints are indicated by a value of TRUE in the 'estimate' column. The year at which projections begin varies from country to country."
      ,""
    )
  }
  if("value_ncu" %in% names){
    notesList<-c(
      notesList
      ,"This data contains information that has been converted from current native currency units (NCU) to constant US Dollars. The NCU values are contained in the 'value-ncu' column, while the converted and deflated values are contained in the 'value' column."
      ,""
    )
  }
  addWorksheet(wb,"Notes")
  
  #Copy the data
  write.csv(data,paste0(cwd,"/",formatted_basename,".csv"),row.names=FALSE,na="")
  addWorksheet(wb,"Data")
  writeData(wb,sheet="Data",data,colNames=TRUE,rowNames=FALSE)    
  
  #If we have an ID, a year to widen it by and it's simple, provide wide
  if("di_id" %in% names & "year" %in% names & "value" %in% names)  {
    if("entity_name" %in% names){
      wdata <- reshape(data[c("di_id","entity_name","year","value")],idvar=c("di_id","entity_name"),timevar="year",direction="wide")
    }else{
      wdata <- reshape(data[c("di_id","year","value")],idvar=c("di_id"),timevar="year",direction="wide")
    }
    wnames <- names(wdata)
    for(j in 1:length(wnames)){
      wname = wnames[j]
      if(substr(wname,1,5)=="value"){
        names(wdata)[names(wdata) == wname] <- substr(wname,7,nchar(wname))
      }
    }
    notesList<-c(
      notesList
      ,"On the 'Data-wide-value' sheet, we have provided the indicator in a wide format. The values you see listed there are from the 'value' column."
      ,""
    )
    addWorksheet(wb,"Data-wide-value")
    writeData(wb,sheet="Data-wide-value",wdata,colNames=TRUE,rowNames=FALSE)  
    write.csv(wdata,paste(cwd,"/",formatted_basename,"-wide-value",".csv",sep=""),row.names=FALSE,na="")
  }
  if("district_id" %in% names & "year" %in% names & "value" %in% names)  {
    if("entity_name" %in% names){
      wdata <- reshape(data[c("district_id","entity_name","year","value")],idvar=c("district_id","entity_name"),timevar="year",direction="wide")
    }else{
      wdata <- reshape(data[c("district_id","year","value")],idvar=c("district_id"),timevar="year",direction="wide")
    }
    wnames <- names(wdata)
    for(j in 1:length(wnames)){
      wname = wnames[j]
      if(substr(wname,1,5)=="value"){
        names(wdata)[names(wdata) == wname] <- substr(wname,7,nchar(wname))
      }
    }
    notesList<-c(
      notesList
      ,"On the 'Data-wide-value' sheet, we have provided the indicator in a wide format. The values you see listed there are from the 'value' column."
      ,""
    )
    addWorksheet(wb,"Data-wide-value")
    writeData(wb,sheet="Data-wide-value",wdata,colNames=TRUE,rowNames=FALSE)  
    write.csv(wdata,paste(cwd,"/",formatted_basename,"-wide-value",".csv",sep=""),row.names=FALSE,na="")
  }
  #Wide for original-value
  if("di_id" %in% names & "year" %in% names & "original_value" %in% names)  {
    if("entity-name" %in% names){
      wdata <- reshape(data[c("di_id","entity_name","year","original_value")],idvar=c("di_id","entity_name"),timevar="year",direction="wide")
    }else{
      wdata <- reshape(data[c("di_id","year","original_value")],idvar=c("di_id"),timevar="year",direction="wide")
    }
    wnames <- names(wdata)
    for(j in 1:length(wnames)){
      wname = wnames[j]
      if(substr(wname,1,14)=="original_value"){
        names(wdata)[names(wdata) == wname] <- substr(wname,16,nchar(wname))
      }
    }
    notesList<-c(
      notesList
      ,"On the 'Data-wide-original-value' sheet, we have provided the indicator in a wide format. The values you see listed there are from the 'original-value' column."
      ,""
    )
    addWorksheet(wb,"Data-wide-original-value")
    writeData(wb,sheet="Data-wide-original-value",wdata,colNames=TRUE,rowNames=FALSE)  
    write.csv(wdata,paste(cwd,"/",formatted_basename,"-wide-original-value",".csv",sep=""),row.names=FALSE,na="")
  }
  
  #Reference
  #Copy entity.csv
  if(grepl("uganda",id)){
    write.csv(all.entities.ug,paste(cwd,"ug_entity.csv",sep="/"),row.names=FALSE,na='') 
  }else if(grepl("kenya",id)){
    write.csv(all.entities.ke,paste(cwd,"ke_entity.csv",sep="/"),row.names=FALSE,na='') 
  }else{
    write.csv(all.entities,paste(cwd,"entity.csv",sep="/"),row.names=FALSE,na='') 
  }
  if(basename %in% names(refMap)){
    refNames = refMap[[basename]]
    notesList<-c(
      notesList
      ,"The following tabs have been included for reference purposes:"
      ,paste(refNames,collapse=", ")
      ,""
    )
    for(j in 1:length(refNames)){
      refBaseName = refNames[j][[1]]
      #Copy the reference files
      try({
        refData <- ddw(refBaseName)
        if(nrow(refData)>0){
          write.csv(refData,paste0(cwd,"/",refBaseName,".csv"),row.names=FALSE,na="")
          addWorksheet(wb,refBaseName)
          writeData(wb,sheet=excel_trunc(refBaseName),refData,colNames=TRUE,rowNames=FALSE) 
        }
      })
    }
  }
  
  #Cap off notes sheet
  notesList<-c(
    notesList
    ,""
    ,""
    ,"The following is data downloaded from Development Initiative's Datahub: http://devinit.org/data"
    ,"It is licensed under a Creative Commons Attribution 4.0 International license."
    ,"More information on licensing is available here: https://creativecommons.org/licenses/by/4.0/"
    ,"For concerns, questions, or corrections: please email info@devinit.org"
    ,"Copyright Development Initiatives Poverty Research Ltd. 2018"
  )
  notesDf <- data.frame(notesList)
  writeData(wb,sheet="Notes",notesDf,colNames=FALSE,rowNames=FALSE)  
  write.table(notesDf,paste0(cwd,"/",formatted_basename,"-notes",".csv"),col.names=FALSE,row.names=FALSE,na="",sep=",")
  saveWorkbook(wb, paste0(formatted_basename,".xlsx"), overwrite = TRUE)
  
  #Go back to user-data folder
  setwd(wd)
}

concepts1 <- read.csv(configs$concepts1)
concepts2 <- read.csv(configs$concepts2)
concepts3 <- read.csv(configs$concepts3)
concepts4 <- read.csv(configs$concepts4)
concepts5 <- read.csv(configs$concepts5)
concepts6 <- read.csv(configs$concepts6)
concepts7 <- read.csv(configs$concepts7)
concepts8 <- read.csv(configs$concepts8)
concepts9 <- read.csv(configs$concepts9)

concepts.list = list(
  concepts1,
  concepts2,
  concepts3,
  concepts4,
  concepts5,
  concepts6,
  concepts7,
  concepts8,
  concepts9
)
                       
concepts = rbindlist(concepts.list,fill=T)
errors.list = c()
for(id in unique(concepts$id)){
  message(id)
  tryCatch({
    schema = strsplit(id,split=".",fixed=T)[[1]][1]
    table = strsplit(id,split=".",fixed=T)[[1]][2]
    dat <- dbReadTable(con,c(schema,table))
    if(nrow(dat)>0){
      userDat(dat,id)
    }
  },error=function(e){
    message(e)
    errors.list = c(errors.list,id)
    })
  
}
message(errors.list)
dbDisconnect(con)

#Zip em up
filenames <- list.files(wd, pattern="/*", full.names=FALSE)
for(i in 1:length(filenames)){
  files <- dir(filenames[i],full.names=TRUE)
  zip(zipfile = filenames[i],files=files)
}
