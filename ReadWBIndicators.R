
#Replacement for WDIData github library
options(digits = 17)
getWDIData <- function(indicator,date){
  
  pagingQuery <- paste('http://api.worldbank.org/v2/countries/all/indicators/',as.character(indicator),
                       '?date=',as.character(date),'&format=json',sep="");
 
  initialQuery <- fromJSON(pagingQuery)
  initialQuery <- initialQuery[[1]]
  
  message(paste0('Fetching indicator ',indicator,': Total Items ',initialQuery$total,' Total Pages: ', initialQuery$pages))
  
  completeDataSet <- matrix(ncol = 4,nrow = 0);
  
  # Iterate through all pages of indicator
  for(page in 1:initialQuery$pages){
    
    normalQuery <- paste0('http://api.worldbank.org/v2/countries/all/indicators/',indicator,
                          '?date=',date,'&page=',page,'&format=json');
    
    dataSet <- fromJSON(normalQuery)[[2]]
  
    #For each page get all the 5 values needed
    for(iterator in 1:length(dataSet)){
      v <- c(dataSet[[iterator]]$country['id'] %>% unname,
               dataSet[[iterator]]$countryiso3code,
           dataSet[[iterator]]$date,
        if(is.null(dataSet[[iterator]]$value)) NA else dataSet[[iterator]]$value)
      
      completeDataSet <- rbind(completeDataSet,v)
    }
    
  }
  
    completeDataSet <- completeDataSet  %>% as.data.frame
    
    setnames(completeDataSet,c(paste0('V',1:4)),c('iso2c','iso3c','year','value'))
    
    factorToValue <- function(val){
      as.numeric(as.character(val))
    }
    
   completeDataSet$year <- sapply(completeDataSet[,3],factorToValue)
   completeDataSet$value <- sapply(completeDataSet[,4],factorToValue)
  
  return(completeDataSet)
  
  
}

# Call to API
#myData <- getWDIData(indicator = 'NY.GDP.MKTP.CD',date= '2017:2017')
 

