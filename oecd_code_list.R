library('XML')

m_url <- 'https://webfs.oecd.org/crs-iati-xml/Lookup/DAC-CRS-CODES.xml'

con <- url(m_url)
codes <- readLines(con)
close(con)

codes <- as.character(codes)

#Parse XML to list
xmldata <- xmlToList(codes)

#There is a listof code lists, extract each in a loop or write a function and pass to apply
#Working with one just to get content
xmldata_codelist <- as.list(xmldata[[1]])

#Each code list has a list of items, get the items
code_list_items <- as.list(xmldata_codelist$`codelist-items`)

#Loop through each code list item and get the data details
getcodelistitems <- function(codelistitem){
  item <- c(
    codelistitem$code,
    codelistitem$name$narrative,
    codelistitem$category,
    #codelistitem$description$reference,
    codelistitem$description$narrative
    )
  
}

for(i in 1:length(xmldata)){
  
  xmldata_codelist <- as.list(xmldata[[i]])
  name <- xmldata_codelist$metadata$name$narrative
  data <- matrix(ncol = 4,nrow = 0)
  
  code_list_items <- as.list(xmldata_codelist$`codelist-items`)
  
  for(k in 1:length(code_list_items)){
    item <- getcodelistitems(code_list_items[[k]])
    
    data <- mapply(c, data,item)
  #  rbind(data,item)
  }
  
  assign(name,data.frame(data))
}


xmldata$metadata$name[[1]]

codelist <- as.list(xmldata$`codelist-items`)

codeListNames <- function(xml){
  #Iterate over this list and pick all content
  xmldata <- as.list(xmldata[[1]])
  codelist <- as.list(xmldata$`codelist-items`)
  narrative <- xmldata[[1]]$metadata$narrative
}

paste0('value') <- 'test'

print(sapply(xmldata,codeListNames))