library('XML')

m_url <- 'https://webfs.oecd.org/crs-iati-xml/Lookup/DAC-CRS-CODES.xml'

con <- url(m_url)
codes <- readLines(con)
close(con)

codes <- as.character(codes)
section_names <- list('Aid Type',
                  'Aid Type (category)',
                  'Channel category',
                  'Channel of delivery',
                  'Collaboration Type',
                  'Finance Type',
                  'Finance Type (category)',
                  'Flow Type',
                  'Income group',
                  'Income group category',
                  'DAC 5 Digit Sector',
                  'DAC 3 Digit Sector')


sections <- list('aid_type'=1,
                 'aid_type_category'=2,
                 'channel_category'=3,
                 'channel_of_delivery'=4,
                 'collaboration_type'=5,
                 'finance_type'=6,
                 'finance_category'=7,
                 'flow_type'=8,
                 'income_group'=9,
                 'income_group_category'=10,
                 'dac5_sector'=11,
                 'dac3_sector'=12)

#Parse XML to list
xmldata <- xmlToList(codes)

# First check if the indices match the names in the xml sheet, if they don't,
# reorganize the sections list and assign the correct index to the xml
for(i in 1:length(xmldata)){
  #Print the names of the sections available
  print(paste0(i,' - ',xmldata[[i]]$metadata$name))
  
  #Check if the name index matches the index in sector_names
  stopifnot(xmldata[[i]]$metadata$name == section_names[[i]])
}

# Process Aid Type

#There is a listof code lists, extract each in a loop or write a function and pass to apply
#Working with one just to get content
aid_type_list <- as.list(xmldata[[sections$aid_type]])
aid_type_list_items <- aid_type_list$`codelist-items`
#Each code list has a list of items, get the items

#create an object for the aid_type_lists
aid_type_list_data <- matrix(nrow=length(aid_type_list_items),ncol=4)

#Columns
# Type, Subtype,Description,Clarification
for(i in 1:length(aid_type_list_items)){

  aid_type_list_data[i,1] <- aid_type_list_items[[i]]$category
  aid_type_list_data[i,2] <- aid_type_list_items[[i]]$code
  aid_type_list_data[i,3] <- aid_type_list_items[[i]]$name$narrative
  aid_type_list_data[i,4] <- aid_type_list_items[[i]]$description$narrative

}

aid_type <- data.frame(aid_type_list_data)
setnames(aid_type_list_data,paste0('X',1:4),c('Type', 'Subtype','Description','Clarification'))
rm(aid_type_list,aid_type_list_items,codelist_item_on_1,aid_type_list_data)


# Process Aid Type (category)

aid_type_category <- as.list(xmldata[[sections$aid_type_category]])
aid_type_category <- aid_type_category$`codelist-items`
aid_type_category_m <- matrix(nrow=length(aid_type_category),ncol=2)

for(i in 1:length(aid_type_category)){
  
  aid_type_category_m[i,1] <- aid_type_category[[i]]$code
  aid_type_category_m[i,2] <- aid_type_category[[i]]$name$narrative

}

aid_type_category <- data.frame(aid_type_category_m)
setnames(aid_type_category,paste0('X',1:2),c('Category', 'Name'))
rm(aid_type_category_m)

#Process Channel category

channel_category <- as.list(xmldata[[sections$channel_category]])
channel_category <- channel_category$`codelist-items`
channel_category_m <- matrix(nrow=length(channel_category),ncol=3)

# List of headings Category_ID, channel_name

for(i in 1:length(channel_category)){
  channel_category_m[i,1] <- channel_category[[i]]$code
  channel_category_m[i,2] <- channel_category[[i]]$name$narrative
  channel_category_m[i,3] <- channel_category[[i]]$.attrs[[1]]
}

channel_category <- data.frame(channel_category_m)
setnames(channel_category,paste0('X',1:3),c('Category_ID', 'Name','Status'))
rm(channel_category_m)


#Process channel_of_delivery

channel_of_delivery <- as.list(xmldata[[sections$channel_of_delivery]])
channel_of_delivery <- channel_of_delivery$`codelist-items`
channel_of_delivery_m <- matrix(nrow=length(channel_of_delivery),ncol=6)

# -- Channel_Category ($category), Channel_ID(code), Full_Name (name$narrative),
#  Reference (reference),Status,Activation,Withdrawal

for(i in 1:length(channel_of_delivery)){
    channel_of_delivery_m[i,1] <- channel_of_delivery[[i]]$category
    channel_of_delivery_m[i,2] <- channel_of_delivery[[i]]$code
    channel_of_delivery_m[i,3] <- channel_of_delivery[[i]]$name$narrative
    channel_of_delivery_m[i,4] <- channel_of_delivery[[i]]$.attrs[[1]]
    channel_of_delivery_m[i,5] <- channel_of_delivery[[i]]$.attrs[[2]]
    channel_of_delivery_m[i,6] <- channel_of_delivery[[i]]$.attrs[[3]]
   
}

channel_of_delivery <- data.frame(channel_of_delivery_m)
setnames(channel_of_delivery,paste0('X',1:6),c('Channel_Category', 'Channel_ID','Full_Name','Status','Activation','Withdrawal'))
rm(channel_of_delivery_m)


# Process Collaboration Type

collaboration_type <- as.list(xmldata[[sections$collaboration_type]])
collaboration_type <- collaboration_type$`codelist-items`
collaboration_type_m <- matrix(nrow=length(collaboration_type),ncol=2)
  
for(i in 1:length(collaboration_type)){
  
  collaboration_type_m[i,1] <- collaboration_type[[i]]$code
  collaboration_type_m[i,2] <- collaboration_type[[i]]$name$narrative
}

collaboration_type <- data.frame(collaboration_type_m)
setnames(collaboration_type,paste0('X',1:2),c('Code', 'Name'))
rm(collaboration_type_m)

# Process Finance Type

finance_type <- as.list(xmldata[[sections$finance_type]])
finance_type <- finance_type$`codelist-items`
finance_type_m <- matrix(nrow=length(finance_type),ncol=4)

for(i in 1:length(finance_type)){
  
  finance_type_m[i,1] <- finance_type[[i]]$category
  finance_type_m[i,2] <- finance_type[[i]]$code
  finance_type_m[i,3] <- finance_type[[i]]$name$narrative
  finance_type_m[i,4] <- finance_type[[i]]$.attrs[[1]]
}

finance_type <- data.frame(finance_type_m)
setnames(finance_type,paste0('X',1:4),c('Finance_Category','Code', 'Name','Status'))
rm(finance_type_m)

# Process Finance Category

# Process Finance Type

finance_category <- as.list(xmldata[[sections$finance_category]])
finance_category <- finance_category$`codelist-items`
finance_category_m <- matrix(nrow=length(finance_category),ncol=3)

for(i in 1:length(finance_category)){
  
  finance_category_m[i,1] <- finance_category[[i]]$code
  finance_category_m[i,2] <- finance_category[[i]]$name$narrative
  finance_category_m[i,3] <- finance_category[[i]]$.attrs[[1]]
}

finance_category <- data.frame(finance_category_m)
setnames(finance_category,paste0('X',1:3),c('Finance_Category','Category_Name', 'Status'))
rm(finance_category_m)

#Process Flow type

flow_type <- as.list(xmldata[[sections$flow_type]])
flow_type <- flow_type$`codelist-items`
flow_type_m <- matrix(nrow=length(flow_type),ncol=4)

for(i in 1:length(flow_type)){
  
  flow_type_m[i,1] <- flow_type[[i]]$code
  flow_type_m[i,2] <- flow_type[[i]]$name$narrative
  flow_type_m[i,3] <- flow_type[[i]]$description$narrative
  flow_type_m[i,4] <- flow_type[[i]]$.attrs[[1]]
}

flow_type <- data.frame(flow_type_m)
setnames(flow_type,paste0('X',1:4),c('Flow_Code','Flow_Name','Short_Name', 'Status'))
rm(flow_type_m)

# Process Income Group

income_group <- as.list(xmldata[[sections$income_group]])
income_group <- income_group$`codelist-items`
income_group_m <- matrix(nrow=length(flow_type),ncol=5)

for(i in 1:length(income_group)){

  income_group_m[i,1] <- income_group[[i]]$category
  income_group_m[i,2] <- income_group[[i]]$code
  income_group_m[i,3] <- income_group[[i]]$name$narrative
  income_group_m[i,4] <- income_group[[i]]$.attrs[1]
  income_group_m[i,5] <- income_group[[i]]$.attrs[2]
}

income_group <- data.frame(income_group_m)
setnames(income_group,paste0('X',1:5),c('Category_Code','Code','Name','Status', 'Activation'))
rm(income_group_m)

# Process Income Group Category

income_group_category <- as.list(xmldata[[sections$income_group_category]])
income_group_category <- income_group_category$`codelist-items`
income_group_category_m <- matrix(nrow=length(income_group_category),ncol=3)

for(i in 1:length(income_group_category)){
  
  income_group_category_m[i,1] <- income_group_category[[i]]$code
  income_group_category_m[i,2] <- income_group_category[[i]]$name$narrative
  income_group_category_m[i,3] <- income_group_category[[i]]$.attrs[1]
  
}

income_group_category <- data.frame(income_group_category_m)
setnames(income_group_category,paste0('X',1:3),c('Code','Name','Status'))
rm(income_group_category_m)

# Process DAC 5 Sectors

dac_5_sectors <- as.list(xmldata[[sections$dac5_sector]])
dac_5_sectors <- dac_5_sectors$`codelist-items`
dac_5_sectors_m <- matrix(nrow=length(dac_5_sectors),ncol=5)

for(i in 1:length(dac_5_sectors)){

  dac_5_sectors_m[i,1] <- dac_5_sectors[[i]]$category
  dac_5_sectors_m[i,2] <- dac_5_sectors[[i]]$code
  dac_5_sectors_m[i,3] <- dac_5_sectors[[i]]$name$narrative
  dac_5_sectors_m[i,4] <- ifelse(is.null(dac_5_sectors[[i]]$description$narrative),NA,dac_5_sectors[[i]]$description$narrative)
  dac_5_sectors_m[i,5] <- dac_5_sectors[[i]]$.attrs[[1]]
  
}

dac_5_sectors <- data.frame(dac_5_sectors_m)
setnames(dac_5_sectors,paste0('X',1:5),c('Category','Code','Name','Description','Status'))
rm(dac_5_sectors_m)


#Process DAC 3
dac_3_sectors <- as.list(xmldata[[sections$dac3_sector]])
dac_3_sectors <- dac_3_sectors$`codelist-items`
dac_3_sectors_m <- matrix(nrow=length(dac_3_sectors),ncol=3)

for(i in 1:length(dac_3_sectors)){
  
  dac_3_sectors_m[i,1] <- dac_3_sectors[[i]]$code
  dac_3_sectors_m[i,2] <- dac_3_sectors[[i]]$name$narrative
  dac_3_sectors_m[i,3] <- ifelse(is.null(dac_3_sectors[[i]]$description$narrative),NA,dac_3_sectors[[i]]$description$narrative)
  
}

dac_3_sectors <- data.frame(dac_3_sectors_m)
setnames(dac_3_sectors,paste0('X',1:3),c('Code','Name','Description'))
rm(dac_3_sectors_m)