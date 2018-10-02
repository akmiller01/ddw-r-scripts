list.of.packages <- c("data.table","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


#Load dac2a mirror data from source data
#Path to row downloaded data 
#Make sure all files are already extracted

dac2a_path <- "/Users/boss/Dev_Musings/devinit/ddw_update/data_source/oecd_dac_table_2a/2018_07_30/Table2a_Data.csv"
dac2b_path <- "/Users/boss/Dev_Musings/devinit/ddw_update/data_source/oecd_dac_table_2b/2018_07_30/Table2b_Data.csv"
dac5_path <- "/Users/boss/Dev_Musings/devinit/ddw_update/data_source/oecd_dac_table_5/2018_07_30/Table5_Data.csv"
dac1_path <- "/Users/boss/Dev_Musings/devinit/ddw_update/data_source/oecd_dac_table_1/2018_09_11/Table1_Data.csv"

#This cannot point to a file, we will be working with several files
#Checking header lengths, invalid characters and missing data content

crs_path <- "/Users/boss/Dev_Musings/devinit/ddw_update/data_source/oecd_crs/2018_07_30"

clean_dac2a_file = function(){
  
  dac2a <- fread(dac2a_path)
  setnames(dac2a,
           c('RECIPIENT',
                   'Recipient_',
                   'DONOR',
                   'Donor_',
                    'PART',
                   'Part_',
                   'AIDTYPE',
                   'Aid_type_',
                   'DATATYPE',
                   'Amount_type',
                   'TIME',
                   'Year',
                   'Value',
                   'Flags')
                   ,
           c('recipient_code',
             'recipient_name',
             'donor_code',
             'donor_name',
             'part_code',
             'part_name',
             'aid_type_code',
             'aid_type_name',
             'data_type',
             'amount_type',
             'time',
             'year',
             'value',
             'flags'
             ));
  
  if(!dir.exists('mirrors')){
    dir.create('mirrors')
  }

  fwrite(dac2a,file = 'mirrors/dac2a.csv',append = FALSE);
  rm(dac2a);
  
}

clean_dac2b_file = function(){
  
  dac2b <- fread(dac2b_path)
  setnames(dac2b,
           c('RECIPIENT'
             ,'Recipient_'
             ,'DONOR'
             ,'Donor_'
             ,'PART'
             ,'Part_'
             ,'AIDTYPE'
             ,'Aidtype_'
             ,'DATATYPE'
             ,'Amounttype_'
             ,'TIME'
             ,'Year'
             ,'Value'
             ,'Flags')
           ,
           c('recipient_code',
             'recipient_name',
             'donor_code',
             'donor_name',
             'part_code',
             'part_name',
             'aid_type_code',
             'aid_type_name',
             'data_type',
             'amount_type',
             'time',
             'year',
             'value',
             'flags'
           ));
  
  if(!dir.exists('mirrors')){
    dir.create('mirrors')
  }
  
  fwrite(dac2b,file = 'mirrors/dac2b.csv',append = FALSE);
  
}

clean_dac5_file = function(){
  dac5 <- fread(dac5_path)
  setnames(dac5,
           c('DONOR'
             ,'Donor_'
             ,'SECTOR'
             ,'Sector_'
             ,'AIDTYPE'
             ,'Aidtype_'
             ,'AMOUNTTYPE'
             ,'Amounttype_'
             ,'TIME'
             ,'Year'
             ,'Value'
             ,'Flags')
           ,
           c('donor_code'
             ,'donor_name'
             ,'sector_code'
             ,'sector_name'
             ,'aid_type_code'
             ,'aid_type_name'
             ,'amount_type_code'
             ,'amount_type_name'
             ,'time'
             ,'year'
             ,'value'
             ,'flags'
           ));
  
  if(!dir.exists('mirrors')){
    dir.create('mirrors')
  }
  
  fwrite(dac5,file = 'mirrors/dac5.csv',append = FALSE);

}


clean_dac1_file = function(){
  dac1 <- fread(dac1_path)
  setnames(dac1,
           c('DONOR'
             ,'Donor'
             ,'PART'
             ,'Part'
             ,'AIDTYPE'
             ,'Aid type'
             ,'FLOWS'
             ,'Fund flows'
             ,'AMOUNTTYPE'
             ,'Amount type'
             ,'TIME'
             ,'Year'
             ,'Value'
             ,'Flags')
           ,
           c('donor_code'
             ,'donor_name'
             ,'part_code'
             ,'part_name'
             ,'aid_type_code'
             ,'aid_type_name'
             ,'flows'
             ,'fund_flows'
             ,'amount_type_code'
             ,'amount_type_name'
             ,'time'
             ,'year'
             ,'value'
             ,'flags'
           ));
  
  if(!dir.exists('mirrors')){
    dir.create('mirrors')
  }
  
  fwrite(dac1,file = 'mirrors/dac1.csv',append = FALSE);
}

#This can be done better, read actual files and use ncol before doing processing, that way
#You wont need this method
check_crs_headers = function(file_v){
  
  expected_2016_length <- 86
  
  #Check that the length of the total number of columns match what is expected
  #If column lengths don't match, do a manual visual check to see what has been added or removed
  #All columns need to have the same lenghth for all files of CRS because each change affects all years
  for(fi in file_v){
    
    fo <- file(fi,"r")
    first_line <- readLines(fo,n = 1)
    split_vec <- strsplit(first_line,"|",fixed=TRUE)[[1]]
    
    if((length(split_vec) < expected_2016_length) || (length(split_vec) > expected_2016_length)){
      e <- simpleError(sprintf("Found length %f for file %s but expecting %f ",length(split_vec),fi,expected_2016_length))
      stop(e)
    }
  }
  
 
}

merge_crs_tables = function(file_vec){
  
  crs_tmp_file <- "crs_merge.csv"
  file_header_path <- file_vec[2]
  #create file and add file header content to it
  file_header <- file(file_header_path,"r")
  if(!dir.exists('crs_cleanup')){
    dir.create('crs_cleanup')
  }
  
  header_content <- readLines(file_header,n = 1);
  output_file_name <- paste("crs_cleanup",crs_tmp_file,sep = "/");
  
  write.table(header_content,file = output_file_name ,sep="|");
  
  #Clean up intermiidiate files because CRS is abit big
  close(file_header)
  rm(file_header_path)
  
  for(fi in file_vec){
    
    #file_con = file(fi);
    
    crs_dt <- read_delim(fi,"|",col_types=cols(.default=col_character()))
    
    problems(crs_dt)
 
   # close(file_con)
    crs_dt <- data.table(crs_dt)
    
    write.table(crs_dt,file=output_file_name,append=TRUE,sep="|",na="",col.names = FALSE)
  }
  
}



test_merge = function(file_vec){
  
  crs_tmp_file <- "crs_merge.csv"
  file_header_path <- file_vec[2]
  #create file and add file header content to it
  file_header <- file(file_header_path,"r")
  if(!dir.exists('crs_cleanup')){
    dir.create('crs_cleanup')
  }
  
  header_content <- readLines(file_header,n = 1);
  output_file_name <- paste("crs_cleanup",crs_tmp_file,sep = "/");
  
  write.table(header_content,file = output_file_name ,sep="|");
  
  #Clean up intermiidiate files because CRS is abit big
  close(file_header)
  rm(file_header_path)
  
  for(fi in file_vec){
    
    #file_con = file(fi);
    ?
    crs_dt <- read_delim(fi,"|",col_types=cols(.default=col_character()))
    
    problems(crs_dt)
    
    # close(file_con)
    crs_dt <- data.table(crs_dt)
    
    write.table(crs_dt,file=output_file_name,append=TRUE,sep="|",na="",col.names = FALSE)
  }
  
}


load_crs_file = function(){
  file_list <- list.files(path = crs_path,pattern = "*.txt",full.names = TRUE,recursive = FALSE);
  
  check_crs_headers(file_list)
  merge_crs_tables(file_list);
  
 # lapply()
  
 # fwrite(dac1,file = 'mirrors/dac1.csv',append = FALSE);
}

load_crs_file()


clean_dac2a_file()
clean_dac1_file()
clean_dac2b_file()
clean_dac5_file()