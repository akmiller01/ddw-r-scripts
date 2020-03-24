library(data.table)

source("load_configs.R")
setwd(configs$wd)
source("connect.R")

setwd(configs$download_folder)

all.entities <- ddw("reference.di_entity")
all.entities <- all.entities[c("id","name")]
names(all.entities) <- c("di_id","entity_name")

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

mr.list = list()

concepts = rbindlist(concepts.list,fill=T)
for(id in unique(concepts$id)){
  message(id)
  tryCatch({
    schema = strsplit(id,split=".",fixed=T)[[1]][1]
    table = strsplit(id,split=".",fixed=T)[[1]][2]
    dat <- dbReadTable(con,c(schema,table))
    if(sum(names(dat)!=c("di_id","year","value"))==0){
      mr.dat = subset(dat,di_id=="MR")
      mr.dat$indicator = id
      mr.list[[id]] = mr.dat
    }
  },error=function(e){
    message(e)
  })
  
}
dbDisconnect(con)

mr.all = rbindlist(mr.list)

descriptions = concepts[,c("id","description")]
names(descriptions) = c("indicator","indicator_description")
descriptions$dup = duplicated(descriptions$indicator)
descriptions = subset(descriptions,!dup)
descriptions$dup = NULL
mr.all.merge = merge(mr.all,descriptions,all.x=T)
write.csv(mr.all.merge,"all_mauritania_data.csv",na="",row.names=F)
