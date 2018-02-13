###################################
##### summarize_JSON          #####
##### By: Marloes Eeftens     #####
##### 1st version: 12/02/2018 #####
###################################

summarize_JSON<-function(JSON_folder,pattern,summary_folder,ws_name,averaging_time,timevar,idvar,datavars){

  #If big_data already exists, load it:
  #If big_data does not exist yet, create big_data, files_processed, files_disregarded:
  if(file.exists(paste0(summary_folder,ws_name,".Rdata"))){
    load(paste0(summary_folder,ws_name,".Rdata"))
  }else{
    big_data<-data.frame()
    files_processed<-c()
    files_disregarded<-c()
  }

  #Functions
  toChron <- function(t) as.chron(t)

  #Define the to-do list according to new files which may have appeared:
  list_files<-list.files(JSON_folder)
  list_valid_files<-list_files[grepl(pattern=pattern,list_files)]
  files_to_process<-list_valid_files[!list_valid_files %in% c(files_processed,files_disregarded)]
  nr_to_process<-length(files_to_process)

  processing_start<-Sys.time()
  #Import all the files
  if(nr_to_process==0){
    nr_processed<-0
    nr_disregarded<-0
  }else{
    for(i in 1:nr_to_process){

      #See if file is empty, if not, read it in:
      if(file.info(paste0(JSON_folder,files_to_process[i]))$size==0){
        files_disregarded<-c(files_disregarded,files_to_process[i])
      }else{
        dat1<-read_json(paste0(JSON_folder,files_to_process[i]),simplifyVector=TRUE)

       #If the file does not have the right structure, disregards it and add to files_disregarded:
        if(!all(c(timevar,idvar,datavars) %in% names(dat1))){
          files_disregarded<-c(files_disregarded,files_to_process[i])
       #If in the right format, import and process:
       }else{
         dat1$posixtime<-as.POSIXct(strptime(dat1[,which(names(dat1)==timevar)],format="%Y-%m-%dT%H:%M:%S",tz="Europe/Zurich"))
          dat1$device_id<-dat1[,which(names(dat1)==idvar)]
          dat1<-subset(dat1,select=c("posixtime","device_id",datavars))
          #Get rid of any duplicated records:
          unique_nodes<-unique(dat1$device_id)
          #Initiate dat2_list
          dat2_list<-list()
          for(j in unique_nodes){
            dat2<-subset(dat1,device_id==j,select=c("posixtime",datavars))
            dat2<-dat2[!duplicated(dat2$posixtime),]
            #Clean the data, set timepoints where all variables are zero to NA.
            position_vars<-which(names(dat2) %in% datavars)
            dat2[rowSums(dat2[,position_vars],na.rm=TRUE)==0,position_vars]<-NA
            dat2_zoo<-read.zoo(dat2,index=which(names(dat2)=="posixtime"),FUN=toChron)
            dat2_zoo2<-aggregate(dat2_zoo,trunc(time(dat2_zoo),averaging_time),FUN=mean)
            dat2_avg<-as.data.frame(fortify(dat2_zoo2))
            names(dat2_avg)<-c("posixtime",datavars)
            dat2_avg$posixtime<-as.POSIXct(dat2_avg$posixtime,tz="Europe/Zurich")
            dat2_avg$device_id<-j
            dat2_list[[j]]<-dat2_avg
          }
          #Combine all data from list:
          new_data<-rbindlist(lapply(dat2_list,FUN=function(x) x)) #Rbindlist is 10* faster than do.call
          big_data<-rbind.data.frame(big_data,new_data)
          #Add file i to files_processed:
          files_processed<-c(files_processed,files_to_process[i])
        }
      }
    }
    processing_time<-Sys.time()-processing_start
    if(processing_time>60*15){warning("Dataset took over 15 min to compile...")}

    #Save the new version of big_data:
    save(big_data,files_processed,files_disregarded,file=paste0(summary_folder,ws_name,".RData"))
    nr_processed<-sum(files_processed %in% files_to_process)
    nr_disregarded<-sum(files_disregarded %in% files_to_process)
  }
  #Return message:
  nr_total<-length(files_processed)
  message<-paste0("There were ",nr_to_process," new files detected, of which ",nr_processed," were processed and ",
                  nr_disregarded," were disregarded. The resulting data are stored in the workspace ",
                  paste0(summary_folder,ws_name,".RData"),", which contains the processed data of ",nr_total," files.")
  return(message)

}
