# Load files from all folders 
folders_list <- list.files(events_tmp, "\\ADR")
count_list <- list()
for (i in 1:length(folders_list)){
  file_list <-list.files(paste0(events_tmp, folders_list[i]), "\\.rds$")
  if (length(file_list)>0){
    combined_diagnosis_events<-lapply(paste0(paste0(events_tmp, folders_list[i], "/"), file_list), readRDS)
    combined_diagnosis_events<-do.call(rbind,combined_diagnosis_events)
    count <- combined_diagnosis_events[,.N, by = .(year, month(event_date))]
    count_list[[i]] <- count
  }
}

#data_list <- setNames(split(count, seq(nrow(count))), rownames(count))
# # Load files from all folders 
# folders_list <- list.files(events_tmp, "\\ADR")
# conditions_files <- list()
# conditions_codelist <- list()
# files_list <- list()
# 
# for (i in 1:length(folders_list)){
#     conditions_codelist[[i]] <- names(conditions_all[i][[1]])
#     conditions_files[[i]]<-c(list.files(paste0(events_tmp, folders_list[i]), "\\_start.rds$"),
#                         list.files(paste0(events_tmp, folders_list[i]), "\\_RCD.rds$"),
#                         list.files(paste0(events_tmp, folders_list[i]), "\\_SNOMED.rds$")
#                         )
#     if (length(conditions_files)>0){
#       #create combination year_condition from years(years present in the study) and all names of conditions in the codelist
#         filter_var<-as.data.table(expand.grid(years_events,conditions_codelist[[i]]))
#         names(filter_var)<-c("year","diagnosis")
#         filter_var[, comb:= paste0(year, "_", diagnosis)]
#         filter_var<-filter_var[!duplicated(comb)]
#         # #Create list by conditions and years
#         files<-vector(mode="list", length=filter_var[,.N])
#         names(files)<-filter_var[["comb"]]
#         
#         for (j in 1:length(files)){
#             files[[j]]<-conditions_files[[i]][grepl(names(files)[j], conditions_files[[i]])]
#           }
#         files<-Filter(length,files)
#         files_list[[i]] <- files
#     }
#   
# }
# 
#     ############################################################################
#     #Load each list element by combining all files inside one element
#     #perform the necessary counts
#     #export the data in populations named by events_year_condition_sex_population where sex==female
#     #remove all files from tmp
#     ############################################################################
# i <- 1
# j <- 1
# for (i in 1:length(files_list)){    
#   for (j in 1:length(files_list[[i]])){
#       # combined_diagnosis_events<-lapply(paste0(paste0(events_tmp, folders_list[i],"/"),files_list[[i]][[j]]), readRDS)
#       # combined_diagnosis_events<-do.call(rbind,combined_diagnosis_events)
#       combined_diagnosis_events<-lapply(paste0(paste0(events_tmp, folders_list[i],"/"),files_list[[i]][[j]]), readRDS)
#       # combined_diagnosis_events<-do.call(rbind,combined_diagnosis_events)
# #       combined_diagnosis_events[,code_nodot:=gsub("\\.","",event_code)]
# #       # combined_diagnosis_events[event_vocabulary!="SNOMEDCT_US",truncated_code:=substr(code_nodot,1,4)]
# #       # combined_diagnosis_events[event_vocabulary=="SNOMEDCT_US",truncated_code:=event_code]
# #       combined_diagnosis_events[event_vocabulary %!in% c("SNOMEDCT_US", "SCTSPA"),truncated_code:=substr(code_nodot,1,4)]
# #       combined_diagnosis_events[event_vocabulary %!in% c("SNOMEDCT_US", "SCTSPA"),truncated_code:=event_code]
# #       
# #       if (subpopulations_present=="Yes"){
# #         if(combined_diagnosis_events[,.N]>0){
# #           saveRDS(combined_diagnosis_events, paste0(diag_pop,subpopulations_names[s], "/", names(files)[i],"_events_diagnoses.rds"))
# #         }
# #       } else {
# #         if(combined_diagnosis_events[,.N]>0){
# #           saveRDS(combined_diagnosis_events, paste0(diag_pop,names(files)[i],"_events_diagnoses.rds"))
# #         }
# #       }
#   }
# }
# #     #rm(files)
# #     
# #     for(i in 1:length(conditions_files)){
# #       unlink(paste0(events_tmp,conditions_files[i]))
# #     }
# #   }
# 
#   
#   
# 
# 
# 
# # filter_var_list <- list()
# # files_list <- list()
# # 
# 
# #   if (length(conditions_files[[i]])>0){
# 
# #   }
# # }
# # 
# # #Load each list element by combining all files inside one element
# # #perform the necessary counts
# # #export the data in populations named by events_year_condition_sex_population where sex==female
# # #remove all files from tmp
# # ############################################################################
# # for (j in 1:length(files)){
# #       for (j in 1:length(files)){
# #       #   combined_diagnosis_events<-lapply(paste0(paste0(events_tmp, folders_list[i],"/"),files[[j]]), readRDS)
# #       #   combined_diagnosis_events<-do.call(rbind,combined_diagnosis_events)
# #       #   combined_diagnosis_events[,code_nodot:=gsub("\\.","",event_code)]
# #       #   combined_diagnosis_events[event_vocabulary %!in% c("SNOMEDCT_US", "SCTSPA"),truncated_code:=substr(code_nodot,1,4)]
# #       #   combined_diagnosis_events[event_vocabulary %!in% c("SNOMEDCT_US", "SCTSPA"),truncated_code:=event_code]
# #       # 
# #       #   if (subpopulations_present=="Yes"){
# #       #     if(combined_diagnosis_events[,.N]>0){
# #       #       saveRDS(combined_diagnosis_events, paste0(diag_pop,subpopulations_names[s], "/", names(files)[i],"_events_diagnoses.rds"))
# #       #     }
# #       #   } else {
# #       #     if(combined_diagnosis_events[,.N]>0){
# #       #       saveRDS(combined_diagnosis_events, paste0(diag_pop,names(files)[i],"_events_diagnoses.rds"))
# #       #     }
# #       #   }
# #       # }
# #   
# # }
# # 
# # 
# # 
# # #     ############################################################################
# # #     #rm(files)
# # #     
# # #     for(i in 1:length(conditions_files)){
# # #       unlink(paste0(events_tmp,conditions_files[i]))
# # #     }
# # #   }
# # #   
