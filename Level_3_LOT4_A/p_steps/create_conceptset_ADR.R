#output folder for Info report in g_output
if ("Info" %in% list.files(output_dir)){
  info_dir<-paste(output_dir, "Info/",sep="")
  do.call(file.remove, list(list.files(info_dir, full.names = T)))
} else {
  #Create the Info folder in the output dir
  dir.create(paste(output_dir, "Info", sep=""))
  info_dir<-paste(output_dir, "Info/", sep="")
}

`%!in%` = Negate(`%in%`)
# Load function: 'load_codelist'
source(paste0(pre_dir,"/functions/LoadCodelist.R"))

# Read ADR codelist -> gives a list of all tabs with matching (partial or full names)
codelist_list<- load_codelist(paste0(pre_dir,"ANNEX2a_Valproate_Code_List_20201125.xlsx"), matches <- c("ADR"))
# codelist_list<- load_codelist(paste0(pre_dir,"ANNEX2a_Oral_Retinoids_Code_List_20201124.xlsx"), matches <- c("ADR"))

# Create a datatable with all ADRs
#codelist_all <- rbindlist(codelist_list) 

# Create a separate df for each of the ADR's
ADR_names <- names(codelist_list)
vocabularies_list <- list()
conditions_read_all <- list()
conditions_snomed_all <- list()
conditions_start_all <- list()
conditions_all <- list()
codelist_all <- list()

for(i in seq_along(codelist_list)) {
  # Create a datatable with all ADRs
  codelist <- rbindlist(codelist_list[i])
  
  # Drop columns you will not use
  # In the original code we kept necessary columns
  codelist <-codelist[,c("Concept name","Coding system", "Code")]
  setnames(codelist,"Coding system", "Coding_system")
  setnames(codelist,"Concept name", "Condition")

  # # Coding_System clean up
  # # Transforms ICD10/CM -> ICD10CM
  codelist[,Coding_system:=gsub("\\/","",codelist[,Coding_system])]
  # # Transforms ICPC2EENG -> ICPC2
  codelist[,Coding_system:=gsub("EENG","",codelist[,Coding_system])]
  # # Deletes records where code is missing (-)
  codelist <- codelist[ !Code == '-',]
  #Create variable dot_present
  codelist[,dot_present:=str_detect(codelist[,Code],"\\.")]
  #Create variable code_no_dot by removing dot from all codes
  codelist[,code_no_dot:=gsub("\\.","",codelist[,Code])]
  vocabularies_list<-codelist[!duplicated(Coding_system), Coding_system]
  #put all information in a list
  conditions<-vector(mode="list", length=length(unique(na.omit(codelist[,Condition]))))
  names(conditions)<-unique(na.omit(codelist[,Condition]))
  for (l in 1:length(conditions)){
    vocabularies<-vector(mode="list", length=length(unique(na.omit(codelist[,Coding_system]))))
    names(vocabularies)<-unique(na.omit(codelist[,Coding_system]))
    for (j in 1:length(vocabularies)){
      vocabularies[[j]]<-codelist[Condition==names(conditions)[l] & Coding_system==names(vocabularies)[j], Code]
    }
    conditions[[l]]<-list.append(conditions[[l]],vocabularies)
    rm(vocabularies)
  }
  
  
  #remove empty vocabularies
  conditions<-lapply(conditions, function(x) Filter(length, x))
  
  conditions_all[[i]] <- conditions
  
  #################################################################################################################
  #Rule: start with
  #Coding system: ICD9, ICD9CM, ICD10, ICD10CM, ICPC
  #################################################################################################################
  #vocabularies that will be filtered with start with
  conditions_start<-list()
  for(l in 1:length(conditions_all[[i]])){
    conditions_start[[l]]<-conditions_all[[i]][[l]][names(conditions_all[[i]][[l]])%!in% c("SNOMEDCT_US", "SCTSPA", "RCD", "RCD2")]
  }

  names(conditions_start)<-names(conditions_all[[i]])

  for(l in 1:length(conditions_start)){
    lapply(conditions_start[[l]], function(x) x[names(x) %in% c("Code")])
  }

  conditions_start <- Filter(function(x) length(x) > 0, conditions_start)

  conditions_start_all[[i]] <- conditions_start

  ###############################################################################################################
  #Rule:Remove dot, start with
  #Coding system: Read codes v2
  ###############################################################################################################
  conditions_read<-vector(mode="list", length=length(unique(na.omit(codelist[,Condition]))))
  names(conditions_read)<-unique(na.omit(codelist[,Condition]))

  for (l in 1:length(conditions_read)){
    vocabularies_RCD<-vector(mode="list", length=2)
    vocabularies_RCD$RCD2<-codelist[Condition==names(conditions_read)[l] & Coding_system=="RCD2", code_no_dot]
    vocabularies_RCD$RCD<-codelist[Condition==names(conditions_read)[l] & Coding_system=="RCD", code_no_dot]

    conditions_read[[l]]<-list.append(conditions_read[[l]],vocabularies_RCD)
    rm(vocabularies_RCD)
  }
  conditions_read<-lapply(conditions_read, function(x) Filter(length, x))
  conditions_read <- Filter(function(x) length(x) > 0, conditions_read)

  conditions_read_all[[i]] <- conditions_read
  ################################################################################################################
  #Rule: match exactly
  #Coding system: SNOMEDCT_US
  #################################################################################################################
  #SNOMED codes
  conditions_snomed<-list()
  for(l in 1:length(conditions_all[[i]])){
    conditions_snomed[[l]]<-conditions_all[[i]][[l]][names(conditions_all[[i]][[l]])%in% c("SNOMEDCT_US", "SCTSPA")]
  }
  names(conditions_snomed)<-names(conditions_all[[i]])
  conditions_snomed <- Filter(function(x) length(x) > 0, conditions_snomed)


  conditions_snomed_all[[i]] <- conditions_snomed

  codelist[,comb:=paste(Condition, Coding_system,"_")]
  codelist[,dot_present:=NULL][,code_no_dot:=NULL]
  codelist[,Codes:=paste0(Code, collapse = ", "), by="comb"]
  codelist<-codelist[!duplicated(comb)]
  codelist[,Code:=NULL][,comb:=NULL]
  setnames(codelist,"Condition","event_definition")

  assign(ADR_names[i], as.data.table(codelist))
  write.csv(get(ADR_names[i]), paste0(info_dir, ADR_names[i], ".csv"), row.names = FALSE)
  codelist_all[[i]] <- codelist
}
names(conditions_all) <- names(codelist_list)
names(conditions_read_all) <- names(codelist_list)
names(conditions_snomed_all) <- names(codelist_list)
names(conditions_start_all) <- names(codelist_list)
names(codelist_all) <- names(codelist_list)
#rm(codelist_list, conditions_read, conditions_snomed, conditions_start, conditions, codelist, i,j,l)

