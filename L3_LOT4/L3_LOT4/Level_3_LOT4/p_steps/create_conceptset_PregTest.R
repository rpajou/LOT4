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

# Read Pregnancy_Testing codelist -> gives a list of all tabs with matching (partial or full names)
codelist_list<- load_codelist(paste0(pre_dir,"ANNEX2a_Valproate_Code_List_20201125.xlsx"), matches <- c("Pregnancy_Testing"))

# Create a datatable with all ADRs
codelist <- rbindlist(codelist_list)

#select only necessary columns
# Select only necessary columns 
codelist <-codelist[,c("Concept name","Coding system", "Code")]
setnames(codelist,"Coding system", "Coding_system")
setnames(codelist,"Concept name", "Condition")
# Coding_System clean up
# Transforms ICD10/CM -> ICD10CM
codelist[,Coding_system:=gsub("\\/","",codelist[,Coding_system])]
# Transforms ICPC2EENG -> ICPC2
codelist[,Coding_system:=gsub("EENG","",codelist[,Coding_system])]
# Deletes records where code is missing (-)
codelist <- codelist[ !Code == '-',] 
#Create variable dot_present
codelist[,dot_present:=str_detect(codelist[,Code],"\\.")]
#Create variable code_no_dot by removing dot from all codes
codelist[,code_no_dot:=gsub("\\.","",codelist[,Code])]
vocabularies_list<-codelist[!duplicated(Coding_system), Coding_system]
#put all information in a list
conditions<-vector(mode="list", length=length(unique(na.omit(codelist[,Condition]))))
names(conditions)<-unique(na.omit(codelist[,Condition]))
for (i in 1:length(conditions)){
  vocabularies<-vector(mode="list", length=length(unique(na.omit(codelist[,Coding_system]))))
  names(vocabularies)<-unique(na.omit(codelist[,Coding_system]))
  for (j in 1:length(vocabularies)){
    vocabularies[[j]]<-codelist[Condition==names(conditions)[i] & Coding_system==names(vocabularies)[j], Code]
  }
  conditions[[i]]<-list.append(conditions[[i]],vocabularies)
  rm(vocabularies)
}

#remove empty vocabularies
conditions<-lapply(conditions, function(x) Filter(length, x))

################################################################################################################
#Rule: start with
#Coding system: "ICD10CM"     "ICD9CM"      "ICPC2P"  "MTHICD9"     "ICPC" 
#################################################################################################################
#vocabularies that will be filtered with start with
conditions_start<-list()
for(i in 1:length(conditions)){
  conditions_start[[i]]<-conditions[[i]][names(conditions[[i]])%!in% c("SNOMEDCT_US", "SCTSPA", "RCD", "RCD2")]
}

names(conditions_start)<-names(conditions)

for(i in 1:length(conditions_start)){
  lapply(conditions_start[[i]], function(x) x[names(x) %in% c("Code")])
}

conditions_start <- Filter(function(x) length(x) > 0, conditions_start)

################################################################################################################
#Rule:Remove dot, start with
#Coding system: Read codes v2: "RCD", "RCD2" 
###############################################################################################################
conditions_read<-vector(mode="list", length=length(unique(na.omit(codelist[,Condition]))))
names(conditions_read)<-unique(na.omit(codelist[,Condition]))

for (i in 1:length(conditions_read)){
  vocabularies_RCD<-vector(mode="list", length=2)
  vocabularies_RCD$RCD2<-codelist[Condition==names(conditions_read)[i] & Coding_system=="RCD2", code_no_dot]
  vocabularies_RCD$RCD<-codelist[Condition==names(conditions_read)[i] & Coding_system=="RCD", code_no_dot]
  
  conditions_read[[i]]<-list.append(conditions_read[[i]],vocabularies_RCD)
  rm(vocabularies_RCD)
}
conditions_read<-lapply(conditions_read, function(x) Filter(length, x))
conditions_read <- Filter(function(x) length(x) > 0, conditions_read)
################################################################################################################
#Rule: match exactly
#Coding system:  "SNOMEDCT_US", "SCTSPA"  
#################################################################################################################
#SNOMED codes
conditions_snomed<-list()
for(i in 1:length(conditions)){
  conditions_snomed[[i]]<-conditions[[i]][names(conditions[[i]])%in% c("SNOMEDCT_US", "SCTSPA")]
}
names(conditions_snomed)<-names(conditions)
conditions_snomed <- Filter(function(x) length(x) > 0, conditions_snomed)
################################################################################################################
#output folder for Info report in g_output
if ("Info" %in% list.files(output_dir)){
  info_dir<-paste(output_dir, "Info/",sep="")
  do.call(file.remove, list(list.files(info_dir, full.names = T)))
} else {
  #Create the Info folder in the output dir
  dir.create(paste(output_dir, "Info", sep=""))
  info_dir<-paste(output_dir, "Info/", sep="")
}

codelist[,comb:=paste(Condition, Coding_system,"_")]
codelist[,dot_present:=NULL][,code_no_dot:=NULL]
codelist[,Codes:=paste0(Code, collapse = ", "), by="comb"]
codelist<-codelist[!duplicated(comb)]
codelist[,Code:=NULL][,comb:=NULL]
setnames(codelist,"Condition","event_definition")

write.csv(codelist, paste0(info_dir, "preg_test_codelist.csv"), row.names = F)

