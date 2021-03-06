#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 11/10/2021



# READ.ME
#LOT 4 preselection application onto multiple table subsets (especially MEDICINES)

# set path
path_write<-"C://Users//Acer//OneDrive//Documents//GitHub//LOT4//CDMInstances//LOT4//CDMInstances_preselection//"
path_dir<-"C://Users//Acer//OneDrive//Documents//GitHub//LOT4//CDMInstances//LOT4//"
#get tables 
#VJOLA please insert the code here for loading the lists of tables
#Get EVENTS, MO, SO, MEDICINES, VACCINES tables
actual_tables_preselect<-list()
actual_tables_preselect$EVENTS<-list.files(path_dir, pattern="^EVENTS")
actual_tables_preselect$MEDICAL_OBSERVATIONS<-list.files(path_dir, pattern="^MEDICAL_OBSERVATIONS")
actual_tables_preselect$SURVEY_OBSERVATIONS<-list.files(path_dir, pattern="^SURVEY_OBSERVATIONS")
actual_tables_preselect$MEDICINES<-list.files(path_dir, pattern="^MEDICINES")
actual_tables_preselect$VACCINES<-list.files(path_dir, pattern="^VACCINES")
actual_tables_preselect$SURVEY_ID<-list.files(path_dir, pattern="^SURVEY_ID")
actual_tables_preselect$EUROCAT<-list.files(path_dir, pattern="^EUROCAT")
actual_tables_preselect$PERSONS<-list.files(path_dir, pattern="^PERSONS")

all_actual_tables<-list.files(path_dir)


#get functions
source(paste0(path, "preselection_DAP.r"))


#run personsfilter on PERSONS table (PERSONS USUALLY one table)

  PERSONS<-read.csv(paste0(path_dir, actual_tables_preselect$PERSONS))
  personsfilter_ID<-as.vector((personsfilter(personstable = PERSONS))[[1]])

  
ATCfilter_ID<- list()
for(i in 1:length(actual_tables_preselect$MEDICINES)) {
  MEDS<-read.csv(paste0(path_dir, actual_tables_preselect$MEDICINES[[i]]))
  output<- ATCfilter(medtable = MEDS)
  ATCfilter_ID[[i]]<-unique(output[[1]])
}

ATCfilter_ID_unique<-as.vector(unique(unlist(ATCfilter_ID)))

#combine filters for final preselction IDs

final_ID<-ATCfilter_ID_unique[(ATCfilter_ID_unique%in%personsfilter_ID)==T]

tables_df<-as.data.frame(unlist(actual_tables_preselect))
colnames(tables_df)<-"CDMtableName"
tables_vec_all<-unique(as.vector(tables_df$CDMtableName))


#subset data using presection IDs and write new files
#need to name each new table the same as the old table, then write in the new folder
for(i in 1:length(tables_vec)){
  tablename<-(tables_vec_all[i])
    mytable<-read.csv(paste0(path_dir,tablename), row.names = NULL)
    preselect_table<-mytable[mytable$person_id%in%final_ID,]
    write.csv(preselect_table, paste0(path_write, tablename))
}

myextab<-read.csv(paste0(path_dir,tables_vec[1]))

myextab<-read.csv(file="C://Users//Acer//OneDrive//Documents//GitHub//LOT4//CDMInstances//LOT4//EVENTS_PS_2.csv")

#only wrote 8 files (one for each section of the actual files list)... It's only keeping the last one in each sublist...