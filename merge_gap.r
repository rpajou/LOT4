#Author: Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 22/10/2021


# Use CreateSpells to determine overall or overlapping periods 
# Assume that a gap of max 7 days can be ignored - and spells are concatenated.
# We take the most recent spell. If DAPs observe in their data a large loss of spells (loss of observation time within the study period), we will reconsider taking the longest observation period. 

#25/10 Roel: make script into function using data.table
#26/10 Vjola: apply merge_gaps after applying Create_Spells

merge_gap<-function(mydata=OBSERVATION_PERIODS, ID=person_id, startdate=op_start_date, enddate=op_end_date, gap=7){

ID_vec<-unique(mydata[[ID]])

mydata[mydata[[ID]]==ID_vec[i],]
final_obs<-list()

for (i in 1:length(ID_vec)){
  j<-1
  while(j<nrow(mydata[mydata[[ID]]==ID_vec[i],])){
   
    if((mydata[mydata[[ID]]==ID_vec[i],][j+1][[startdate]]-mydata[mydata[[ID]]==ID_vec[i],][j][[enddate]])<=gap){
      mydata[mydata[[ID]]==ID_vec[i],][j][[enddate]]<-mydata[mydata[[ID]]==ID_vec[i],][j+1][[enddate]]
      mydata[mydata[[ID]]==ID_vec[i],][j+1][[startdate]]<-NA
    }
    j<-j+1
    # else {break}}
   final_obs[[i]]<-complete.cases(mydata[mydata[[ID]]==ID_vec[i],])
  }
  merge_obs<-do.call(rbind.data.frame, final_obs)
  return(merge_obs)}



merge_gap(mydata = OBSERVATION_PERIODS)


