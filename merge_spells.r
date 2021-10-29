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

new_obs<- split(mydata, f = mydata[[ID]] )

final_obs<-list()

for (i in 1:length(new_obs)){
  j<-1
  while(j<nrow(new_obs[[i]])){
   
    if((new_obs[[i]][j+1][[startdate]]-new_obs[[i]][j][[enddate]])<=gap){
      new_obs[[i]][j][[enddate]]<-new_obs[[i]][j+1][[enddate]]
      j<-j+1
    }
    else {break}}
  final_obs[[i]]<-new_obs[[i]][1]
}

merge_obs<-do.call(rbind.data.frame, final_obs)
return(merge_obs)}

merge_gap(mydata = OBSERVATION_PERIODS)

OBSERVATION_PERIODS$person_id
merge_obs$obs_length<-merge_obs$op_end_date-merge_obs$op_start_date

hist(merge_obs$obs_length)
