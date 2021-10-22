#Author: Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 22/10/2021


# Use CreateSpells to determine overall or overlapping periods 
# Assume that a gap of max 7 days can be ignored - and spells are concatenated.
# We take the most recent spell. If DAPs observe in their data a large loss of spells (loss of observation time within the study period), we will reconsider taking the longest observation period. 


new_obs<- split(OBSERVATION_PERIODS, f = OBSERVATION_PERIODS$person_id )

final_obs<-list()

for (i in 1:length(new_obs)){
  j<-1
  while(j<nrow(new_obs[[i]])){
   
    if((new_obs[[i]][j+1]$op_start_date-new_obs[[i]][j]$op_end_date)<=7){
      new_obs[[i]][j]$op_end_date<-new_obs[[i]][j+1]$op_end_date
      j<-j+1
    }
    else {break}}
  final_obs[[i]]<-new_obs[[i]][1]
}


merge_obs<-do.call(rbind.data.frame, final_obs)

merge_obs$obs_length<-merge_obs$op_end_date-merge_obs$op_start_date

hist(merge_obs$obs_length)
