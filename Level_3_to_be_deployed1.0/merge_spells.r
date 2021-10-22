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
