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


##########################################################3
#reverse order

#ordering in the file problem- not ordered on startdate because of different OP_meaning

new_obs<- split(OBSERVATION_PERIODS, f = OBSERVATION_PERIODS$person_id )
new_obs<-new_obs[order( new_obs$op_start_date),]
final_obs<-list()

sample_dat<-OBSERVATION_PERIODS[OBSERVATION_PERIODS$person_id=="ConCDM_SIM_200421_00005",]
sample_dat[order(sample_dat$op_start_date),]


for (i in 1:length(new_obs)){
  j<-nrow(new_obs[[i]])
  while(j>1){
    
    if((new_obs[[i]][j-1]$op_end_date-new_obs[[i]][j]$op_start_date)<=7){
      new_obs[[i]][j]$op_start_date<-new_obs[[i]][j-1]$op_start_date
      j<-j-1
    }
    else {break}}
  final_obs[[i]]<-new_obs[[i]][j]
}


merge_obs<-do.call(rbind.data.frame, final_obs)
