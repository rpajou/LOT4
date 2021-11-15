#generate lineplots of counts of variables
#monthly counts by year
#total counts over whole study period

#plot groups of variables on one plot
# can't use count list because doesn't have variable names

count_names<-list.files(diag_pop, pattern="count")

count_files<-lapply(paste0(diag_pop, count_names), readRDS)

par(mfrow=c(length(count_list),1))
#why stopping at 2019-01 (should be 2019-12)

for(i in 1:length(count_list)){
  # par(mfrow=c(length(count_list),1))
  main_name<-substr(count_names[[i]], 1,nchar(count_names[[i]])-11)
  var_counts<-count_files[[i]]$N
  mask_counts<-var_counts[varcounts<=5]<-5
  mycounts<-ts(mask_counts, frequency = 12, start = 2009,end = 2020)
  mydates<-paste0(15,"-",count_list[[i]]$month, "-", count_list[[i]]$year)
  count_list[[i]]$date<-as.Date(mydates, "%d-%m-%y")
  # plot(count_list[[i]]$date, count_list[[i]]$N, xaxt="n", yaxt="n")
  plot(mycounts, xaxt="n", yaxt="n", xlab="", ylab="counts", main=main_name, lwd=2, cex.main=1.5)
  tsp = attributes(mycounts)$tsp
  dates = seq(as.Date("2009-01-01"), by = "month", along = mycounts)
  axis(1, at = seq(tsp[1], tsp[2], along = mycounts),las=2, labels = format(dates, "%Y-%m"))
  axis(2, seq(0:(max(count_files[[i]]$N)+1)))

}

#maksing values <=5 (with legend note)

# layering plots from same group

for(i in 1:length(count_list)){
  main_name<-substr(count_names[[i]], 1,nchar(count_names[[i]])-11)
  mycounts<-ts(count_files[[i]]$N, frequency = 12, start = 2009)
  mydates<-paste0(15,"-",count_list[[i]]$month, "-", count_list[[i]]$year)
  count_list[[i]]$date<-as.Date(mydates, "%d-%m-%y")
  # plot(count_list[[i]]$date, count_list[[i]]$N, xaxt="n", yaxt="n")
  plot(mycounts, xaxt="n", yaxt="n", xlab="", ylab="", main=main_name )
  tsp = attributes(mycounts)$tsp
  dates = seq(as.Date("2009-01-01"), by = "month", along = mycounts)
  axis(1, at = seq(tsp[1], tsp[2], along = mycounts),las=2, labels = format(dates, "%Y-%m"))
  axis(2, seq(0:(max(count_files[[i]]$N)+1)))
  
}