library(XLConnect)
path <- "raw"
merge_file_name <- "csv/merged_file.csv"

filenames <- list.files(path= path, full.names=TRUE)

All <- lapply(filenames,function(filename){
  print(paste("Merging",filename,sep = " "))
  read.csv(filename)
})
df <- do.call(rbind.data.frame, All)
write.csv(df,merge_file_name)


