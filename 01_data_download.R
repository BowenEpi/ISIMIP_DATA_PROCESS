## DOWDLOAD PROJECTION DATA
library(parallel);library(foreach);library(doParallel)

## read filelist
filelist <- as.data.frame(read.table("data/filelist.txt", header = FALSE))
ssp <- c("historical","ssp126","ssp370","ssp585")
model_name <- c("gfdl-esm4","ipsl-cm6a-lr","mpi-esm1-2-hr","mri-esm2-0","ukesm1-0-ll")
t_var <- c("tas","tasmax","tasmin")

## CREATE FOLDERS
for (k in seq(ssp)) {
  for (i in seq(model_name)) {
    for (j in seq(t_var)) {
      dir.create(paste0("data/rawdata/",ssp[k]))
      dir.create(paste0("data/rawdata/",ssp[k],"/",model_name[i]))
      dir.create(paste0("data/rawdata/",ssp[k],"/",model_name[i],"/",t_var[j]))
    }
  }
}


## CREATE DESTINATION
filelist$dest <- sapply(strsplit(filelist[,"V1"],split = "/"), function(x) x[13])
names(filelist) <- c("url",'dest')

time.start <- Sys.time()
## LOOP TO DOWNLOAD
core <- detectCores() 
cl <- makeCluster(6)
registerDoParallel(cl)

# Start parallel
foreach(i=1:nrow(filelist)) %dopar% {
  url <- filelist$url[i]
  dest <- filelist$dest[i]
  ##
  dir.name <- unlist(strsplit(dest,split = "_"))
  ##
  download.file(url = url, 
                destfile = paste0("data/rawdata/",dir.name[4],"/",dir.name[1],"/",dir.name[5],"/",dest), 
                quiet = T,mode = "wb")
}
# END cluster
stopCluster(cl)

Sys.time()-time.start
