read_nvss_vintage_pop <- function(year=c("19","20")) {
  
  # match year (defaults to "19", must be either "19" or "20")
  year = match.arg(year)

  # define the url
  url=paste0("https://www.cdc.gov/nchs/nvss/bridged_race/pcen_v20",year, "_y", year,"_txt.zip")
  
  # download zip into temp directory and extract file 
  zf <- tempfile()
  download.file(url, destfile = zf, silent=T)
  zd <- tempdir()
  unzip(zf,exdir=zd)
  
  # get the file name, and read it in as a single column (V1) per line
  fname <- list.files(zd,full.names = T, pattern=paste0("pcen_v20",year, "_y", year,".txt"))
  res <- data.table::fread(fname,sep = "",header = F)
  
  # parse the line (column V1), into fips, age, and population
  # note that population is summed over age and fips (race and sex is also included)
  res[,fips:=stringr::str_sub(V1,10,14)]
  res[,age:=as.numeric(stringr::str_sub(V1,15,16))]
  res[,pop:=as.numeric(stringr::str_sub(V1,19))]
  
  res <- res[, .("pop" = sum(pop, na.rm=T)), by=.(fips,age)]

  return(res[])
}