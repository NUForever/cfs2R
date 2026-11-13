#' Realiza los pre-procesos de descarga, corte y conversión la información del modelo CFSv2 en tiempo real
#'
#' Información mas detallada.
#'
#' @return Archivos NetCDF para el rango de tiempocortados en la zona de estudio
#'
#' @export

preprocesscfsrealtime <- function(variable,lat1,lat2,lon1,lon2,direct) {

  base::system(paste0("rm ",direct,"/* "))
  urlRT <- "http://nomads.ncep.noaa.gov/pub/data/nccf/com/cfs/prod/cfs/"
  html <- paste(readLines(urlRT), collapse="\n")
  matched <- str_match_all(html, "<a href=\"(.*?)\"")
  links <- matched[[1]][, 2]
  links <- as.list(links)
  links <- (links[-1])
  hRT <- c("00","06","12","18")
  dfRT = data.frame(lista=character(), stringsAsFactors = F)
  for (d in links) {
    for (h in hRT) {
      links2 = substr(d,1,12)
      links3 = substr(d,5,12)
      linkRT = paste(urlRT,links2,"/",h,"/time_grib_01/prate.01.",links3,h,".daily.grb2",sep="")
      if (url.exists(linkRT)) {
        linkRT = as.data.frame(linkRT)
        dfRT <- rbind(dfRT,linkRT)
      }
    }
  }
  dfRT <- data.frame(dfRT,stringsAsFactors = F)
  colnames(dfRT) <- "lista"
  n=12
  dfRT <- tail(dfRT,n)
  URL2 <- as.vector(dfRT$lista)


  for (url in URL2) {
    #DESCARGA
    download.file(url, destfile = paste(direct,"/",basename(url), sep = ''),
                  method="auto", quiet = FALSE, mode="wb", cacheOK = TRUE)
    archivo = substr(url,98,107) #Real_Time
    #CONVERSION
    system(paste0("cd ",direct,"; cdo -f nc copy ",variable,".01.",archivo,".daily.grb2 ",archivo,".nc;",sep="")) # RealTime
    #CORTE
    system(paste0("cd ",direct,"; cdo sellonlatbox",",",lat1,",",lat2,",",lon1,",",lon2," ",archivo,".nc ",archivo,"_cortado.nc;",sep=""))#0.37s
    #BORRADO DE NC Y GRB2
    system(paste0("rm ",direct,"/",archivo,".nc;",sep=""))
    system(paste0("rm ",direct,"/",variable,".01.",archivo,".daily.grb2",sep=""))
  }
}
