#' Realiza los pre-procesos de descarga, corte y conversión la información del modelo CFSv2 operacional y retrospectivo.
#'
#' Información mas detallada.
#'
#' @return Archivos NetCDF para el rango de tiempocortados en la zona de estudio
#'
#' @export

preprocess <- function(product,variable,start,end,lat1,lon1,lat2,lon2,direct) {

  lista <- system.file("extdata", paste0("lista_",product,".csv"), package = "cfs2R")
  url <- read.csv(lista)
  URL <- as.data.frame(url$lista)
  URL2 <- as.vector(URL$`url$lista`)
  index_start = match(start, URL2)
  index_end = match(end, URL2)
  URL3 = URL2[index_start:index_end]

  for (url in URL2) {
    if (!url.exists(url)) {
      next
    }

    #DESCARGA
    download.file(url, destfile = paste(direct,"/",basename(url), sep = ''),
                  method="auto", quiet = FALSE, mode="wb", cacheOK = TRUE)
    if (product=="reforecast") {
      archivo = substr(url,66,75) #reforecast
      } else {
      archivo = substr(url,84,93) #operational
      }

    #CONVERSION
    if (product=="reforecast") {
      system(paste0("cd ",direct,"; cdo -f nc copy ",variable,".",archivo,".time.grb2 ",archivo,".nc;",sep="")) # Reforecast
    } else {
      system(paste0("cd ",direct,"; cdo -f nc copy ",variable,".01.",archivo,".daily.grb2 ",archivo,".nc;",sep="")) # Operational
    }

    #CORTE
    system(paste0("cd ",direct,"; cdo sellonlatbox",",",lat1,",",lat2,",",lon1,",",lon2," ",archivo,".nc ",archivo,"_cortado.nc;",sep=""))#0.37s

    #BORRADO DE NC Y GRB2
    system(paste0("rm ",direct,"/",archivo,".nc;",sep=""))

    if (product=="reforecast"){
      system(paste0("rm ",direct,"/",variable,".",archivo,".time.grb2",sep="")) # reforecast
    } else {
      system(paste0("rm ",direct,"/",variable,".01.",archivo,".daily.grb2",sep="")) # operational
    }
    }
  }
