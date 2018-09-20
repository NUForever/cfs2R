
#' Crea serie predictora para período operacional, desagregada espacialmente y con el sesgo corregido.
#'
#' Información mas detallada.
#'
#' @return Matriz multidimensional
#'
#' @export

modelcfs <- function(tol = 0.5,n = 12,p = 0.3 , coef, MMoperational, auxfiles) {

est <- read.csv(paste0(auxfiles,"/est.csv"))
pred <- read.csv(paste0(auxfiles,"/pred.csv"))
hest = read.csv(paste0(auxfiles,"/hest.csv"), stringsAsFactors = FALSE)

BRCK = MMoperational
cf = coef

fechas = names(BRCK[[1]])

dia_inicial = as.character(fechas[[2]])
dia_inicial = gsub('\\_','', dia_inicial)
dia_inicial = ymd(substr(dia_inicial,1,8))

dia_final   = as.character(fechas[[length(fechas)]])
dia_final = gsub('\\_','', dia_final)
dia_final = ymd(substr(dia_final,1,8))

largo_dim = dim(BRCK)[1]*dim(BRCK)[2]
geopuntos = seq(1,largo_dim,1)

dates = seq(dia_inicial,dia_final, by = 'days')
dates <- as.list(dates)

#CREO UNA MATRIZ DE DATAFRAES 3x3 a rellenar ## CAMBIAR A NxN
OPERATIONAL = array(as.data.frame(0, colnames = c("day")), dim=c(dim(BRCK)[1],dim(BRCK)[2]))
for (p in geopuntos) {
  OPERATIONAL[[p]] = data.frame(matrix(vector(), 0, 2,
                                       dimnames=list(c(), c("fecha", "precipitacion"))),
                                stringsAsFactors=F)
}

for (k in geopuntos) {
  exdf = BRCK[[k]]
  for (k2 in dates) {
    aux = dplyr::filter(exdf, ymd(`BRCK[[la, lo]]`)==ymd(paste(k2[[1]]))) #primero selecciono la fila que contiene
    aux <- aux[ , ! apply( aux , 2 , function(x) all(is.na(x)) ) ] #borro las columnas con NA
    aux <- aux[tail(seq_along(aux),n)] #dejo las n últimas columnas = n ultimas observaciones

    aux$`BRCK[[la, lo]]` <- NULL #borro la columna innecesaria de fechas (ya que solo está el dia de análisis)
    names(aux) <- NULL #borro los headers (en este caso eran los nombres de los archivos)
    aux <- c(aux)
    l = length(aux)
    lluvias = sum(aux > tol)

    if (l == 0) {
      ind = 0
    } else {
      ind = lluvias/l
    }

    if (ind*n >= n*p ) {
      aux <- aux[ aux > tol]
      aux <- unlist(aux,use.names=F)

      mm = mean(aux)

      if (mm <= tol) {
        mm = 0
      }

    } else {
      mm = 0
    }

    registro = data.frame(k2, mm)
    OPERATIONAL[[k]] <- rbind(OPERATIONAL[[k]],registro) #se bindea como row el registro
  }
}

hest$DateTime = mdy(hest$DateTime)

nombres = names(hest)
nombres = gsub('\\ ', '.', nombres)
lista_estaciones = as.list(nombres[-1]) #con punto

tol=0 #tolerancia para registros históricos. dejar esto fijo

est.sp <- SpatialPoints(list(est$lat,est$lon))
pred.sp <- SpatialPoints(list(pred$lat,pred$lon))
est$near <- apply(spDists(est.sp,pred.sp), 1, which.min)   # creo la columna near para vecino mas cercano


# ACÁ SE LEE LA MATRIZ DE COEFICIENTES
for (e in lista_estaciones) {
  e = as.character(e)
  e2 = gsub('\\.', ' ', e) #nombre estacion sin el con espacio en vez de punto

  nearest_station <- est %>%     #selecciono la mas cercana
    dplyr::filter(location == e2) %>%
    dplyr::select(near)
  near_index <-
    as.numeric(nearest_station) # indice del pronóstico mas cercano

  for (i in 1:12) {
    CFSv2 = OPERATIONAL[[near_index]] %>%
      dplyr::filter(month(k2) == i)
    colnames(CFSv2) <- c("DateTime", "mm")
    C = cf %>%         #COEFICIENTES DE AJUSTE CORRESPONDIENTES
      dplyr::filter(meses == i) %>%
      dplyr::filter(estaciones == e2)

    if (is.na(C$scalereforecast) || is.na(C$shapereforecast)) {
      CFSv2_downscaled = CFSv2$mm
    }
    else {
      vrt = pgamma(CFSv2$mm,
                   scale = C$scalereforecast,
                   shape = C$shapereforecast)
      CFSv2_downscaled = qgamma(vrt,
                                scale = C$scaleobservado,
                                shape = C$shapeobservado)
    }

    if (i == 1) {
      M <- data_frame(DateTime = CFSv2$DateTime, mm = CFSv2_downscaled)
    } else {
      N <- data_frame(DateTime = CFSv2$DateTime, mm = CFSv2_downscaled)
      M <- dplyr::bind_rows(M, N)
    }
  }

  M <- arrange(M,DateTime)
  colnames(M)[ncol(M)] <- e
  if (e == lista_estaciones[[1]]) {
    OPERATIONAL_DOWNSCALED = M
  } else {
    M <- M[,-1]
    OPERATIONAL_DOWNSCALED = dplyr::bind_cols(OPERATIONAL_DOWNSCALED, M)
  }
  rm("M")
}
OPERATIONAL_DOWNSCALED

}
