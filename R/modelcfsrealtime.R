
#' Crea serie predictora en tiempo real, desagregada espacialmente y con el sesgo corregido.
#'
#' Información mas detallada.
#'
#' @return Matriz multidimensional
#'
#' @export
#'
modelcfsrealtime <- function(variable,tol = 0.5 ,p = 0.3 , n = 12 ,coef , direct, auxfiles) {
  #modelcfsrealtime(variable="prate",coef=,direct="/home/forever/Desktop/PROY/DATA/Peo")
  #PARA CORREGIR EL ERROR DE ESTE CÓDIGO VOY A TENER QUE PROBAR POR PARTES
  # PRIMERO: VER SI el ladrillo REALTIME está bien,
  # si esta mal hay que corregir antes
  nn = n
  cf = coef
  trans = list.files(direct)
  trans = as.list(trans)
  TRANS <- as.vector(substr(trans,1,10))

  vec = c()
  for (k in TRANS) {
    a2 = nc_open(paste0(direct,"/",k,"_cortado.nc",sep=""))
    prate = ncvar_get(a2,paste0(variable),start=c(1,2,1),count=c(1,1,-1))*6*3600 #OJO ACA! como esta tomando la grilla??

    tp = ncvar_get(a2,"time") #1 obtener fecha inicial desde el ncdf
    tunitsp = ncatt_get(a2,"time","units")
    tustrp <- strsplit(tunitsp$value, " ")
    fini = unlist(tustrp)[3]
    hini = unlist(tustrp)[4]

    dh = ymd_hms(paste(fini,hini,sep=" "), tz = "UTC") - hours(12) + 6*hours(0:(length(tp)-1))#conversion a "chile" : las 00 están a las 8am
    serie <- data.frame(dh, prate)
    xtsserie = xts(serie[,-1], order.by=serie[,1])#3 convertir a xts
    xtsserie <- apply.daily(xtsserie,sum) #4 agregar a nivel diario
    df = data.frame(xtsserie) #6 convertir a df

    vec <- c(vec,max(dim(df)))
    nc_close(a2)
  }
  m = min(vec)
  mm = max(vec)
  largo = length(TRANS)
  N_A = rep(0:largo, each=4, len=(largo))
  N_A <- as.data.frame(N_A)
  ################################ CREACION DE MATRIZ MULTIDIMENSIONAL ###################################
  ######2) contar los archivos NN
  NN = length(vec)
  vecNC = vec+N_A

  LM = max(vecNC)

  NAarriba=N_A

  ######3) crear vector de horas VH de largo LM
  K2 = TRANS[1]
  a2 = nc_open(paste0(direct,"/",K2,"_cortado.nc",sep=""))
  tp = ncvar_get(a2,"time") #1 obtener fecha inicial desde el ncdf
  tunitsp = ncatt_get(a2,"time","units")
  tustrp <- strsplit(tunitsp$value, " ")
  fini = unlist(tustrp)[3]
  hini = unlist(tustrp)[4]

  lon = ncvar_get(a2,"lon")
  lon <- c(rep(1,(length(lon)))*360-lon)*-1
  lat = ncvar_get(a2,"lat")
  lat <- rev(lat)
  # Indices
  laidx = seq(1, length(lat), 1)
  loidx = seq(1, length(lon), 1)
  #listapuntos = expand.grid(laidx,loidx)
  nc_close(a2)

  VH = ymd(paste(fini), tz = "UTC") - days(1) + days(0:(LM-1))
  VH = as.data.frame(VH)
  colnames(VH) <- c("day")

  BRCK = array(as.data.frame(VH, colnames = c("day")), dim=c(length(laidx),length(loidx))) #probar esta linea pé
  for (lo in loidx) {
    for (la in laidx) {
      as.data.frame(BRCK[[la,lo]])
    }
  }

  for (archivo in TRANS) {
    #separar los numeros de archivo  para crear año_mes_diahora
    AAño = substr(archivo, 1, 4)
    MMes = substr(archivo, 5, 6)
    DDiahora = substr(archivo, 7, 10)

    a2 = nc_open(paste0(direct,"/",archivo,"_cortado.nc",sep=""))
    ########################
    ########################
    for (lo in loidx) {
      for (la in laidx) {

        tp = ncvar_get(a2,"time")#1 obtener fecha inicial desde el ncdf
        tunitsp = ncatt_get(a2,"time","units")
        tustrp <- strsplit(tunitsp$value, " ")
        fini = unlist(tustrp)[3]
        hini = unlist(tustrp)[4]

        dh = ymd_hms(paste(fini,hini,sep=" "), tz = "UTC") - hours(6) + 6*hours(0:(length(tp)-1))#conversion a "chile" : las 00 están a las 8am chilenas

        prate = ncvar_get(a2,variable,start=c(lo,la,1),count=c(1,1,-1))*6*3600 # transformacion unidades: 6 horas acum

        serie <- data.frame(dh, prate)
        xtsserie = xts(serie[,-1], order.by=serie[,1])#3 convertir a xts
        xtsserie <- apply.daily(xtsserie,sum) #4 agregar a nivel diario
        df = fortify(xtsserie)

        FCprate_diario = df$xtsserie

        # se crean los vectores con NA al principio y final correspondiente a cada archivo
        archivo <- as.integer(archivo)
        TRANS <- as.vector(TRANS)

        if (archivo==TRANS[1]) {
          NA_final = rep(NA, LM - length(FCprate_diario) )
          FCprate_diario_plusNA = c(FCprate_diario,NA_final)

        } else {

          NA_final = rep(NA, LM - NAarriba[which(TRANS==archivo)-1, ] -length(FCprate_diario))

          NA_principio = rep(NA, NAarriba[which(TRANS==archivo)-1, ])
          FCprate_diario_plusNA =  c(NA_principio,FCprate_diario,NA_final)

        }

        BRCK[[la,lo]] <- cbind.data.frame(BRCK[[la,lo]],FCprate_diario_plusNA)
        names(BRCK[[la,lo]])[length(names(BRCK[[la,lo]]))]<- paste(AAño,"_",MMes,"_",DDiahora,sep="") #nombrar aca a la ultima columna
      }
    }
    nc_close(a2)
  }

  trans = list.files(direct)
  trans = as.list(trans)
  TRANS <- as.vector(substr(trans,1,10))

  dia_inicial = ymd(substr(TRANS[1],1,8))
  dia_final = dia_inicial + days(14)

  largo_dim = dim(BRCK)[1]*dim(BRCK)[2]
  geopuntos = seq(1,largo_dim,1)

  dates = seq(dia_inicial,dia_final, by = 'days')
  dates <- as.list(dates)

  #CREO UNA MATRIZ DE DATAFRAES 3x3 a rellenar
  REALTIME = array(as.data.frame(0, colnames = c("day")), dim=c(dim(BRCK)[1],dim(BRCK)[2]))
  for (p in geopuntos) {
    REALTIME[[p]] = data.frame(matrix(vector(), 0, 2,
                                      dimnames=list(c(), c("fecha", "precipitacion"))),
                               stringsAsFactors=F)
  }

  for (k in geopuntos) {
    exdf = BRCK[[k]]
    for (k2 in dates) {
      #LINEA SIGUIENTE TIENE ERROR
      aux = dplyr::filter(exdf, ymd(`BRCK[[la, lo]]`)==ymd(paste(k2[[1]]))) #primero selecciono la fila que contiene el día que quiero analizar

      aux <- aux[ , ! apply( aux , 2 , function(x) all(is.na(x)) ) ] #borro las columnas con NA
      aux <- aux[tail(seq_along(aux),nn)] #dejo las n últimas columnas = n ultimas observaciones


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


      if (ind*nn >= nn*p ) {
        aux <- aux[ aux > tol]
        aux <- unlist(aux,use.names=F)

        mm = mean(aux)
        #acá debo agregar otro if, por que a veces mm queda < tol
        if (mm <= tol) {
          mm = 0
        }

      } else {
        mm = 0
      }

      registro = data.frame(k2, mm)
      REALTIME[[k]] <- rbind(REALTIME[[k]],registro) #se bindea como row el registro
    }
  }

est <- read.csv(paste0(auxfiles,"/est.csv"))
pred <- read.csv(paste0(auxfiles,"/pred.csv"))
hest = read.csv(paste0(auxfiles,"/hest.csv"), stringsAsFactors = FALSE)
hest$DateTime = mdy(hest$DateTime)
nombres = names(hest)
nombres = gsub('\\ ', '.', nombres)
lista_estaciones = as.list(nombres[-1])

mes_actual = ymd_hms(Sys.time())
mes_actual <- month(mes_actual)

tol=0 #tolerancia a valores observados

est.sp <- SpatialPoints(list(est$lat,est$lon))
pred.sp <- SpatialPoints(list(pred$lat,pred$lon))
est$near <- apply(spDists(est.sp,pred.sp), 1, which.min)

## POR ACA ESTA EL ERRORCITO DE LA COSA. 180 observaciones wtf
for (e in lista_estaciones) {
  e = as.character(e)
  e2 = gsub('\\.', ' ', e)

    nearest_station <- est %>%     #selecciono la mas cercana
      dplyr::filter(location == e2) %>%
      dplyr::select(near)
    near_index <- as.numeric(nearest_station) # indice del pronóstico mas cercano

      CFSv2 = REALTIME[[near_index]] %>%
        dplyr::filter(month(k2) == mes_actual)
      colnames(CFSv2) <- c("DateTime", "mm")
      C = cf %>%         #COEFICIENTES DE AJUSTE CORRESPONDIENTES
        dplyr::filter(meses == mes_actual) %>%
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

      M <- data_frame(DateTime = CFSv2$DateTime, mm = CFSv2_downscaled)


    M <- arrange(M,DateTime)
    colnames(M)[ncol(M)] <- e
    if (e == lista_estaciones[[1]]) {
      REALTIME_DOWNSCALED = M
    } else {
      M <- M[,-1]
      REALTIME_DOWNSCALED = dplyr::bind_cols(REALTIME_DOWNSCALED, M)
    }
    rm("M")
  }
REALTIME_DOWNSCALED
}

