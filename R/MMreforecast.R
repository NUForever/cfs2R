#' Crea matriz multidimensional de archivos retrospectivos del modelo CFSv2
#'
#' Información mas detallada.
#'
#' @return Matriz multidimensional, donde cada elemento es un dataframe asociado a una coordenada.
#'
#' @export

MMreforecast <- function(variable="prate",direct) {

  #####Acá se lee la lista de archivos.
  lista <- system.file("extdata", "lista_reforecast.csv", package = "cfs2R", mustWork = TRUE)
  trans <- read.csv(lista)

  #####transformo la lista a vector, para que el ciclo for pueda recorrerla.
  TRANS <- as.vector(trans$lista)
  #####A continuación se encuentra el menor número de pronósticos hacia adelante, ya que este va a ser el valor
  #####máximo de largo que tendrán todos los pronósticos.

  f1 = ymd_h(TRANS[1])
  f2 = ymd_h(TRANS[length(TRANS)])
  intervalo = f1 %--% f2
  LM = round(intervalo/ddays(1)) + 500 # le sumo 500 para que no se quede corto el VH(corre para reforecast)
  VH = ymd(f1, tz = "UTC") - days(1) + days(0:(LM-1))
  VH = as.data.frame(VH)
  colnames(VH) <- c("day")

  #####Se crea un vector vacío, donde se van a rellenar los largos de cada vector

  vec_NA = c()
  for (k in TRANS) {

    k2 = substr(k, 1, 8) #año mes dia
    hora_archivo = as.integer(substr(k, 9,10))
    file_idx = which(VH$day==ymd(k2))

    if (hora_archivo==18) {
      cantidad_NA = file_idx -1
      vec_NA <- c(vec_NA,cantidad_NA)
    } else {
      cantidad_NA = file_idx-2
      vec_NA <- c(vec_NA,cantidad_NA)
    }

  }

  # Se extrae la información relevante del primer archivo .nc
  KK = TRANS[1]
  a2 = nc_open(paste0(direct,"/",KK,"_cortado.nc",sep=""))
  # Se extrae el vector de longitudes que trae el archivo
  lon = ncvar_get(a2,"lon")
  # pero viene de la siguiente manera:
  # 286.090 287.028 287.966 288.904 289.842 290.780 291.718
  # por lo que se aplicará la siguiente conversión para que queden de la forma habitual:
  lon <- c(rep(1,(length(lon)))*360-lon)*-1    #esta wea esta mala
  #-73.910 -72.972 -72.034 -71.096 -70.158 -69.220 -68.282

  # También se extrae el vector de latitudes, que viene de forma habitual:
  lat = ncvar_get(a2,"lat")
  # -31.65342 -32.59830 -33.54318 -34.48805 -35.43293

  # Revierto el orden de las latitudes. NO RECUERDO EL POR QUE
  # CREO QUE NO AFECTA EN NADA, PERO NO OLVIDAR
  lat <- rev(lat)
  # -35.43293 -34.48805 -33.54318 -32.59830 -31.65342

  # Se crea un vector de índices que luego recorreremos
  laidx = seq(1, length(lat), 1)
  # 1 2 3 4 5
  loidx = seq(1, length(lon), 1)
  # 1 2 3 4 5 6 7

  #listapuntos = expand.grid(laidx,loidx), recordar esta forma
  # Cerramos el archivo .nc que abrimos, ya le sacamos toda la información necesaria.
  nc_close(a2)

  ### Esta linea la agrego solamente por que estoy usando una grilla menor (3x3) a la completa (7x5), el corte
  # para la otra debe tomar solo la grilla de 3x3.
  #laidx <- c(1,2,3)
  #loidx <- c(1,2,3)

  # Se crea la matriz multidimensional BRCK de 3x3, que contiene dataframes para cada punto, de momento esos
  # data frames solo contienen las fechas.
  BRCK = array(as.data.frame(VH, colnames = c("day")), dim=c(length(laidx),length(loidx)))
  for (lo in loidx) {
    for (la in laidx) {
      as.data.frame(BRCK[[la,lo]])
    }
  }

  # Ahora se procede a rellenar esos data frames con cada pronóstico que le corresponda, para ello
  # se recorre nuevamente la lista de archivos TRANS que contiene los nombres de los archivos .nc
  for (archivo in TRANS) {
    # Primero se extrae la información correspondiente al Año, Mes y Dia-Hora de cada archivo
    AAño = substr(archivo, 1, 4)
    MMes = substr(archivo, 5, 6)
    DDiahora = substr(archivo, 7, 10)


    # se abre el archivo
    a2 = nc_open(paste0(direct,"/",archivo,"_cortado.nc",sep=""))

    #Ahora se van a empezar a recorrer los puntos.
    for (lo in loidx) {
      for (la in laidx) {
        # Se obtiene la fecha inicial del archivo, como también la hora inicial, igual que antes.
        tp = ncvar_get(a2,"time")
        tunitsp = ncatt_get(a2,"time","units")
        tustrp <- strsplit(tunitsp$value, " ")
        fini = unlist(tustrp)[3]
        hini = unlist(tustrp)[4]
        # Igual que antes, se crea un vector del mismo largo que los pronósticos, que va a ir de 6 en 6 horas.
        # Se convierten las horas a Chile, 00 a las 8am Chilenas.
        dh = ymd_hms(paste(fini,hini,sep=" "), tz = "UTC") - hours(12) + 6*hours(0:(length(tp)-1))
        # Se extrae la información, OJO que acá se está partiendo de lo+2 y la+1 para solo tomar el cuadro de 3x3 que encierra a la
        # región en cuestión.
        # Se transforman las unidades, ya que se encuentra en precipitación por segundo, y hay que pasar a precipitación acumulada en 6 horas
        prate = ncvar_get(a2,paste0(variable),start=c(lo,la,1),count=c(1,1,-1))*6*3600 #ojo aca que el lo+2 y la+1 es pq estoy tomando grilla menor
        # se crea una data frame con el vector de días-horas dh y el vector de pronósticos prate.
        serie <- data.frame(dh, prate)
        # se transforma a un objeto xts, que es se puede agregar de forma mas fácil a nivel diario, como suma.
        xtsserie = xts(serie[,-1], order.by=serie[,1])
        xtsserie <- apply.daily(xtsserie,sum)
        # Para volver a data frame se utiliza la función fortify de la librería ggplot2
        df = fortify(xtsserie)

        # Ya agregado a nivel diario, se extrae esta prate, y se guarda en un vector.
        FCprate_diario = df$xtsserie

        # A continuación se crean los vectores con NA al principio y final correspondiente a cada archivo
        archivo <- as.integer(archivo)
        TRANS <- as.vector(TRANS)
        # Si el archivo que se esta leyendo es el primero. No se agregan NAs al principio, solo al final.

        if (archivo==TRANS[1] | archivo==TRANS[2] | archivo==TRANS[3]) {

          NA_final = rep(NA, LM - length(FCprate_diario))
          FCprate_diario_plusNA = c(FCprate_diario,NA_final)

        } else {

          # Se encuentran cuantos son los NA que van al final
          NA_final = rep(NA, LM - vec_NA[which(TRANS==archivo)] -length(FCprate_diario))
          # Se encuentran cuantos son los NA que van al principio
          NA_principio = rep(NA, vec_NA[which(TRANS==archivo)])
          # Se agregan todos los NA al registro
          FCprate_diario_plusNA =  c(NA_principio,FCprate_diario,NA_final)

        }

        # Finalmente se agregan estos valores al BRCK correspondiente al punto que se esta viendo.
        BRCK[[la,lo]] <- cbind.data.frame(BRCK[[la,lo]],FCprate_diario_plusNA)
        # Se le cambia el nombre a ultima columna para que sea el Año_Mes_DiaHora que corresponde al archivo
        names(BRCK[[la,lo]])[length(names(BRCK[[la,lo]]))]<- paste(AAño,"_",MMes,"_",DDiahora,sep="") #nombrar aca a la ultima columna
      }
    }
    nc_close(a2)
  }
  MMreforecast = BRCK
  MMreforecast
}
