#' Crea matriz multidimensional de archivos operacionales del modelo CFSv2
#'
#' Información mas detallada.
#'
#' @return Matriz multidimensional, donde cada elemento es un dataframe asociado a una coordenada.
#'
#' @export


MMoperational <- function(variable = "prate",direct) {
lista <- system.file("extdata", "lista_operational.csv", package = "cfs2R")
trans <- read.csv(lista)
TRANS <- as.vector(trans$lista)

##################SE ENCUENTRA EL NUMÉRO MENOR DE PRONOSTICOS HACIA ADELANTE EN TODOS LOS FORECAST #########3

vec = c()
for (k in TRANS) {
  a2 = nc_open(paste0(direct,"/",k,"_cortado.nc",sep=""))
  prate = ncvar_get(a2,variable,start=c(1,2,1),count=c(1,1,-1))*6*3600 #OJO ACA! como esta tomando la grilla??

  tp = ncvar_get(a2,"time")#1 obtener fecha inicial desde el ncdf
  tunitsp = ncatt_get(a2,"time","units")
  tustrp <- strsplit(tunitsp$value, " ")
  fini = unlist(tustrp)[3]
  hini = unlist(tustrp)[4]

  dh = ymd_hms(paste(fini,hini,sep=" "), tz = "UTC") - hours(18) + 6*hours(0:(length(tp)-1))#conversion a "chile" : las 00 están a las 8am
  serie <- data.frame(dh, prate)
  xtsserie = xts(serie[,-1], order.by=serie[,1])#3 convertir a xts
  xtsserie <- apply.daily(xtsserie,sum) #4 agregar a nivel diario
  df = data.frame(xtsserie) #6 convertir a df

  vec <- c(vec,max(dim(df)))
  nc_close(a2)
}
######1)  VALOR MINIMO DE PRONÓSTICOS EN UN ARCHIVO... TODOS DEBEN TENER EL MISMO LARGO
m = min(vec)
mm = max(vec)

lina<- read.csv(system.file("extdata", "lista_operationalAUX.csv", package = "cfs2R"))
largo_lina = max(dim(lina))

# ACÁ TENGO QUE GENERALIZAR ESTA MIERDA !
N_A = rep(0:largo_lina, each=4, len=(largo_lina+3))
N_A <- as.data.frame(N_A)
N_A <- as.data.frame(N_A[-c(1, 2, 3,
                            285,585, #2011 miss
                            1344,1478,1526,1541,1561,1562,1574,1979,1981,1987, #2012
                            2698,2705,3107,3108,3109,3110,3181,3182,3233,3234,3235,3236,3237,3239,3240,3241,3335, #2013
                            5365, #2014
                            6822, #2015
                            7276,7615,7659,7687,8389,8391), ]) #2016 #es index +3 pq hay 3 numeros de mas

################################ CREACION DE MATRIZ MULTIDIMENSIONAL ###################################
######2) contar los archivos NN
NN = length(vec)
vecNC = vec+N_A

LM = max(vecNC)

NAarriba=N_A

######3) crear vector de horas VH de largo LM
KK = TRANS[1]
a2 = nc_open(paste0(direct,"/",KK,"_cortado.nc",sep=""))
tp = ncvar_get(a2,"time")#1 obtener fecha inicial desde el ncdf
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
      dh = ymd_hms(paste(fini,hini,sep=" "), tz = "UTC") - hours(18) + 6*hours(0:(length(tp)-1))#conversion a "chile" : las 00 están a las 8am chilenas

      prate = ncvar_get(a2,variable,start=c(lo,la,1),count=c(1,1,-1))*6*3600 # transformacion unidades: 6 horas acum

      serie <- data.frame(dh, prate)
      xtsserie = xts(serie[,-1], order.by=serie[,1])#3 convertir a xts
      xtsserie <- apply.daily(xtsserie,sum) #4 agregar a nivel diario
      df = fortify(xtsserie) #fortify funciona la raja, pertenece a la libreria ggplot2 LLAMAR antes !
      #colnames(df) <- c(paste("Fecha",archivo,sep=""),paste("PRATE",archivo,sep=""))

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
MMoperational = BRCK
MMoperational
}
