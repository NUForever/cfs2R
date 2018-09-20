
#' Encuentra una matriz de coeficientes de ajuste gamma para la SDBC, por cada estación meteorológica y por cada mes.
#'
#' Información mas detallada.
#'
#' @return Matriz multidimensional
#'
#' @export

SDBCcfs <- function(variable, MMreforecast, auxfiles, tol=0.5) {

  est <- read.csv(paste0(auxfiles,"/est.csv"))
  pred <- read.csv(paste0(auxfiles,"/pred.csv"))
  hest = read.csv(paste0(auxfiles,"/hest.csv"), stringsAsFactors = FALSE)

  BRCK = MMreforecast

  for (i in 1:12) {
    if (i < 10) {
      assign(paste0("STATR",i), apply(BRCK, c(1,2),
                                      function(x) {
                                        x[[1]] %>%
                                          dplyr::select(dplyr::contains(paste("_0",i,"_",sep="")), 1) %>%
                                          dplyr::filter(month(`BRCK[[la, lo]]`) == i) %>%
                                          dplyr::select(-contains("BRCK")) %>%
                                          unlist(use.names = FALSE) %>%
                                          data.frame() %>%
                                          dplyr::filter_all(all_vars(. > tol))

                                      } ))
    } else {
      assign(paste0("STATR",i), apply(BRCK, c(1,2),
                                      function(x) {
                                        x[[1]] %>%
                                          dplyr::select(dplyr::contains(paste("_",i,"_",sep="")), 1) %>%
                                          dplyr::filter(month(`BRCK[[la, lo]]`) == i) %>%
                                          dplyr::select(-contains("BRCK")) %>%
                                          unlist(use.names = FALSE) %>%
                                          data.frame() %>%
                                          dplyr::filter_all(all_vars(. > tol))
                                      } ))
    }
  }

  forecast_list = list(STATR1,STATR2,STATR3,STATR4,STATR5,STATR6,
                       STATR7,STATR8,STATR9,STATR10,STATR11,STATR12)

  hest$DateTime = mdy(hest$DateTime)

  nombres = names(hest)
  nombres = gsub('\\ ', '.', nombres)
  lista_estaciones = as.list(nombres[-1]) #con punto

  #CREACION DATAFRAME A RELLENAR CON COEFICIENTES
  lista_estaciones2 = gsub('\\.', ' ', lista_estaciones)  #sin punto
  lista_estaciones2 = rep(lista_estaciones2,12)
  meses = rep(1:12, each=length(lista_estaciones))
  shapereforecast = rep(0, length(lista_estaciones2))
  scalereforecast = rep(0, length(lista_estaciones2))
  shapeobservado = rep(0, length(lista_estaciones2))
  scaleobservado = rep(0, length(lista_estaciones2))
  maxrainpron = rep(0, length(lista_estaciones2))
  maxrainobs = rep(0, length(lista_estaciones2))

  coef = data_frame(estaciones = lista_estaciones2, meses = meses,
                    shapereforecast = shapereforecast, scalereforecast = scalereforecast,
                    shapeobservado = shapeobservado, scaleobservado = scaleobservado,
                    maxrainpron = maxrainpron, maxrainobs = maxrainobs)

  tol=0 #tolerancia para registros históricos

  est.sp <- SpatialPoints(list(est$lat,est$lon))
  pred.sp <- SpatialPoints(list(pred$lat,pred$lon))
  est$near <- apply(spDists(est.sp,pred.sp), 1, which.min)   # creo la columna near para vecino mas cercano

  for (e in lista_estaciones) {

    e = as.character(e)
    e2 = gsub('\\.', ' ', e) #nombre estacion sin el con espacio en vez de punto

    nearest_station1 <- est %>%     #selecciono la mas cercana
      dplyr::filter(location == e2) %>%
      dplyr::select(near)
    nearest_station <- as.numeric(nearest_station1)

    for (mes in 1:12) {
      data_reforecast = forecast_list[[mes]][[nearest_station]]
      colnames(data_reforecast) <- "mm"
      data_reforecast = data_reforecast$mm

      registro_historico <- hest %>%
        dplyr::select(DateTime,dplyr::contains(e)) %>%
        dplyr::mutate(m = month(DateTime)) %>%
        dplyr::filter(m == mes) %>%
        dplyr::filter(get(e) > tol) %>% #OJO ACA PQ ESTOY BORRANDO LOS CEROS AL PARECER :o POSIBLE ERROR OJO NO OLVIDAR
        dplyr::select(contains(e))
      colnames(registro_historico) <- "mm"
      registro_historico <- registro_historico$mm
      #ajuste gamma con fitdistr de librería MASS

      if (length(registro_historico)<1) {
        scale.observado = NA
        shape.observado = NA
        maxobs = 0
      } else {
        param.observado <- fitdistr(registro_historico, "gamma", start=list(shape=1, scale=1), lower=0.1)
        shape.observado = param.observado$estimate["shape"]
        scale.observado = param.observado$estimate["scale"]
        maxobs = max(registro_historico)
      }

      if (length(data_reforecast)<1) {
        scale.reforecast = NA
        shape.reforecast = NA
        maxreforecast = 0
      } else {
        param.reforecast <- fitdistr(data_reforecast, "gamma", start=list(shape=1, scale=1), lower = 0.5)
        shape.reforecast = param.reforecast$estimate["shape"]
        scale.reforecast = param.reforecast$estimate["scale"]
        maxreforecast = max(data_reforecast)
      }

      coef$shapeobservado[which((coef$estaciones==e2) & (coef$meses==mes))] <- shape.observado
      coef$scaleobservado[which((coef$estaciones==e2) & (coef$meses==mes))] <- scale.observado
      coef$shapereforecast[which((coef$estaciones==e2) & (coef$meses==mes))] <- shape.reforecast
      coef$scalereforecast[which((coef$estaciones==e2) & (coef$meses==mes))] <- scale.reforecast
      coef$maxrainobs[which((coef$estaciones==e2) & (coef$meses==mes))] <- maxobs
      coef$maxrainpron[which((coef$estaciones==e2) & (coef$meses==mes))] <- maxreforecast
    }
  }
  #se retorna el df de coeficientes
  return(coef)
  }
