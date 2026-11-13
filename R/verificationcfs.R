
#' Encuentra matriz de estadígrafos de bondad de ajuste, para cada estación meteorológica y por cada mes.
#'
#' Información mas detallada.
#'
#' @return Dataframe
#'
#' @export


verificationcfs <- function(variable = "prate", coef, auxfiles, modelcfsoperational) {

  cf <- coef
  hest = read.csv(paste0(auxfiles,"/hest.csv"), stringsAsFactors = FALSE)
  hest$DateTime = mdy(hest$DateTime)
  nombres = names(hest)
  nombres = gsub('\\ ', '.', nombres)
  lista_estaciones = as.list(nombres[-1])

  dia_inicial = modelcfsoperational$DateTime[[1]]
  dia_final = modelcfsoperational$DateTime[[dim(FORECASToperational)[1]]]

  V = data_frame(ind = c("ME","MAE","MSE","RMSE","NRMSE %","PBIAS %",
                         "RSR","rSD","NSE","mNSE","rNSE","d","md",
                         "rd","cp","r","R2","bR2","KGE","VE"))

  for (e in lista_estaciones) {
    e = as.character(e)
    e2 = gsub('\\.', ' ', e)

    dfope = modelcfsoperational %>%
      dplyr::select(1,e)
    dfope = data.frame(dfope)

    dfobs = hest %>%
      dplyr::select(DateTime,dplyr::contains(e)) %>%
      dplyr::filter(DateTime >= dia_inicial & DateTime <= dia_final)
    names(dfobs)[2] <- "mm"
    dfobs[dfobs<as.numeric(-1)] <- NA
    dfobs[dfobs<0.5] <- 0

    #AGREGACIÓN
    xtsope <- xts(dfope[,-1], order.by=dfope[,1])
    #xtsope <- apply.weekly(xtsope,sum)
    xtsope <- apply.monthly(xtsope,sum)
    #xtsope <- apply.yearly(xtsope,sum)
    xtsobs <- xts(dfobs[,-1], order.by=dfobs[,1])
    #xtsope <- apply.weekly(xtobs,sum)
    xtsobs <- apply.monthly(xtsobs,sum)
    #xtsobs <- apply.yearly(xtsobs,sum)

    I = data.frame(gof(sim=xtsope,obs=xtsobs,na.rm = TRUE))
    colnames(I) <- e
    V = cbind(V,I)

  }

  V2 = as.data.frame(t(V), stringsAsFactors = F)
  V2 = mutate_all(V2, as.numeric)
  colnames(V2) <- c("ME","MAE","MSE","RMSE","NRMSE%","PBIAS%",
                    "RSR","rSD","NSE","mNSE","rNSE","d","md",
                    "rd","cp","r","R2","bR2","KGE","VE")

  V2 = V2[-1,]
  rownames(V2) <- lista_estaciones
  V2
}
