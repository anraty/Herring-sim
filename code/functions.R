
#   funktio tulostaulun rakentamisesksi

cl_tbl <- function(res_list){
  p_s <- sub(".*pHer:", "", names(res_list))
  p_num <- p_s %>% gsub("%","", .) %>% as.numeric()
  df <- c()
  
  for(i in 1:length(res_list)){
    #   valitaan oikea df listasta
    res_i <- res_list[[i]]
    n = length(res_i)
    
    #   valitaan tarvittavat sarakkeet
    pHer_tr <- res_i$pHer_tr
    pSprat_tr <- res_i$pSprat_tr
    pHer_ely <- res_i$pHer_ely
    pSprat_ely <- res_i$pSprat_ely
    #   lasketaan erotukset uusiksi
    # dHer <- ((pHer_ely+10^-5)-(pHer_tr+10^-5))/(pHer_ely+10^-5)
    # dSprat <- ((pSprat_ely+10^-5)-(pSprat_tr+10^-5))/(pSprat_ely+10^-5)
    dHer <- ((pHer_ely)-(pHer_tr))/(pHer_ely)
    dSprat <- ((pSprat_ely)-(pSprat_tr))/(pSprat_ely)
    #   indikaattorit jos erotus yli +-10%    
    iHer_r <- as.numeric(abs(dHer)>0.1)
    iSprat_r <- as.numeric(abs(dSprat)>0.1)
    iHer <- iHer_r %>% replace_na(1)
    iSprat <- iSprat_r %>% replace_na(1)
    iTot <- ((iHer+iSprat)>0) %>% as.numeric
    
    df_i <- data.frame(TpHer = rep(p_num[i]/100, n), pHer_tr, pHer_ely, pSprat_tr, 
                       pSprat_ely, dHer, dSprat, iHer, iSprat, iTot)
    if(i == 1) df <- df_i
    else df <- rbind(df,df_i)
    
  }
  
  
  return(df)
  
}




#   funktio tulostalulun tekemiseksi
#   tulostaulussa on sarakkeet kummallekin erotukselle
#   niiden 90% lv ja osuus sakoista kummankin lajin kohdalla

res_tbl <- function(data){
  Tp <- unique(data$TpHer)
  df <- c()
  for(p in Tp){
    di <- data %>% filter(TpHer == p)
    n <- nrow(di)
    fHer <- sum(di$iHer)/n
    fSprat <- sum(di$iSprat)/n
    fTot <- sum(di$iTot) / n
    
    df <- rbind(df, c(p, 1-p, fHer, fSprat, fTot))
    
  }
  cn <- c("pHer", "pSprat", "fHer", "fSprat", "fTot")
  df <- as.data.frame(df)
  colnames(df) <- cn
  return(df)
  
}


#   plottausfunktiot

plot_sakko <- function(data, saalis){
  ggplot(data = data)+
    geom_line(aes(x = pHer, y = fHer, col = "Silakan ero >10%"))+
    geom_line(aes(x = pHer, y = fSprat, col = "Kilohailin ero >10%"))+
    geom_line(aes(x = pHer, y = fTot, col = "Silakan tai kilohailin ero >10%"),
              lty = 2, lwd =1.5, alpha = 0.5)+
    scale_color_manual(values = c("Silakan ero >10%" = "blue",
                                  "Kilohailin ero >10%" = "red",
                                  "Silakan tai kilohailin ero >10%" = "black"))+
    labs(color = "", x = "Silakan osuus saaliissa", y = "Simulaatioiden osuus, joissa >10% poikkeama")+
    ggtitle(paste("Yhteissaalis ", (saalis/1000), "T", sep=""))
}


plot_dens <- function(data, saalis, adjust=10){
  #   lista kuvien tallennukseen
  pl_ls <- list()
  
  
  for(p in unique(data$TpHer)){
    data_m <- data %>% filter(TpHer == p)
    plHer <- 
      ggplot(data = data_m)+
      geom_density(aes(pHer_tr, color = "Kalastaja"), adjust = adjust)+
      geom_density(aes(pHer_ely, color = "ELY"), adjust = adjust)+
      geom_vline(xintercept = p, lty = 2)+
      scale_color_manual(values= c(
        "Kalastaja" = "blue",
        "ELY" = "red"
      ))+
      labs(color = "", x = "Silakan osuus saaliissa", y = "")+
      ggtitle(paste("Yhteissaalis ", (saalis/1000), "T", sep=""))
    
    plSprat <- 
      ggplot(data = data_m)+
      geom_density(aes(pSprat_tr, color = "Kalastaja"), adjust = adjust)+
      geom_density(aes(pSprat_ely, color = "ELY"), adjust = adjust)+
      geom_vline(xintercept = (1-p), lty = 2)+
      scale_color_manual(values= c(
        "Kalastaja" = "blue",
        "ELY" = "red"
      ))+
      labs(color = "", x = "Kilohailin osuus saaliissa", y = "")+
      ggtitle(paste("Yhteissaalis ", (saalis/1000), "T", sep=""))
    
    pl <- arrangeGrob(plHer, plSprat, ncol = 2)
    
    pl_ls[[paste("pHer: ", p, sep  ="")]] <- pl
    
  }
  return(pl_ls)
}



