library(tidyverse)
#   simuloidaan silakka=1 kilohaili=0 yhteissaaliin lajiosuuksien estimointia
#   erinäisillä otantamenetelmillä ja erinäisillä saaliin koostumistavoilla

#   tavoitteena selvittää, millaiselella otannalla päästään samaan tulokseen riittävän usein
#   tavoitteena on päästä samaan tulokseen 10% marginaalilla

#   yksinkertaistamiseksi yhteissaaliin (catch_tot) oletetaan olevan 
#   yksikössä 
#   määrää tässä vaiheessa silakan paino ja kilohialin paino
mu_Her = 50
mu_Sprat = 15
unit = 0.001 #kg

#   jaetaan yhteisaalis (kilot) yksiköihin
catch_tot = 50000/unit


#   funktio yksittäisen troolari - ely mittauskerran simuloimiseksi

p_est <- function(data, n_sample, unit){
  
  #   montako yksikköä
  n <- length(data)

  
  #   otoskoon lasekminen, montako yksikköä 7.5kg ämpärissä
  #s_sample = 7.5/unit
  
  # eri otokset
  flag = 0              
  id_s <- list()
  p_s <- list()
  #  vedon suuruus
  size <- n/n_sample
  #   troolarin otokset
  #   oteteaan otos joka vedosta
  n1_tr = n0_tr = 0
  while(flag<n_sample){
    #   vedon "alku" ja "loppupisteet"
    id_1 <- flag*size +1
    id_2 <- (flag+1)*size
    #   sankko vedosta n_sample
    #samp = data[round(id_1):round(id_2)] %>% sample(.,s_sample)
    ################################################################
    #   arvo sankon ottamisen alkupiste ja laske cumsum eteenpäin
    #   monesko indeksi ylittää 7500 = loppu

    
    #   silakan ja kilohailin määrät sankoissa, kumulatiivine summa
    
    #   n1_tr * muHer
    n1_tr <- n1_tr+ sum(samp)      #   silakan yksiköt
    n0_tr <- n0_tr + (s_sample-sum(samp))     #   kilohailin yksiköt

    #   while loopille seurattavaa
    flag = flag+1
  }
  
  
  #   elyn otokset, ely ottaa 7.5kg / 10000kg
  n_ely <- ceiling(n/(10000/unit))
  #n_ely <- round(n/100000)
  #n_ely <- floor(n/100000)
  
  size_ely <- n/n_ely
  #   oteteaan otos joka 10tn
  n1_ely = n0_ely = 0
  flag = 0
  while(flag<n_ely){
    #   vedon "alku" ja "loppupisteet"
    id_1 <- flag*size_ely +1
    id_2 <- (flag+1)*size_ely
    #   sankko vedosta n_sample
    
    #   tähän while jossa sankkoa täytetään "kokonaisilla kaloilla"
    #   kunnes 7.5kg täynnä 
    samp = data[round(id_1):round(id_2)] %>% sample(.,s_sample)
    
    #   silakan ja kilohailin määrät sankoissa, kumulatiivine summa
    n1_ely <- n1_ely + sum(samp)       #   silakan yksiköt
    n0_ely <- n0_ely + (s_sample-sum(samp))     #   kilohailin yksiköt
    
    #   while loopille seurattavaa
    flag = flag+1
  }
  
  #   palauteteen elyn ja troolarin tulokset ja niiden erotus
  #   muuteteaan takaisin yksilöiksi
  p1_tr <- n1_tr / (n1_tr + n0_tr)
  p0_tr <- 1-p1_tr
  
  p1_ely <- n1_ely / (n1_ely + n0_ely)
  p0_ely <- 1-p1_ely
  
  #   muunnetaan tonneiksi kymmeneosan tarkkuudella
  tHer_tr <- round((p1_tr*n)/(1000/unit),1)
  tSprat_tr <- round((p0_tr*n)/(1000/unit),1)
  
  tHer_ely <- round((p1_ely*n)/(1000/unit),1)
  tSprat_ely <- round((p0_ely*n)/(1000/unit),1)
  
  
  return(c(n1_tr, n0_tr, 
           n1_ely, n0_ely, 
           p1_tr, p0_tr, 
           p1_ely, p0_ely,
           # (tHer_tr-tHer_ely)/((tHer_tr+tHer_ely)/2),
           # (tSprat_tr-tSprat_ely)/((tSprat_tr+tSprat_ely)/2)))
           (tHer_tr-tHer_ely)/(tHer_ely),
           (tSprat_tr-tSprat_ely)/(tSprat_ely)))

  
}


#   funtion n tilanteen simuloimiseksi

sim_tr <- function(n_sim, catch, pulls, unit){
  #   luodaan matriisi, johon tulokset tallennetaan
  res <- matrix(nrow = n_sim, ncol = 10) %>% as.data.frame
  
  # ajetaan troolari ely mittausta simuloivaa funktiona n_sim kertaa
  for(i in 1:n_sim){
    sim <- p_est(catch, pulls, unit = unit)
    res[i,] <- sim
  }
  
  #res <- map_dfr(1:n_sim, ~ p_est(catch, pulls))
  
  colnames(res) <- c("Her_tr", "Spart_tr",
                     "Her_ely", "Sprat_ely",
                     "pHer_tr", "pSprat_tr",
                     "pHer_ely", "pSprat_ely",
                     "dHer", "dSprat")
  
  return(res)
}


#   YKSINKERTAISIN TILANNE, SAATU SAALIS JAKAUTUU TASAISESTI    #
#################################################################

#   simuloidaan "saalisdata" (catch_1) olettaen yhteissaalliin olevan Bernoulli-
#   jakautunut silakan osuuden (p_h) mukaisesti
#p_h1 <- rbeta(1, 10,1)
#   tämän sijasta fiksatut 
print(p_h1)
p_h1s <- seq(0,100, by =5)/100
seq(0,1, by = 0.05)
p_h1 <- 1

#   tämä on gramman yksiköissä, muuta kokonaisiksi kaloiksi muHer yms avulla
catch1 <- rbinom(catch_tot, 1, p_h1)
#   simuloidaan tuhat tapahtumaa, joissa troolari tekee omat mittaukset
#   ja ely omansa
sim1 <- sim_tr(1000, catch1, pulls = 3, unit = unit)
#   katsotaan kuinka monessa mittauksessa erotus on yli 10% 
table(abs(sim1$dHer)>0.1)
table(abs(sim1$dSprat)>0.1)





#   SAATU SAALIS JAKAUTUU SATUNNAISESTI PER TROOLIKERTA    #
############################################################

#   simuloidaan "saalisdata" (catch_2) olettaen saalliin olevan Bernoulli-
#   jakautunut silakan osuuden (p_h2) mukaisesti per trooli kerta

#   viisi troolikertaa
p_h2s <- rbeta(5, 2,2)


print(p_h2s)

catch2 <- c(rbinom(catch_tot/5, 1, p_h2s[1]),
            rbinom(catch_tot/5, 1, p_h2s[2]),
            rbinom(catch_tot/5, 1, p_h2s[3]),
            rbinom(catch_tot/5, 1, p_h2s[4]),
            rbinom(catch_tot/5, 1, p_h2s[5]))
p_h2 <- sum(catch2)/length(catch2)
print(p_h2)

sim2 <- sim_tr(1000, catch2, length(p_h2s), unit = unit)
table(abs(sim2$dHer)>0.1)
table(abs(sim2$dSprat)>0.1)
