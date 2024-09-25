library(tidyverse)
#   simuloidaan silakka=1 kilohaili=0 yhteissaaliin lajiosuuksien estimointia
#   erinäisillä otantamenetelmillä ja erinäisillä saaliin koostumistavoilla

#   tavoitteena selvittää, millaiselella otannalla päästään samaan tulokseen riittävän usein
#   tavoitteena on päästä samaan tulokseen 10% marginaalilla

#   yksinkertaistamiseksi yhteissaaliin (catch_tot) oletetaan olevan 
#   yksikössä 
#   määrää tässä vaiheessa silakan paino ja kilohialin paino

#   pitää olla samassa yksikössä
wHer = 50     #g
wSprat = 15   #g
unit = 0.001    #g

#   otetun sankollisen paino
bucket = 7500

#   jaetaan yhteisaalis (kilot) yksiköihin
catch_tot = 50000/unit

#catch_tot = 50000 #kg


#   function for calculatin cumulative sum from binomial data
#   with assigned weights

WcumSum <- function(data, w1, w0, target){
  #   vaihdetaan 0 -> w0 ja 1 -> w1
  data[data == 1] <- w1
  data[data == 0] <- w0
  
  #   lasketaan kumulatiivinen summa
  cs <- cumsum(data)
  #   tarkistetaan, missä target tulee täyteen
  end <- which(cs>target)[1]
  
  return(end)
  #return(cs)
  
}

Wdata <- function(data , w1, w0){
  
  #   vaihdetaan 0 -> w0 ja 1 -> w1
  data[data == 1] <- w1
  data[data == 0] <- w0
  
  return(data)
  
}


#   01 danan muutos painoista kaloiksi
Wrbern <- function(n, p, w1, w0){
  
  #   lasektaan tarvittavat määrärt nollia ja ykkösiä
  n1 <- round(n*p)
  am1 <- n1/w1
  am0 <- (n-n1)/w0
  
  #   koostetaan niistä vektori
  samp_r <- c(rep(1, am1), rep(0, am0))
  
  #   sotketaan vektori
  id_r <- sample(1:length(samp_r), length(samp_r), replace = F)
  samp <- samp_r[id_r]
  
  return(samp)
}

#   funktio yksittäisen troolari - ely mittauskerran simuloimiseksi

p_est <- function(data, n_sample, unit, wHer, wSprat, target){
  
  #   montako  KALAA
  n <- length(data)
  #   data, jossa 1 ja 0 muutettu vastaaviksi painoiksi
  data_w <- Wdata(data, wHer, wSprat)
  
  n_w <- length(data_w)
  

  # eri otokset
  flag = 0              
  id_s <- list()
  p_s <- list()
  #  vedon suuruus
  size <- n_w/n_sample
  #   troolarin otokset
  #   oteteaan otos joka vedosta
  n1_tr = n0_tr = 0
  while(flag<n_sample){
    #   vedon "alku" ja "loppupisteet"
    id_1 <- flag*size +1
    id_2 <- (flag+1)*size
    
    pull <- data[round(id_1):round(id_2)]
    pull_w <- data_w[round(id_1):round(id_2)]
    #   sankko vedosta n_sample
    #samp = data[round(id_1):round(id_2)] %>% sample(.,s_sample)
    ################################################################
    #   arvo sankon ottamisen alkupiste ja laske cumsum eteenpäin
    #   monesko indeksi ylittää 7500 = loppu
    
    #   vedon alkupiste, buffer varmistaa,että sankollinen on mahdollinen kyseisestä 
    #   kohdasta i.e. sankkoon tarvittaisiin 100  kalaa, mutta kaloja on 
    #   jäljellä 50
    buffer = 500
    start <- runif(1, 1, length(pull_w) - buffer) %>% round
    #end <- WcumSum(pull, wHer, wSprat, target)
    cs_pull <- cumsum(pull_w[start:length(pull_w)])
    end <- which(cs_pull>target)[1]
    
    samp <- pull[start:(start+end)]
    s_sample = length(samp)
    #   n1_tr * muHer
    n1_tr <- n1_tr+ sum(samp) * wHer                   #   silakan paino
    n0_tr <- n0_tr + (s_sample-sum(samp)) * wSprat     #   kilohailin paino
    
    #   while loopille seurattavaa
    flag = flag+1
  }
  
  
  #   elyn otokset, ely ottaa 7.5kg / 10000kg
  #   tähän pitää tehdä muutos
  #   n on nyt kaloja
  n_ely <- ceiling(sum(data_w)/(10000/unit))
  #n_ely <- round(n/100000)
  #n_ely <- floor(n/100000)
  
  size_ely <- n_w/n_ely
  #   oteteaan otos joka 10tn
  n1_ely = n0_ely = 0
  flag = 0
  while(flag<n_ely){
    #   vedon "alku" ja "loppupisteet"
    id_1 <- flag*size_ely +1
    id_2 <- (flag+1)*size_ely

    pull <- data[round(id_1):round(id_2)]
    pull_w <- data_w[round(id_1):round(id_2)]

    
    #   vedon alkupiste, buffer varmistaa,että sankollinen on mahdollinen kyseisestä 
    #   kohdasta i.e. sankkoon tarvittaisiin 100  kalaa, mutta kaloja on 
    #   jäljellä 50
    buffer = 500
    start <- runif(1, 1, length(pull) - buffer) %>% round
    cs_pull <- cumsum(pull_w[start:length(pull_w)])
    end <- which(cs_pull>target)[1]
    
    samp <- pull[start:(start+end)]
    s_sample = length(samp)
    
    #   silakan ja kilohailin määrät sankoissa, kumulatiivine summa
    n1_ely <- n1_ely + sum(samp)*wHer                   #   silakan paino
    n0_ely <- n0_ely + (s_sample-sum(samp))*wSprat      #   kilohailin paino
    
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
  tHer_tr <- round((p1_tr*n_w)/(1000/unit),1)
  tSprat_tr <- round((p0_tr*n_w)/(1000/unit),1)
  
  tHer_ely <- round((p1_ely*n_w)/(1000/unit),1)
  tSprat_ely <- round((p0_ely*n_w)/(1000/unit),1)
  
  
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

sim_tr <- function(n_sim, catch, pulls, unit, wHer, wSprat, target){
  t1 <- Sys.time()
  #   luodaan matriisi, johon tulokset tallennetaan
  res <- matrix(nrow = n_sim, ncol = 10) %>% as.data.frame
  
  # ajetaan troolari ely mittausta simuloivaa funktiona n_sim kertaa
  for(i in 1:n_sim){
    sim <- p_est(catch, pulls, unit = unit, wHer = wHer, wSprat = wSprat, target = target)
    res[i,] <- sim
    
    if(i%%100 == 0) print(paste("Iteration:", i))
  }
  
  #res <- map_dfr(1:n_sim, ~ p_est(catch, pulls))
  #   lisää "virallinen" osuus
  colnames(res) <- c("Her_tr", "Spart_tr",
                     "Her_ely", "Sprat_ely",
                     "pHer_tr", "pSprat_tr",
                     "pHer_ely", "pSprat_ely",
                     "dHer", "dSprat")
  t2 <- Sys.time()
  
  print(t2-t1)
  
  return(res)
}


#   YKSINKERTAISIN TILANNE, SAATU SAALIS JAKAUTUU TASAISESTI    #
#################################################################

#   simuloidaan "saalisdata" (catch_1) olettaen yhteissaalliin olevan Bernoulli-
#   jakautunut silakan osuuden (p_h) mukaisesti
p_h1 <- rbeta(1, 10,1)
#   tämän sijasta fiksatut 
print(p_h1)
p_h1s <- seq(0,100, by =5)/100
seq(0,1, by = 0.05)
p_h1 <- 0.6

#   tämä on gramman yksiköissä, muuta kokonaisiksi kaloiksi muHer yms avulla
#catch1 <- rbinom(catch_tot, 1, p_h1)
catch1 <- Wrbern(catch_tot, p_h1, wHer, wSprat)



#   simuloidaan tuhat tapahtumaa, joissa troolari tekee omat mittaukset
#   ja ely omansa
sim1 <- sim_tr(100000, catch1, pulls = 5, unit = unit, wHer = wHer, wSprat = wSprat, target = bucket)
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
