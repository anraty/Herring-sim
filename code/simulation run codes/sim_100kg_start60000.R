library(tidyverse)
library(doParallel)
library(foreach)
PathOut<-"res/"
t1 <- Sys.time()
print(t1)

#   silakka simuloinnit
#   saaliit 17.5T, 60T, 120T
#   silakan osuudet 0, 0.1, 0.2, ..., 1
#   0, 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 1 
#   elyn otos <20T = 10kg, 20T-100T = 40kg, >100T = 40kg
#   tasaisesti koko saaliin matkalta

#   troolari ottaa 7.5kg / 10T
#target_tr  = 7500 # g
target_tr  = 100000 # g

#   100 000 "troolaria"
nsim = 10000

#nsim = 100

wHer = 50     #g
wSprat = 15   #g
unit = 0.001

catch_tot = 60000/unit # g

#p_s <- c(0.05, 0.1, 0.9, 0.95)
p_s <- c(0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95)
#   jotta 10% poikkeamaa sovelleteaan, lajia tulee olla saaliina vähintään
#   100kg
#   p_s ylä ja alaraja lasketaan tämän mukaisesti, olottaen pienimmän 
#   lajikohtaisen saaliin olevan 110 kg
p_smallest <- (110/unit)/catch_tot
ps <- c(p_smallest, p_s, 1-p_smallest)


#   FUNKTIOT    #
#################


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



p_est <- function(data, unit, wHer, wSprat, target){
  
  #   montako  KALAA
  n <- length(data)
  #   data, jossa 1 ja 0 muutettu vastaaviksi painoiksi
  data_w <- Wdata(data, wHer, wSprat)
  
  n_w <- length(data_w)
  
  #n_sample <- ceiling(sum(data_w) / (10000/unit))
  n_sample <- 1
  
  # eri otokset
  flag = 0              
  # id_s <- list()
  # p_s <- list()
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
    buffer = ceiling(target/wSprat)
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
  
  
  #   elyn otokset, ely ottaa taulukon mukaan, 10kg, 20kg, 40kg
  #   elyn sankko täyttyy 2.5kg kerralla
  #   n on nyt kaloja
  target_ely = 10/unit
  if(sum(data_w)<(100000/unit) & sum(data_w) >(20000/unit)) target_ely = 20/unit
  if(sum(data_w)>(100000/unit)) target_ely = 40/unit
  #   100kg lis?mittaus
  target_ely = target_ely + 100/unit
  print(target_ely)
  
  n_ely <- ceiling(target_ely/(2.5/unit))
  
  
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
    buffer = ceiling(target_ely/wSprat)
    start <- runif(1, 1, length(pull) - buffer) %>% round
    cs_pull <- cumsum(pull_w[start:length(pull_w)])
    end <- which(cs_pull>(2.5/unit))[1]
    
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
  
  # #   muunnetaan tonneiksi kymmeneosan tarkkuudella
  # tHer_tr <- round((p1_tr*n_w)/(1000/unit),1)
  # tSprat_tr <- round((p0_tr*n_w)/(1000/unit),1)
  # 
  # tHer_ely <- round((p1_ely*n_w)/(1000/unit),1)
  # tSprat_ely <- round((p0_ely*n_w)/(1000/unit),1)
  
  #   muunnetaan kiloiksi
  tHer_tr <- round((p1_tr*n_w+1)*unit)
  tSprat_tr <- round((p0_tr*n_w+1)*unit)
  
  tHer_ely <- round((p1_ely*n_w+1)*unit)
  tSprat_ely <- round((p0_ely*n_w+1)*unit)
  
  
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

sim_tr <- function(n_sim, catch, unit, wHer, wSprat, target){
  t1 <- Sys.time()
  #   luodaan matriisi, johon tulokset tallennetaan
  res <- matrix(nrow = n_sim, ncol = 10) %>% as.data.frame
  
  # ajetaan troolari ely mittausta simuloivaa funktiona n_sim kertaa
  for(i in 1:n_sim){
    sim <- p_est(catch, unit = unit, wHer = wHer, wSprat = wSprat, target = target)
    res[i,] <- sim
    
    if(i%%100 == 0) print(paste("Iteration:", i))
  }
  
  
  colnames(res) <- c("Her_tr", "Spart_tr",
                     "Her_ely", "Sprat_ely",
                     "pHer_tr", "pSprat_tr",
                     "pHer_ely", "pSprat_ely",
                     "dHer", "dSprat")
  t2 <- Sys.time()
  
  print(t2-t1)
  
  return(res)
}

#catch <- Wrbern(catch_tot, p = p_s[2], w1 = wHer, w0 = wSprat)


#######################
#   RINNAKKAISAJONA   #
#######################

#   rekisteröidään clusteri

# cluster <- makeCluster(6)
# registerDoParallel(cluster)
# 
# #   läpikäytävät indeksit
# ind <- 1:6
# 
# res_list <- list()
# #   käynnistetään
# #res_list <- foreach(i = 1:5, .export=ls(.GlobalEnv), .packages = "tidyverse") %dopar%{
# res_list <- foreach(i = ind, .packages = "tidyverse") %dopar%{
#   res_temp = list()
#   catch_i <- Wrbern(catch_tot, ps[i], wHer, wSprat)
#   
#   res_i <- sim_tr(nsim, catch_i, unit, wHer, wSprat, 7500)
#     
#   run_name_i <- paste("catch:", catch_tot*unit, "kg", "_pHer:", round(ps[i]*100, 5), "%", sep = "")
#   
#   res_temp[[run_name_i]] <- res_i
#   res_list[[i]] <- res_temp
#   
# }
# 
# stopCluster(cl = cluster)
# 
# results <- list()
# for(i in 1:length(res_list)){
#   l1 <- res_list[[i]][1]
#   
#   results[names(l1)] <- l1
# 
# }

cluster <- makeCluster(5)
registerDoParallel(cluster)

#   läpikäytävät indeksit
ind <- c(1,3,5,7,9)

res_list <- list()
#   käynnistetään
#res_list <- foreach(i = 1:5, .export=ls(.GlobalEnv), .packages = "tidyverse") %dopar%{
res_list <- foreach(i = ind, .packages = "tidyverse") %dopar%{
  res_temp = list()
  j = i+1
  catch_i <- Wrbern(catch_tot, ps[i], wHer, wSprat)
  catch_j <- Wrbern(catch_tot, ps[j], wHer, wSprat)
  
  res_i <- sim_tr(nsim, catch_i, unit, wHer, wSprat, target_tr)
  res_j <- sim_tr(nsim, catch_j, unit, wHer, wSprat, target_tr)
  
  run_name_i <- paste("catch:", catch_tot*unit, "kg", "_pHer:", round(ps[i]*100, 5), "%", sep = "")
  run_name_j <- paste("catch:", catch_tot*unit, "kg", "_pHer:", round(ps[j]*100, 5), "%", sep = "")
  
  res_temp[[run_name_i]] <- res_i
  res_temp[[run_name_j]] <- res_j
  res_list[[i]] <- res_temp
  #res_list[j] <- res_j
  
}

stopCluster(cl = cluster)

results <- list()
for(i in 1:length(res_list)){
  l1 <- res_list[[i]][1]
  l2 <- res_list[[i]][2]
  
  results[names(l1)] <- l1
  results[names(l2)] <- l2
}



saveRDS(results, paste(PathOut,"res_100kg_", catch_tot*unit, "kg.rds", sep = ""))
print(time_length(Sys.time()-t1, unit = "hours"))