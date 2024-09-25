data <- Wrbern(catch_tot, p_h1, wHer, wSprat)

n_sample = 3
length(data)

n <- length(data)
#   data, jossa 1 ja 0 muutettu vastaaviksi painoiksi
data_w <- Wdata(data, wHer, wSprat)
sum(data_w)

n_w <- length(data_w)


# eri otokset
flag = 0              
#  vedon suuruus
size <- (n_w/n_sample) %>% round
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
  end <- which(cs_pull>7500)[1]
  
  samp <- pull[start:(start+end)]
  sum(pull_w[start:(start+end)])
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

size_ely <- n/n_ely
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
  cs_pull <- cumsum(pull_w)
  end <- which(cs_pull>target)[1]
  
  samp <- pull[start:end]
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





a <- 70500
b <- 64500
b-a

(a-b)/(b+9400)
