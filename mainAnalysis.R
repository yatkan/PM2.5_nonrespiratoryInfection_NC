##################################################################
##################################################################
#######    Code for PM2.5 and non-respiratory infection   ########
#######            author: Yijing Feng                    ########
#######                                                   ########
#######                                                   ########
##################################################################
##################################################################
##################################################################

library(tableone)
library(gWQS)
library(dplyr)
library(fst)
library(stringr)
library(sandwich)
library(lmtest)
library(metafor)

aggDat100 <- readRDS("demoDat.rds" )


##################################################################
#### association between total PM2.5 and non-respiratory infection
##################################################################

## total pm2.5 and total non-respiratory infection
model1 <- glm(nonrspInfDiag1 ~ pm25+pct_female_mdc+
                pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic
              +popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate
              +education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count)),family = "quasipoisson", data = aggDat100)
summary(model1)

varest <- sqrt(sandwich(model1))[2,2]

exp(model1$coefficients[2])
exp(model1$coefficients[2]+1.96*varest)
exp(model1$coefficients[2]-1.96*varest)

## total pm2.5 and CNS infection
model2 <- glm(cnsInfDiag1 ~ pm25+pct_female_mdc+
                pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic
              +popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate
              +education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count)),family = "quasipoisson", data = aggDat100)
summary(model2)


varest <- sqrt(sandwich(model2))[2,2]
exp(model2$coefficients[2])
exp(model2$coefficients[2]+1.96*varest)
exp(model2$coefficients[2]-1.96*varest)


## total pm2.5 and intestinal infection
model3 <- glm(intestinalDiag1 ~ pm25+pct_female_mdc+
                pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic
              +popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate
              +education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count)),family = "quasipoisson", data = aggDat100)
summary(model3)

varest <- sqrt(sandwich(model3))[2,2]
exp(model3$coefficients[2])
exp(model3$coefficients[2]+1.96*varest)
exp(model3$coefficients[2]-1.96*varest)

## ## total pm2.5 and urinary tract infection
model4 <- glm(urinDiag1 ~ pm25+pct_female_mdc+
                pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic
              +popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate
              +education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count)),family = "quasipoisson", data = aggDat100)
summary(model4)

varest <- sqrt(sandwich(model4))[2,2]
exp(model4$coefficients[2])
exp(model4$coefficients[2]+1.96*varest)
exp(model4$coefficients[2]-1.96*varest)

## total pm2.5 and septicemia
model5 <- glm(septicDiag1 ~ pm25+pct_female_mdc+
                pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic
              +popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate
              +education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count)),family = "quasipoisson", data = aggDat100)
summary(model5)

varest <- sqrt(sandwich(model5))[2,2]
exp(model5$coefficients[2])
exp(model5$coefficients[2]+1.96*varest)
exp(model5$coefficients[2]-1.96*varest)


##################################################################
###### evaluate the association between total pm2.5 and 
###### non-respiratory infection at levels below 9 ug/m3
##################################################################
aggDat100_below9 <- filter(aggDat100, pm25<=9)

## total pm2.5 and total non-respiratory infection (at levels below 9 ug/m3)
model1 <- glm(nonrspInfDiag1 ~ pm25+pct_female_mdc+
                pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic
              +popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate
              +education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count)),family = "quasipoisson", data = aggDat100_below9)
summary(model1)

varest <- sqrt(sandwich(model1))[2,2]
exp(model1$coefficients[2])
exp(model1$coefficients[2]+1.96*varest)
exp(model1$coefficients[2]-1.96*varest)


## total pm2.5 and total CNS infection (at levels below 9 ug/m3)
model2 <- glm(cnsInfDiag1 ~ pm25+pct_female_mdc+
                pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic
              +popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate
              +education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count)),family = "quasipoisson", data = aggDat100_below9)
summary(model2)

varest <- sqrt(sandwich(model2))[2,2]

exp(model2$coefficients[2])
exp(model2$coefficients[2]+1.96*varest)
exp(model2$coefficients[2]-1.96*varest)


## total pm2.5 and intestinal infection (at levels below 9 ug/m3)
model3 <- glm(intestinalDiag1 ~ pm25+pct_female_mdc+
                pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic
              +popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate
              +education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count)),family = "quasipoisson", data = aggDat100_below9)
summary(model3)

varest <- sqrt(sandwich(model3))[2,2]
exp(model3$coefficients[2])
exp(model3$coefficients[2]+1.96*varest)
exp(model3$coefficients[2]-1.96*varest)

## total pm2.5 and urinary tract infection (at levels below 9 ug/m3)
model4 <- glm(urinDiag1 ~ pm25+pct_female_mdc+
                pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic
              +popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate
              +education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count)),family = "quasipoisson", data = aggDat100_below9)
summary(model4)

varest <- sqrt(sandwich(model4))[2,2]
exp(model4$coefficients[2])
exp(model4$coefficients[2]+1.96*varest)
exp(model4$coefficients[2]-1.96*varest)

## total pm2.5 and septicemia (at levels below 9 ug/m3)
model5 <- glm(septicDiag1 ~ pm25+pct_female_mdc+
                pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic
              +popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate
              +education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count)),family = "quasipoisson", data = aggDat100_below9)
summary(model5)

varest <- sqrt(sandwich(model5))[2,2]

exp(model5$coefficients[2])
exp(model5$coefficients[2]+1.96*varest)
exp(model5$coefficients[2]-1.96*varest)



##########################
########################## particulate components and non-respiratory infection
##########################

## scale the covariates for the weighted quantile sum analysis
aggDat100$pct_female_mdc <- scale(aggDat100$pct_female_mdc)
aggDat100$pct_young_mdc <- scale(aggDat100$pct_young_mdc)
aggDat100$pct_mid_mdc <- scale(aggDat100$pct_mid_mdc)
aggDat100$pct_black_mdc <- scale(aggDat100$pct_black_mdc)

aggDat100$pct_white_mdc <- scale(aggDat100$pct_white_mdc)
aggDat100$popdensity <- scale(aggDat100$popdensity)
aggDat100$nearest_hospital_km <- scale(aggDat100$nearest_hospital_km)
aggDat100$amb_visit_pct <- scale(aggDat100$amb_visit_pct)

aggDat100$poverty <- scale(aggDat100$poverty)
aggDat100$medhouseholdincome <- scale(aggDat100$medhouseholdincome)
aggDat100$smoke_rate <- scale(aggDat100$smoke_rate)
aggDat100$education <- scale(aggDat100$education)

aggDat100$hispanic <- scale(aggDat100$hispanic)

aggDat100$no2 <- scale(aggDat100$no2)
aggDat100$ozone <- scale(aggDat100$ozone)
aggDat100$summer_tmmx <- scale(aggDat100$summer_tmmx)
aggDat100$winter_tmmx <- scale(aggDat100$winter_tmmx)


################################################
#############  evaluate the association between 
#############  particulate component and outcome 
#############  using weighted quantile sum 
#############  regression
################################################

mixture <- names(aggDat100)[c(31:45)]


### pm2.5 components and total non-respiratory infection
totalInf <- gwqs(nonrspInfDiag1 ~ wqs+pct_female_mdc+pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic
                 +popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate
                 +education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count)), mix_name = mixture
                 , data = aggDat100, q = 10, 
                 validation = 0.6,b=100, b1_pos = T, b1_constr =T, 
                 family = "quasipoisson",seed = 123)
summary(totalInf)
totalInf$final_weights

# obtain the robust variance estimator

form <- paste0( names(totalInf$data)[1], "~ wqs + pct_female_mdc + pct_young_mdc + pct_mid_mdc + pct_black_mdc + pct_white_mdc + hispanic + popdensity + nearest_hospital_km + amb_visit_pct + poverty + medhouseholdincome + smoke_rate + education + as.factor(year) + pct_dual + winter_tmmx + summer_tmmx + offset(log(count))")

modelDat <- totalInf$data[totalInf$vindex,]
model1 <- glm(formula = form, family = "quasipoisson",
              data = modelDat)
est <- model1$coefficients[2]
ll <- confint(coeftest(model1, vcov = vcovHC,type = "HC1"))[2,1]
ul <- confint(coeftest(model1, vcov = vcovHC,type = "HC1"))[2,2] 


### pm2.5 components and CNS infection
cns <- gwqs(cnsInfDiag1 ~ wqs+pct_female_mdc+pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic
            +popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate
            +education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count)), mix_name = mixture
            , data = aggDat100, q = 10, 
            validation = 0.6,b=100, b1_pos = T, b1_constr =T, 
            family = "quasipoisson",seed = 456)
summary(cns)
cns$final_weights

# obtain the robust variance estimator
form <- paste0( names(cns$data)[1], "~ wqs + pct_female_mdc + pct_young_mdc + pct_mid_mdc + pct_black_mdc + pct_white_mdc + hispanic + popdensity + nearest_hospital_km + amb_visit_pct + poverty + medhouseholdincome + smoke_rate + education + as.factor(year) + pct_dual + winter_tmmx + summer_tmmx + offset(log(count))")

modelDat <- cns$data[cns$vindex,]
model1 <- glm(formula = form, family = "quasipoisson",
              data = modelDat)
est <- model1$coefficients[2]
ll <- confint(coeftest(model1, vcov = vcovHC,type = "HC1"))[2,1]
ul <- confint(coeftest(model1, vcov = vcovHC,type = "HC1"))[2,2] 



### pm2.5 components and intestinal infection
intestinal <- gwqs(intestinalDiag1 ~ wqs+pct_female_mdc+pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic
                   +popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate
                   +education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count)), mix_name = mixture
                   , data = aggDat100, q = 10, 
                   validation = 0.6,b=100, b1_pos = T, b1_constr =T, 
                   family = "quasipoisson",seed = 789)
summary(intestinal)
intestinal$final_weights

# obtain the robust variance estimator
form <- paste0( names(intestinal$data)[1], "~ wqs + pct_female_mdc + pct_young_mdc + pct_mid_mdc + pct_black_mdc + pct_white_mdc + hispanic + popdensity + nearest_hospital_km + amb_visit_pct + poverty + medhouseholdincome + smoke_rate + education + as.factor(year) + pct_dual + winter_tmmx + summer_tmmx + offset(log(count))")

modelDat <- intestinal$data[intestinal$vindex,]
model1 <- glm(formula = form, family = "quasipoisson",
              data = modelDat)
est <- model1$coefficients[2]
ll <- confint(coeftest(model1, vcov = vcovHC,type = "HC1"))[2,1]
ul <- confint(coeftest(model1, vcov = vcovHC,type = "HC1"))[2,2] 


### pm2.5 components and urinary tract infection
urin <- gwqs(urinDiag1 ~ wqs+pct_female_mdc+pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic
             +popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate
             +education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count)), mix_name = mixture
             , data = aggDat100, q = 10, 
             validation = 0.6,b=100, b1_pos = T, b1_constr =T, 
             family = "quasipoisson",seed = 333)
summary(urin)
urin$final_weights

# obtain the robust variance estimator
form <- paste0( names(urin$data)[1], "~ wqs + pct_female_mdc + pct_young_mdc + pct_mid_mdc + pct_black_mdc + pct_white_mdc + hispanic + popdensity + nearest_hospital_km + amb_visit_pct + poverty + medhouseholdincome + smoke_rate + education + as.factor(year) + pct_dual + winter_tmmx + summer_tmmx + offset(log(count))")

modelDat <- urin$data[urin$vindex,]
model1 <- glm(formula = form, family = "quasipoisson",
              data = modelDat)
est <- model1$coefficients[2]
ll <- confint(coeftest(model1, vcov = vcovHC,type = "HC1"))[2,1]
ul <- confint(coeftest(model1, vcov = vcovHC,type = "HC1"))[2,2] 


### pm2.5 components and septicemia infection
septic <- gwqs(septicDiag1 ~ wqs+pct_female_mdc+pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic
               +popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate
               +education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count)), mix_name = mixture
               , data = aggDat100, q = 10, 
               validation = 0.6,b=100, b1_pos = T, b1_constr =T, 
               family = "quasipoisson",seed = 888)
summary(septic)
septic$final_weights

# obtain the robust variance estimator
form <- paste0( names(septic$data)[1], "~ wqs + pct_female_mdc + pct_young_mdc + pct_mid_mdc + pct_black_mdc + pct_white_mdc + hispanic + popdensity + nearest_hospital_km + amb_visit_pct + poverty + medhouseholdincome + smoke_rate + education + as.factor(year) + pct_dual + winter_tmmx + summer_tmmx + offset(log(count))")

modelDat <- septic$data[septic$vindex,]
model1 <- glm(formula = form, family = "quasipoisson",
              data = modelDat)
est <- model1$coefficients[2]
ll <- confint(coeftest(model1, vcov = vcovHC,type = "HC1"))[2,1]
ul <- confint(coeftest(model1, vcov = vcovHC,type = "HC1"))[2,2] 



################################################################
########## source-specific PM2.5 and  non-respiratory infection
################################################################

## do the regression within each of the strata defined by time priod and region
t1 <- readRDS("t1_demo.rds")
t2 <- readRDS("t2_demo.rds")
t3 <- readRDS("t3_demo.rds")

disease <- c("nonrspInf", "cnsInf", "intestinal", "urin", "septic")

for (d in 1:5) {
  
  
  coefMat <- matrix(ncol = 8, nrow = 9)
  seMat <- matrix(ncol = 8, nrow = 9)
  iter <- 1
  
  ## time period 1
  
  for (clu in 1:3) {
    
    sbset <- dplyr::filter(t1, cluster == clu)
    
    fo <- paste0(disease[d], "Diag1 ~ oil+dust+coal+traffic+biomass+pct_female_mdc+pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic+popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate+education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count))")
    
    tt<- glm(fo,family = "quasipoisson", data = sbset)
    
    sources <- c("oil", "dust", "coal", "traffic", "biomass")
    
    for (j in 1:5) {
      coefMat[iter, j] <- tt$coefficients[j+1]
      if (length(grep(sources[j], colnames(sandwich(tt)))) != 0) {
        
        pos <- grep(sources[j], colnames(sandwich(tt)))
        seMat[iter, j] <- sqrt(sandwich(tt)[pos,pos])
        
      }
    }
    
    
    
    iter <- iter+1
    
    
  }
  
  
  ## time period 2
  
  for (clu in 1:3) {
    
    sbset <- dplyr::filter(t2, cluster == clu)
    
    fo <- paste0(disease[d], "Diag1 ~ oil+dust+coal+traffic+biomass+biomassTraffic+nitrate+pct_female_mdc+pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic+popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate+education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count))")
    tt<- glm(formula = fo ,family = "quasipoisson", data = sbset)
    
    sources <- c("oil", "dust", "coal", "traffic", "biomass$", "biomassTraffic", "nitrate")
    
    for (j in 1:7) {
      coefMat[iter, j] <- tt$coefficients[j+1]
      if (length(grep(sources[j], colnames(sandwich(tt)))) != 0) {
        
        pos <- grep(sources[j], colnames(sandwich(tt)))
        seMat[iter, j] <- sqrt(sandwich(tt)[pos,pos])
        
      }
    }
    
    
    
    iter <- iter+1
    
    
  }
  
  
  ## time period 3
  for (clu in 1:3) {
    
    sbset <- dplyr::filter(t3, cluster == clu)
    
    fo <- paste0(disease[d], "Diag1 ~ oil+dust+coal+traffic+biomass+tpTraffic+nitrate+ntpTraffic+pct_female_mdc+pct_young_mdc+pct_mid_mdc+pct_black_mdc+pct_white_mdc+hispanic+popdensity +nearest_hospital_km+amb_visit_pct+poverty+medhouseholdincome+smoke_rate+education+as.factor(year)+pct_dual+winter_tmmx+summer_tmmx+offset(log(count))")
    
    tt<- glm(formula = fo,family = "quasipoisson", data = sbset)
    
    sources <- c("oil", "dust", "coal", "traffic", "biomass", "^tpTraffic", "nitrate", "ntpTraffic")
    
    for (j in 1:8) {
      coefMat[iter, j] <- tt$coefficients[j+1]
      if (length(grep(sources[j], colnames(sandwich(tt)))) != 0) {
        
        pos <- grep(sources[j], colnames(sandwich(tt)))
        seMat[iter, j] <- sqrt(sandwich(tt)[pos,pos])
        
      }
      
    }
    
    
    
    iter <- iter+1
    
    
  }
  
  
  
  saveRDS(coefMat, paste0("mutuallyAdjusted_", disease[d], "Diag1_coef_pm25asTotal.rds"))
  saveRDS(seMat, paste0("mutuallyAdjusted_", disease[d], "Diag1_se_sandwhich.rds"))
  
  
}

############ rescale by SD
# oil
t3Oil <- t3  %>% select(oil)
t2Oil <- select(t2 , oil)
t1Oil <- select(t1 , oil)

oil <- rbind(t1Oil,t2Oil,t3Oil)
oilSD <- sd(oil$oil)


# dust
t1dust <- t1  %>%  select(dust)
t2dust <- t2  %>%  select(dust)
t3dust <- t3 $dust

dust <-  rbind(t1dust,t2dust,t3dust)
dustSD <- sd(dust$dust)

#coal
coal <- c(t1$coal, t2$coal, t3$coal)
coalSD <- sd(coal)

#traffic
t1traffic <- t1$traffic
t2traffic <- t2 %>%  select(traffic)
t3traffic <- t3 %>%  select(traffic)

traffic <- rbind(t1traffic, t2traffic, t2traffic)
trafficSD <- sd(traffic$traffic)

# biomass
t1bio <- t1 %>%  select(biomass)
t2bio <- t2 %>%  select(biomass)
t3bio <- t3 %>%  select(biomass)

bio <- rbind(t1bio, t2bio, t3bio)
biomassSD <- sd(bio$biomass)

# nitrate

t2nitrate <- t2 %>%  select(nitrate)
t3nitrate <- t3 %>%  select(nitrate)

nitrate <- rbind(t2nitrate, t3nitrate)
nitrateSD <- sd(nitrate$nitrate)



fac <- c("oil", "dust", "coal", "traffic", "biomass", "mix", "nitrate")
facSD <- c(oilSD, dustSD, coalSD, trafficSD, biomassSD, 1, nitrateSD)
disease <- c("nonrspInf", "cnsInf", "intestinal", "urin", "septic")
cluster <- c("t1c1", "t1c2", "t1c3","t2c1", "t2c2", "t2c3","t3c1", "t3c2", "t3c3")


### mutually adjusted

rmat <- matrix(ncol = 5, nrow = 7)

for (d in 1:5) {
  
  coefDat <- readRDS(paste0("mutuallyAdjusted_", disease[d],"Diag1_coef_pm25asTotal.rds"))
  seDat <- readRDS(paste0("mutuallyAdjusted_", disease[d],"Diag1_se_sandwhich.rds"))
  
  
  
  for (f in 1:7) {
    
    if (f != 6) {
      
      coef <- coefDat[,f]*facSD[f]
      se <- seDat[,f]*facSD[f]
      dat <- data.frame(cluster, coef, se)
      model1 <- rma(yi = coef, sei = se, data = dat, method = "REML")
      
      est <- round(exp(model1$beta), digits = 3)
      ul <- round(exp(model1$beta+1.96*model1$se), digits = 3)
      ll <- round(exp(model1$beta-1.96*model1$se), digits = 3)
      
      rmat[f,d] <- paste0(est, " (", ll, "-", ul, ")")
      
      #title <- paste(disease[d], fac[f], "mutually Adjusted", sep = ", ")
      
      #png(paste0("/n/dominici_nsaph_l3/Lab/projects/pm_component-infection-wqs/results/nmfReg/figure/figure_multiple_", disease[d], " ", fac[f],"_pm25asTotal.png"  ))
      #forest(model1, slab = dat$cluster, xlab = title)
      #dev.off()
      
    }
    
  }
  
}

colnames(rmat) <- c("nonrspInf", "cnsInf", "intestinal", "urin", "septic")
rownames(rmat) <- c("oil", "dust", "coal", "traffic", "biomass", "mix", "nitrate")

write.csv(rmat, "effect_sandwich.csv")



