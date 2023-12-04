###################################################################
#############       Non-negative matrix factorization   ###########
#############                   sample code             ###########
#############                   Yijing Feng             ###########
###################################################################

#install.packages('NMF') 
library(NMF)
library(stats)
library(dplyr)
library(ggplot2)


####################################################################################################
########## within each time period, we cluster the ZIP codes based on patterns of the component data
#####################################################################################################

########
########  the number of clusters depends on actual data, here we use 3 clusters just for illustration
########

component <- readRDS("component_demo.rds")

### time period 1

subDat1 <- filter(component, tPeriod == 1)
componentAgg <- subDat1 %>% group_by(ZIP) %>% summarise(br = mean(br), ca = mean(ca),
                                                        cu = mean(cu), ec = mean(ec),
                                                        fe = mean(fe), k = mean(k),
                                                        nh4 = mean(nh4), ni = mean(ni),
                                                        no3 = mean(no3), oc = mean(oc),
                                                        pb = mean(pb), si = mean(si),
                                                        so4 = mean(so4), v = mean(v),
                                                        z = mean(z))

nozip <- componentAgg[,c(2:16)]
wardsmethod<-hclust(dist(scale(nozip)),method="ward.D")
plot(wardsmethod)
profiling<-cutree(wardsmethod,3)
componentAgg$cluster <- profiling
componentAgg <- select(componentAgg, ZIP, cluster)
subDat1 <- left_join(subDat1, componentAgg)


### time period 2

subDat2 <- filter(component, tPeriod == 2)
componentAgg <- subDat2 %>% group_by(ZIP) %>% summarise(br = mean(br), ca = mean(ca),
                                                        cu = mean(cu), ec = mean(ec),
                                                        fe = mean(fe), k = mean(k),
                                                        nh4 = mean(nh4), ni = mean(ni),
                                                        no3 = mean(no3), oc = mean(oc),
                                                        pb = mean(pb), si = mean(si),
                                                        so4 = mean(so4), v = mean(v),
                                                        z = mean(z))

nozip <- componentAgg[,c(2:16)]
wardsmethod<-hclust(dist(scale(nozip)),method="ward.D")
plot(wardsmethod)
profiling<-cutree(wardsmethod,3)
componentAgg$cluster <- profiling
componentAgg <- select(componentAgg, ZIP, cluster)
subDat2 <- left_join(subDat2, componentAgg)


### time period 3
subDat3 <- filter(component, tPeriod == 3)
componentAgg <- subDat3 %>% group_by(ZIP) %>% summarise(br = mean(br), ca = mean(ca),
                                                        cu = mean(cu), ec = mean(ec),
                                                        fe = mean(fe), k = mean(k),
                                                        nh4 = mean(nh4), ni = mean(ni),
                                                        no3 = mean(no3), oc = mean(oc),
                                                        pb = mean(pb), si = mean(si),
                                                        so4 = mean(so4), v = mean(v),
                                                        z = mean(z))

nozip <- componentAgg[,c(2:16)]
wardsmethod<-hclust(dist(scale(nozip)),method="ward.D")
plot(wardsmethod)
profiling<-cutree(wardsmethod,3)
componentAgg$cluster <- profiling
componentAgg <- select(componentAgg, ZIP, cluster)
subDat3 <- left_join(subDat3, componentAgg)

###########################################################################
### NMF (for the first time-period only, the whole first time period will 
###        be treated as one cluster for illustration)
###       in the actual analysis, we did it within finer clusters
###########################################################################

compUnit <- c(rep(0.001, 3), 1, 0.001, 0.001, 1, 0.001, 1, 1, 0.001, 0.001, 1,0.001, 0.001)
comp <- as.matrix(subDat1[,c(2:16)])
total <- comp %*% compUnit
subDat1$totalpm <- total

cDat <- subDat1
stdMat <- matrix(ncol = 15, nrow = dim(cDat)[1])

for (q in 2:16) {
  stdMat[,q-1] <- scale(cDat[,q])
}

stdMat<-nneg(stdMat,method='min') 

stdMat <- as.data.frame(stdMat)
colnames(stdMat) <- colnames(component)[c(2:16)]

### try to separate the data into 4,5 and 6 factors and evaluate based on heatmap
res4<-nmf(stdMat,4,seed='random',nrun=100)
coefmap(res4)

res5<-nmf(stdMat,5,seed='random',nrun=100)
coefmap(res5)

res6<-nmf(stdMat,6,seed='random',nrun=100)
coefmap(res6)

###### will go with the 5 factors in this example
### factor 1 is identified as traffic, factor 2 being biomass, factor 3 being coal 
### factor 4 being dust and factor 5 being oil
######

cDat$oil <- basis(res5)[,5]
cDat$dust <- basis(res5)[,4]
cDat$coal <- basis(res5)[,3]
cDat$traffic <- basis(res5)[,1]
cDat$biomass <- basis(res5)[,2]

#transform the unit of the factors to microgram/m3
model1 <- lm(total~oil+dust+coal+traffic+biomass, data = cDat)
summary(model1)

cDat$oil <- cDat$oil * coef(model1)[2]
cDat$dust <- cDat$dust * coef(model1)[3]
cDat$coal <- cDat$coal * coef(model1)[4]
cDat$traffic <- cDat$traffic * coef(model1)[5]
cDat$biomass <- cDat$biomass * coef(model1)[5]

### then we obtain the level of factors by ZIP codes