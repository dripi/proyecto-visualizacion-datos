library(data.table)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

ds <- fread("pax_data_1789_agreements_14-11-19.csv")
ds <- as.data.frame(ds)


table(ds$GRe) # > 0
table(ds$Tral) #> 0
table(ds$CprReli) # > 0
table(ds$PpsAut) # > 0
table(ds$JusTra) #> 0
table(ds$LaCHIt) # > 0
table(ds$LaCHPro) # > 0

ds$Religion <- ifelse(ds$GRe > 0 | 
                        ds$Tral > 0 | 
                        ds$CprReli > 0 | 
                        ds$PpsAut > 0 | 
                        ds$JusTra > 0 | 
                        ds$LaCHIt > 0 | 
                        ds$LaCHPro > 0, 1, 0)

table(ds$Religion)

col <- c("Con"
         ,"Contp"
         ,"Reg"
         ,"Dat"
         ,"Agtp" 
         ,"Stage" 
         ,"Loc1ISO"
         #1STATE DEFINITION
         ,"StDef"
         ,"StGen"
         ,"StCon"
         #1SOCIOECONOMIC
         ,"Dev"
         ,"NatRes"
         ,"Bus"
         #1LAND, PROPERTIES
         ,"LaRef"
         #1SECURITY
         ,"SsrPol"
         #3GENDER
         ,"GCh" #children
         ,"GRa" #racial ethnis groups
         ,"GeWom" #mujeeres
         ,"GeMe" #hombre
         #3HUMAN RIGHTS AND EQUALITY
         ,"HrGen" #human rigths
         ,"EqGen" #equality
         ,"HrDem" #democracy
         ,"HrFra" #human rigths framwork
         ,"HrCp" #civil and political rigth
         ,"HrSec" #socio economic rights
         ,"HrNi" #national human rights institution
         ,"Med" #Media and communication
         ,"HrCit" #citenship
         ,"Prot" #protection measures
         #2GOVERNANCE
         ,"Pol" #political institution
         ,"Cons" #constitutional reform
         ,"Ele" #elections
         ,"Pubad" #civil service
         #2POWERSHARING
         ,"Polps" #political ps
         ,"Terps" #territorial ps
         ,"Eps" #economic ps
         #2JUSTICE
         ,"JusCr" # criminal justice and emergency law
         ,"JusJu" #judiciary and courts
         ,"TjNR" #TRANSITIONAL
     
         ,"Religion")

ds <- ds[,col]

for(i in c(colnames(ds)[grep("^St", colnames(ds))], colnames(ds)[grep("^G", colnames(ds))], colnames(ds)[grep("^Hr", colnames(ds))], colnames(ds)[grep("^E", colnames(ds))])){
  ds[,i] <- as.factor(ds[,i])
}
ds$Tral <- as.factor(ds$Tral)
ds$Terps <- as.factor(ds$Terps)
ds$Polps <- as.factor(ds$Polps)
ds$Mps <- as.factor(ds$Mps)
ds$Med <- as.factor(ds$Med)
ds$Agtp <- as.factor(ds$Agtp)
ds$Contp <- as.factor(ds$Contp)

summary(ds)
str(ds)

colSums(is.na(ds)) # solo hay nulos en CowWar, los imputamos a 0
ds$CowWar <- ifelse(is.na(ds$CowWar), 0, ds$CowWar)


cor_res <- data.frame(matrix(nrow = 243, ncol = 243))
a <- 1
for(i in colnames(ds)[25:267]){
  rownames(cor_res)[a] <- i
  b <- 1
  for(j in colnames(ds)[25:267]){
    #print(cor(ds_empresa[,i], ds_empresa[,j]))
    colnames(cor_res)[b] <- j
    cor_res[a, b] <- cor(ds[,i], ds[,j])
    b <- b + 1
  }
  a <- a + 1
}

cor_res <- data.frame(matrix(nrow = 1, ncol = 243))


b <- 1
  for(j in colnames(ds)[25:267]){
    #print(cor(ds_empresa[,i], ds_empresa[,j]))
    colnames(cor_res)[b] <- j
    cor_res[1, b] <- cor(ds$Religion, ds[,j])
    b <- b + 1
  }


cor_res2 <- cor_res %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)

ggplot(cor_res2, aes(x = rowname, y = colname, fill = value)) +
  geom_tile() +
  scale_fill_distiller(palette = "Reds", direction = 1)


colnames(cor_res)[t(cor_res) > 0.3]

fwrite(ds, "data.csv")
