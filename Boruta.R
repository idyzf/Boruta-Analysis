library(openxlsx)

rm(list = ls())
pt_clinical <- read.xlsx("IHQ_stain_clinical.xlsx")
names(pt_clinical)
pt_clinical$ID <- pt_clinical$ID_REDCAP
rownames(pt_clinical) <- pt_clinical$ID
pt_clinical$ID_REDCAP <- NULL
summary(as.factor(pt_clinical$ID))

pt_nanostring <- read.xlsx("ruvcorrected_boruta.xlsx")
head(pt_nanostring)
rownames(pt_nanostring)<- pt_nanostring$ID
#pt_nanostring$ID <- pt_nanostring$Name
#pt_nanostring$Name <- NULL
str(pt_nanostring)
library(dplyr)
#merging data sets
data1<- left_join(pt_clinical[, c(16, 51)], pt_nanostring)
names(data1)

write.csv(data1, "boruta_data_OS.csv")
rownames(data1) <- data1$ID
data1$ID <- NULL
names(data1)
data2 <- scale(data1[,2:ncol(data1)]) %>% as.data.frame
data2$OS <- data1$Sobrevida.Global.sem.arredondamento %>% as.factor
summary(data2$OS)



library(Boruta)
myboruta <- Boruta(OS ~., data2)
dev.off()
plot(myboruta, las =2)
getNonRejectedFormula(myboruta)

getConfirmedFormula(myboruta)
names(data2)
dat2pca <- prcomp(data2[,1:192])
summary(dat2pca)

