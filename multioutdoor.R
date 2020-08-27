library(caret) 
library(apcluster)
library(ggplot2)
library(cluster)
library(factoextra)
library(fossil)
library(fpc)
library(dplyr)

rm(list=ls())
setwd("C:/Users/MURTHY/Desktop/project")
multi_data <- read.csv("multioutdoor.csv", head = F)
str(multi_data)

sapply(multi_data, mode)

multi_data$V5 <- factor(multi_data[,5])

normalized_multi_data<-select(normalized_multi_data,-c(1,2))
normalized_multi_data$V3


normalized_multi_data<- multi_data
normalized_multi_data$V3 = as.numeric(format(round(normalized_multi_data$V3, 0)))
normalized_multi_data$V4 = as.numeric(format(round(normalized_multi_data$V4, 0)))
preprocessParams<-preProcess(multi_data[,3:4], method="range")
normalized_multi_data[,1:2] <- predict(preprocessParams, multi_data[,3:4])
head(normalized_multi_data[,1:2] )
normalized_multi_data[,1:2][is.na(normalized_multi_data[,1:2])]<-0


table(normalized_multi_data$V5)#finds anomaly instances

tiff("C:/Users/MURTHY/Desktop/projectmultioutdoor.tiff", units="in", width=8, height=6, res=600)
Label = multi_data$V5
satellite_plot <- ggplot(normalized_multi_data , aes(x=V3, y=V4  , color=Label) ) + geom_point() +  theme(text = element_text(size=30)) + guides(color = guide_legend(override.aes = list(size=5)))
satellite_plot  + scale_color_manual(labels = c("Normal", "Anomaly"), breaks = c("0", "1"),
                                     values=c("#56B4E9", "red")) + ggtitle("MultiHop Outdoor") 
dev.off()



set.seed(300)
clusters <- kmeans(normalized_multi_data[,1:2], 2,25)
normalized_multi_data$V6 <- as.factor(clusters$cluster)
str(clusters)

table(normalized_multi_data$V6)

print(normalized_multi_data)

tiff("C:/Users/MURTHY/Desktop/Kmeans_multi_outdoor.tiff", units="in", width=8, height=6, res=600)
Label = normalized_multi_data$V6 
satellite_plot <- ggplot( normalized_multi_data, aes(x=V3, y=V4  , color=Label) ) + geom_point() +  theme(text = element_text(size=30)) + guides(color = guide_legend(override.aes = list(size=5)))
satellite_plot  + scale_color_manual(labels = c("Normal", "Anomaly"), breaks = c("1", "2"),
                                     values=c("#56B4E9", "red")) + ggtitle("MultiHop outdoor - kmeans") 
dev.off()


write.csv(normalized_multi_data,'normalized_multi_outdoor_datakmeans.csv')

#AGNES


d <- dist(normalized_multi_data[,1:2], method = "euclidean")

hc1 <- hclust(d)

sub_grp <- cutree(hc1, k = 2)
table(sub_grp)
# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

plot(hc1, cex = 0.6)
rect.hclust(hc1, k = 2, border = 2:5)



Label_HAP= labels(cutree(hc1,2)  , type ="enum" ) ## "1" = "anomaly" class And "2" = "normal"
table( Pred = Label_HAP, True = normalized_multi_data$V5)
table(Label_HAP)
normalized_multi_data$V6=cutree(hc1,2)
head(normalized_multi_data$V6)
write.csv(normalized_multi_data,'normalized_multi_data_agnes.csv')

table(normalized_multi_data$V6)


clusteredData = read.csv("normalized_multi_data_agnes.csv", header=T)
label<-factor (clusteredData$V6)


tiff("C:/Users/MURTHY/Desktop/multiagnes.tiff", units="in", width=8, height=6, res=600)

satellite_plot <- ggplot( normalized_multi_data, aes(x=V3, y=V4  , color=label) ) + geom_point() +  theme(text = element_text(size=30)) + guides(color = guide_legend(override.aes = list(size=5)))
satellite_plot  + scale_color_manual(labels = c("Normal", "Anomaly"), breaks = c("1", "2"),
                                     values=c("#56B4E9", "red")) + ggtitle("MultiHop outdoor - Agnes") 
dev.off()



#DBSCAN
dbscan::kNNdistplot(normalized_multi_data[,1:2], k = 78)
abline(h = 0.4, lty = 2)

x <- as.matrix(normalized_multi_data[,1:2])
cl <- dbscan(x, eps = .4, MinPts =50 )
cl$cluster
normalized_multi_data$V6 <- as.factor(cl$cluster)
plot(cl, normalized_multi_data[,1:2], main = "DBSCAN", frame = FALSE)


table(normalized_multi_data$V6)


tiff("C:/Users/MURTHY/Desktop/multioutdoorDB.tiff", units="in", width=8, height=6, res=600)
Label <- normalized_multi_data$V6
satellite_plot <- ggplot(normalized_multi_data , aes(x=V3, y=V4  , color=Label) ) + geom_point() +  theme(text = element_text(size=30)) + guides(color = guide_legend(override.aes = list(size=5)))
satellite_plot  + scale_color_manual(labels = c("Anomaly", "Normal"), breaks = c("0", "1"),
                                     values=c("red", "#56B4E9")) + ggtitle("DBSCAN") 
dev.off()

write.csv(normalized_multi_data,'normalized_multi_data_dbscan.csv')

#K- Medoids
kmed <- pam(normalized_multi_data[,1:2], 2)
normalized_multi_data$V6 <- as.factor(kmed$cluster)
kmed$cluster
head(normalized_multi_data)

table(normalized_multi_data$V6)

tiff("C:/Users/MURTHY/Desktop/multioutdoorKmed.tiff", units="in", width=8, height=6, res=600)
Label = factor(normalized_multi_data$V6)
satellite_plot <- ggplot(normalized_multi_data , aes(x=V3, y=V4  , color=Label) ) + geom_point() +  theme(text = element_text(size=30)) + guides(color = guide_legend(override.aes = list(size=5)))
satellite_plot  + scale_color_manual(labels = c("Anomaly", "Normal"), breaks = c("1", "2"),
                                     values=c("red", "#56B4E9")) + ggtitle("PAM") 
dev.off()

write.csv(normalized_multi_data,'normalized_multi_data_kmed.csv')


#HAP
sim <- negDistMat(normalized_multi_data)
apres <- apcluster(sim,  details=TRUE)
show(apres)
aggres <- aggExCluster(sim, apres)
show(aggres)



Label_HAP= labels(cutree(aggres,2)  , type ="enum" ) ## "1" = "anomaly" class And "2" = "normal"
table( Pred = Label_HAP, True = multi_data$V5)
normalized_multi_data$V6=labels(cutree(aggres,2)  , type ="enum" )
write.csv(normalized_multi_data,'normalized_multi_data_hap.csv')


table(normalized_multi_data$V6)
tiff("C:/Users/MURTHY/Desktop/satelliteHAP.tiff", units="in", width=8, height=6, res=600)
Label_HAP <- factor(Label_HAP)
HAP_plot <- ggplot(multi_data , aes(x=V3, y=V4  , color=Label_HAP)) + geom_point() +  theme(text = element_text(size=30)) + guides(color = guide_legend(override.aes = list(size=5)))
HAP_plot  + scale_color_manual(labels = c( "Anomaly",   "Normal" ), breaks = c("1", "2"),
                               values=c(  "red", "#56B4E9" )) + ggtitle("multi outdoor - HAP")


dev.off()




