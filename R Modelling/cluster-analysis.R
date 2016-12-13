# cluster analysis of the station dataset
# following guidance from https://rstudio-pubs-static.s3.amazonaws.com/33876_1d7794d9a86647ca90c4f182df93f0e8.html

library(cluster)

#area.stats <- df[,c(4, which(names(df)=="Avg..Annual.Crime.Rate"):ncol(df))]
df <- df[!(df$Station.Name %in% north.side.stations),]
area.stats <- df[,c(4, which(names(df)=="Transit.Connections"):ncol(df))]
row.names(area.stats) <- df$Station.Name

# area.stats$MedHValue2000 <- NULL
# area.stats$Jobs2002 <- NULL
# area.stats$PopPlusJobsPerSqMile <- NULL

# head(area.stats)
# area.stats.z <- as.data.frame(lapply(area.stats, scale))
# head(area.stats.z)
# 
# station.clusters <- kmeans(area.stats.z, 2)
# station.clusters$size
# par(mfrow=c(1,1))
# pie(colSums(area.stats[station.clusters$cluster==2,]),cex=0.5)
# station.clusters

D = daisy(area.stats, metric='gower')
H.fit <- hclust(D, method="ward.D2")
pdf(file="output/clusters.pdf")
par(mfrow=c(1,1))
for(i in 2:5){
  #plot(H.fit)
  groups <- cutree(H.fit, k=i)
  #rect.hclust(H.fit, k=i, border="red")
  clusplot(area.stats, groups, color=TRUE, shade=TRUE, labels=2, lines=0, main=paste("Station Groups:", i, "Clusters"))
}
dev.off()
  
groups <- cutree(H.fit, k=2)
head(groups)
s1 <- df[df$Station.Name %in% names(groups[groups==1]),]
s2 <- df[df$Station.Name %in% names(groups[groups==2]),]
m1 <- glm.nb(f,  data = s1, link = log)
m2 <- glm.nb(f,  data = s2, link = log)

summary(m1)
summary(m2)

est1 <- cbind(Estimate = coef(m1), confint(m1))
est2 <- cbind(Estimate = coef(m2), confint(m2))
est <- cbind(est1, est2)
round(exp(est),3)
