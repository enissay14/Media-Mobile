#PCA
df.train <- arc_aleatoir_from_df(10,dff)
data <- get_data(df.train)
fit <- princomp(data, cor=TRUE)
summary(fit)  # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines", main="")
pc <- fit$scores # the principal components
#biplot(fit)
plot(pc[,1], pc[,2], xlab="Comp.1", ylab="Comp.2")





#ensemble train
dff.train <- arc_aleatoir(10,dff)
data <- get_data(dff.train)

#hclust
dff.train.dist = dist(data)
hc=hclust(dff.train.dist,"ave")
groups = cutree(hc,4)
table(groups)

#analyse hclust
table(groups, dff.train$wday)
dff.train<-data.frame(dff.train,groups)

#pam
library(cluster)
dff.train.pam = pam(data,4)
plot(dff.train.pam)

#comparaison hclust et pam
table(groups,dff.train.pam$clustering)

dff.train<-data.frame(dff.train,dff.train.pam$clustering)

#observation dans dff.train proche des clusters
clusplot( dff.train.pam )

grp1.hclust <- subset(dff.train, dff.train$groups == 1)
grp2.hclust <- subset(dff.train, dff.train$groups == 2)
grp3.hclust <- subset(dff.train, dff.train$groups == 3)
grp4.hclust <- subset(dff.train, dff.train$groups == 4)
grp1.pam <- subset(dff.train, dff.train$groups == 1)
grp2.pam <- subset(dff.train, dff.train$groups == 2)
grp3.pam <- subset(dff.train, dff.train$groups == 3)
grp4.pam <- subset(dff.train, dff.train$groups == 4)

moyenne_clust(grp3)

moyenne_clust<-function(grp){
  length <- nrow(grp)
  data <- grp  
  data <- data[-(1:2)]
  data <- data[-(89:101)]
  moy <- apply(data,2,sum)
  moy=moy/length
  barplot(moy)
}

arc_aleatoir_from_df <- function(nbre_arc,dff){
  df<- data.frame()
  for (i in 1:nbre_arc){
    df <-  rbind(df, subset(dff, dff$link == sample(1:10000,1) ))
  }
  df
}

get_data<-function(df){
  data <- df
  data <- data[-(1:2)]
  data <- data[-(89:101)]
  data
}




