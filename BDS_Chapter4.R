install.packages("BasketballAnalyzeR")
library(BasketballAnalyzeR)
library(gridExtra)
RNGkind(sample.kind = "Rounding")


getwd()
#118-119
FF <- fourfactors(Tbox, Obox)

OD.Rtg <- FF$ORtg/FF$DRtg
F1.r <- FF$F1.Off/FF$F1.Def
F2.r <- FF$F2.Off/FF$F2.Def
F3.Off <- FF$F3.Off
F3.Def <- FF$F3.Def
P3M <- Tbox$P3M
STL.r <- Tbox$STL/Obox$STL
data <- data.frame(OD.Rtg, F1.r, F2.r, F3.Off, F3.Def, P3M, STL.r)
set.seed(29)
kclu1 <- kclustering(data)
plot(kclu1)

#120
set.seed(29)
kclu2 <- kclustering(data, labels = Tbox$Team, k = 5)
plot(kclu2)
attach(kclu2)
ClusterList
detach(kclu2)

#124
kclu2.PO <- table(kclu2$Subjects$Cluster, Tadd$Playoff)
kclu2.W <- tapply(Tbox$W, kclu2$Subjects$Cluster, mean)

Xbar <- data.frame(cluster=c(1:5), N=kclu2.PO[,1],
                   Y=kclu2.PO[,2], W=kclu2.W)
barline(data=Xbar, id = "cluster", bars = c("N", "Y"),
        labels.bars = c("Playoff: No", "Playoff: Yes"),
        line = "W", label.line = "Average Wins", decreasing = FALSE)

cluster <- as.factor(kclu2$Subjects$Cluster)
Xbubble <- data.frame(Team = Tbox$Team, PTS = Tbox$PTS, 
                      PTS.Opp = Obox$PTS, cluster, W = Tbox$W)
labs <- c("PTS", "PTS.Opp", "cluster", "Wins")
bubbleplot(Xbubble, id = "Team", x = "PTS", y = "PTS.Opp",
           col = "cluster", size = "W", labels = labs)

#126
shots <- subset(pbp, !is.na(pbp$shot_distance) & pbp$team == "GSW")
shots <- dplyr::mutate_if(shots, is.factor, droplevels)

#127
attach(shots)
data <- data.frame(PTS = points, DIST = shot_distance,
                   TIMEQ = periodTime, PL = playlength)
detach(shots)
set.seed(1)
kclu1 <- kclustering(data, algorithm = "MacQueen",
                     nclumax =15, iter.max = 500)
plot(kclu1)

#128
set.seed(1)
kclu2 <- kclustering(data, algorithm = "MacQueen", iter.max = 500, k = 6)
plot(kclu2)

cluster <- as.factor(kclu2$Subjects$Cluster)
shots <- data.frame(shots, cluster)
shots$xx <- shots$original_x/10
shots$yy <- shots$original_y/10-41.75

no.clu <- 6
p1 <- p2 <- vector(no.clu, mode = "list")
for ( k in 1:no.clu) {
  shots.k <- subset(shots, cluster==k)
  p1[[k]] <- shotchart(data = shots.k, x = "xx", y = "yy",
                       z = "result", type = NULL, 
                       scatter= TRUE, drop.levels = FALSE)
  p2[[k]] <- shotchart(data = shots.k, x = "xx", y = "yy",
                       z = "periodTime", col.limits = c(0, 720),
                       result = "result", num.sect = 5,
                       type = "sectors", scatter = FALSE)
}
grid.arrange(grobs = p1, nrow = 3)
grid.arrange(grobs = p2, nrow = 3)

#133
shots.pl <- table(shots$player, shots$cluster)
Xineq <- as.data.frame.matrix(shots.pl)

no.clu <- 6
p <- vector(no.clu, mode = "list")
for(k in 1:no.clu) {
  ineqC <- inequality(Xineq[,k], npl = nrow(Xineq))
  title <- paste("Cluster", k)
  p[[k]] <- plot(ineqC, title = title)
}
grid.arrange(grobs = p, nrow = 3)

shots.perc <- shots.pl/rowSums(shots.pl)
Xbar <- data.frame(player = rownames(shots.pl),
                   rbind(shots.perc),
                   FGA = rowSums(shots.pl))
labclusters <- c("Cluster 1", "Cluster 2", "Cluster 3",
                 "Cluster 4", "Cluster 5", "Cluster 6")

#135
barline(data = Xbar, id = "player", line = "FGA",
        bars = c("X1", "X2", "X3", "X4", "X5", "X6"),
        order.by = "FGA", label.line = "Field Goals Attempted",
        labels.bars = labclusters)

#138

data <- data.frame(Pbox$PTS, Pbox$P3M, REB = Pbox$OREB + Pbox$DREB, 
                   Pbox$AST, Pbox$TOV, Pbox$STL, Pbox$BLK, Pbox$PF)
data <- subset(data, Pbox$MIN >= 1500)
ID <- Pbox$Player[Pbox$MIN >= 1500]

#139
hclu1 <- hclustering(data)
plot(hclu1)

#140
hclu2 <- hclustering(data, labels = ID, k = 9)
plot(hclu2, profiles = TRUE)

#141
plot(hclu2, rect = TRUE, colored.branches = TRUE,
     cex.labels = .5)

Pbox.subset <- subset(Pbox, MIN >= 1500)
MIN <- Pbox.subset$MIN
X <- data.frame(hclu2$Subjects, scale(data), MIN)

#143
dvar <- c("PTS", "P3M", "REB", "AST",
          "TOV", "STL", "BLK", "PF")
svar <- "MIN"
yRange <- range(X[,dvar])
sizeRange <- c(1500, 3300)
no.clu <- 9
p <- vector(no.clu, mode = "list")
for (k in no.clu) {
  XC <- subset(X, Cluster == k)
  vrb <- variability(XC[, 3:11], data.var = dvar,
                     size.var = svar, weight = FALSE, 
                     VC = FALSE)
  title <- paste("Cluster", k)
  p[[k]] <- plot(vrb, size.lim = sizeRange, ylim = 8,
                 title = title, leg.pos = c(0,1),
                 leg.just = c(-.5, 0),
                 leg.box = "vertical",
                 leg.brk = seq(1500, 3000, 500),
                 leg.title.pos = "left", leg.nrow = 1,
                 max.circle = 7)
}
grid.arrange(grobs = p, ncol = 3)
