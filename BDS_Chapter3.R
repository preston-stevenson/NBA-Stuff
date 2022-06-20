install.packages("BasketballAnalyzeR")
library(BasketballAnalyzeR)
library(gridExtra)
RNGkind(sample.kind = "Rounding")
install.packages("vcd")
library(vcd)
install.packages("lsr")
install.packages("tidygraph")
install.packages("CINNA")
#23
pbp <- PbPmanipulation(PbP.BDB)


#60
PbP.GSW <- subset(pbp, team == "GSW")
ev <- c("ejection", "end of period", "jump ball", "start of period",
        "unknown", "violation", "timeout", "sub", "foul", "turnover")
event.unsel <- which(PbP.GSW$event_type %in% ev)
PbP.GSW.ev <- PbP.GSW[-event.unsel,]

#61
attach(PbP.GSW.ev)
T <- table(oppTeam, event_type, exclude = ev)
detach(PbP.GSW.ev)

assocstats(T)

#64
library(dplyr)
library(lsr)
library(tibble)
FF <- fourfactors(Tbox, Obox)
attach(Tbox)
attach(FF)
X <- data.frame(PTS, P2M, P3M, FTM, REB = OREB + DREB, 
                AST, STL, BLK, ORtg, DRtg)
detach(Tbox)
detach(FF)
Playoff <- Tadd$Playoff
#This doesnt work
eta <- sapply(X, function(Y){
  cm <- round(tapply,(Y, Playoff, mean), 1)
  eta2 <- etaSquared(aov(Y~Playoff))[1]*100
  c(cm, round(eta2, 2))
}) %>%
  t() %>%
  as.data.frame() %>%
  rename(No=N, Yes=Y, eta2=V3) %>%
  rownames_to_column('rownm') %>%
  arrange(-eta2) %>%
  column_to_rownames('rownm')

#65
data <- subset(Pbox, MIN>= 500)
attach(data)
X <- data.frame(AST, TOV)/MIN
detach(data)
cor(X$AST, X$TOV)

#67
cor(rank(X$AST), rank(X$TOV))
cor(X$AST, X$TOV, method = 'spearman')
cor(X)

#68-69
d1 <- merge(Pbox, Tadd, by = "Team")
d1 <- subset(d1, MIN>= 500)
d1


X <- data.frame(PTS = d1$PTS/d1$MIN, P3M = d1$P3M/d1$MIN, P2M = d1$P2M/d1$MIN, 
                REB = (d1$OREB + d1$DREB)/d1$MIN, AST = d1$AST/d1$MIN, 
                TOV = d1$TOV/d1$MIN, STL = d1$STL/d1$MIN, BLK = d1$BLK/d1$MIN, d1$Playoff)
corrmatrix <- corranalysis(X[,1:8], threshold = .5)
plot(corrmatrix)

#71
scatterplot(X, data.var = c("PTS", "P3M", "P2M", "REB", "AST", "TOV", "STL"),
            z.var = "BLK", diag = list(continuous="blankDiag"))

#74
attach(Pbox)
data = data.frame(PTS, P3M, P2M, REB = OREB + DREB, 
                  AST, TOV, STL, BLK)
detach(Pbox)
data <- subset(data, Pbox$MIN >= 1500)
id <- Pbox$Player[Pbox$MIN >= 1500]

mds <- MDSmap(data)
plot(mds, labels = id)

selp <- which(id == "Al Horford" | id == "Kyle Kuzma" | 
              id == "Myles Turner" | id == "Kyle Korver" |
              id == "Andrew Wiggins")
steph <- which(id == "Stephen Curry" | id == "Kevin Durant")
plot(mds, labels = id, subset = steph, col.subset = "tomato")
plot(mds, labels = id, subset = selp, col.subset = "tomato")
plot(mds, labels = id, subset = selp, col.subset = "tomato",
     zoom = c(0,3,0,2))

#76
plot(mds, z.var = c("P2M", "P3M", "AST", "REB"), 
     level.plot = FALSE, palette = topo.colors)
plot(mds, z.var = c("P2M", "P3M", "AST", "REB"), 
      palette = topo.colors, contour = TRUE)

#78
PbP.GSW <- subset(pbp, team == "GSW")
netdata <- assistnet(PbP.GSW)

set.seed(7)
plot(netdata)

#79
set.seed(7)
plot(netdata, layout = "circle", edge.thr = 20)

#81
cols <- paste0(c("a","h"), rep(1:5, each=2))
PbP.GSW.DG0 <- PbP.GSW[!apply(PbP.GSW[,cols], 1, "%in%", 
                              x = "Draymond Green"),]
netdata.DG0 <- assistnet(PbP.GSW.DG0)
set.seed(1)
plot(netdata.DG0)

#82
PbP.GSW.DG0 <- subset(PbP.GSW.DG0, ShotType=="2P" | ShotType == "3P")
p0 <- mean(PbP.GSW.DG0$points)
p10 <- mean(PbP.GSW.DG0$playlength)

PbP.GSW.DG1 <- PbP.GSW[apply(PbP.GSW[,cols], 1, "%in%", 
                              x = "Draymond Green"),]
PbP.GSW.DG1 <- subset(PbP.GSW.DG1, ShotType=="2P" | ShotType == "3P")
p1 <- mean(PbP.GSW.DG1$points)
p11 <- mean(PbP.GSW.DG1$playlength)

#83
plot(netdata, layout = "circle", edge.thr = 20,
     node.col = "FGPTS_AST", node.size = "ASTPTS")
plot(netdata, layout = "circle", edge.thr = 20,
     node.col = "FGPTS", node.size = "FGPTS_ASTp")

TAB <- netdata$assistTable
X <- netdata$nodeStats

names(X)[1] <- "Player"
data <- merge(X, Pbox, by = "Player")

#85
mypal <- colorRampPalette(c("blue", "yellow", "red"))
scatterplot(data, data.var = c("FGM", "FGM_ASTp"), z.var = "MIN", 
            labels = data$Player, palette = mypal, repel_labels = TRUE)

sel <- which(data$MIN > 984)
tab <- TAB[sel,sel]

#86
no.pl <- nrow(tab)
pR <- pM <- vector(no.pl, mode="list")
GiniM <- array(NA, no.pl)
GiniR <- array(NA, no.pl)
for(pl in 1:no.pl) {
  ineqplM <- inequality(tab[,pl], npl = no.pl)
  GiniM[pl] <- ineqplM$Gini
  ineqplR<- inequality(tab[,pl], npl = no.pl)
  GiniR[pl] <- ineqplR$Gini
  title <- rownames(tab)[pl]
  pM[[pl]] <- plot(ineqplM, title=title)
  pR[[pl]] <- plot(ineqplR, title=title)
}
library(gridExtra)
grid.arrange(grobs=pM, nrow=2)
grid.arrange(grobs=pR, nrow=2)

#88
library(vcd)
assocstats(tab)

XX <- data.frame(X[sel,], GiniM, GiniR)
labs <- c("Gini Index for assists made",
          "Gini Index for assists received",
          "Assists Received", "Assists Made")
bubbleplot(XX, id = "Player", x= "GiniM", y = "GiniR",
           col = "FGM_AST", size = "AST",
           labels = labs, text.size = 4)

#89
library(tidygraph)
library(igraph)
library(CINNA)

net1 <- as_tbl_graph(netdata$assistNet)
class(net1) <- "igraph"

#90
centr_degree(net1)
alpha_centrality(net1)
closeness(net1, mode = "all")
betweenness(net1)
calculate_centralities(net1)

#91
data.team <- subset(pbp, team == "GSW" & result != "")
data.opp <- subset(pbp, team!= "GSW" & result != "")

densityplot(data = data.team, shot.type = "2P",
            var="periodTime", best.scorer = TRUE)
densityplot(data = data.team, shot.type = "2P",
            var="totalTime", best.scorer = TRUE)
densityplot(data = data.team, shot.type = "2P",
            var="playlength", best.scorer = TRUE)
densityplot(data = data.team, shot.type = "2P",
            var="shot_distance", best.scorer = TRUE)
densityplot(data = data.opp, shot.type = "2P",
            var="periodTime", best.scorer = TRUE)
densityplot(data = data.opp, shot.type = "2P",
            var="totalTime", best.scorer = TRUE)
densityplot(data = data.opp, shot.type = "2P",
            var="playlength", best.scorer = TRUE)
densityplot(data = data.opp, shot.type = "2P",
            var="shot_distance", best.scorer = TRUE)

#94
KD <- subset(pbp, player == "Kevin Durant" & result != "")
SC <- subset(pbp, player == "Stephen Curry" & result != "")
densityplot(data = KD, shot.type = "field", var = "playlength")
densityplot(data = KD, shot.type = "field", var = "shot_distance")
densityplot(data = SC, shot.type = "field", var = "playlength")
densityplot(data = SC, shot.type = "field", var = "shot_distance")

#96
pbp$xx <- pbp$original_x/10
pbp$yy <- pbp$original_y/10 -41.75
KT <- subset(pbp, player == "Klay Thompson")

shotchart(data = KT, x = "xx", y = "yy", type = "density-polygons")
shotchart(data = KT, x = "xx", y = "yy", type = "density-raster")
shotchart(data = KT, x = "xx", y = "yy", type = "density-hexbin")

shotchart(data = KT, x = "xx", y = "yy", type = "density-polygons",
          scatter = TRUE)
shotchart(data = KT, x = "xx", y = "yy", type = "density-raster",
          scatter = TRUE, pt.col = "tomato", pt.alpha = .25)
shotchart(data = KT, x = "xx", y = "yy", type = "density-hexbin",
          nbins = 50, palette = "bwr")

#98
data <- subset(Pbox, MIN >= 500)
attach(data)
X <- data.frame(PTS, P3M, P2M, REB = OREB + DREB, AST)/MIN
detach(data)
scatterplot(X, data.var = 1:5, lower = list(continuous = "density"),
            diag = list(continuous = "densityDiag"))
