install.packages("BasketballAnalyzeR")
library(BasketballAnalyzeR)
library(gridExtra)
RNGkind(sample.kind = "Rounding")

data=(package="BasketballAnalyzeR")

#23
pbp <- PbPmanipulation(PbP.BDB)

#26
tm <- c("BOS", "CLE", "GSW", "HOU")
selTeams <- which(Tadd$team %in% tm)
FF.sel <- fourfactors(Tbox[selTeams,], Obox[selTeams,])
plot(FF.sel)

#27
FF <- fourfactors(Tbox, Obox)
listPlots <- plot(FF)

grid.arrange(grobs = listPlots[1:2], ncol = 1)

X <- data.frame(Tbox, PTS.O=Obox$PTS, TOV.O=Obox$TOV, CONF = Tadd$Conference)
XW <- subset(X, CONF=="W")
labs <- c("Steals", "Blocks", "Defensive Rebounds")
barline(data=XW, id="Team", bars = c("STL", "BLK", "DREB"), 
        line = "TOV.O", order.by = "PTS.O", labels.bars = labs)

#30
Pbox.HR <- subset(Pbox, Team == "Houston Rockets" & MIN >= 500)
barline(data = Pbox.HR, id = "Player", bars = c("P2p", "P3p", "FTp"),
        line = "MIN", order.by = "PM", labels.bars = c("2P%", "3P%", "FT%"),
        title = "Houston Rockets")

#31
Pbox.PG <- subset(Pbox, Player=="Russell Westbrook" |
                 Player=="Stephen Curry" |
                 Player=="Chris Paul" |
                 Player=="Kyrie Irving" |
                 Player=="Damian Lillard" |
                 Player=="Kyle Lowry" |
                 Player=="John Wall" |
                 Player=="Rajon Rondo" |
                 Player=="Kemba Walker")
attach(Pbox.PG)
X <- data.frame(P2M, P3M, FTM, REB = OREB+DREB, AST, STL, BLK)/MIN
detach(Pbox.PG)
radialprofile(data=X, title=Pbox.PG$Player, std=FALSE)

#32
radialprofile(data=X, title=Pbox.PG$Player, std=TRUE)

#34
Pbox.sel <- subset(Pbox, MIN >= 500)
attach(Pbox.sel)
X <- data.frame(AST, TOV, PTS)/MIN
detach(Pbox.sel)
mypal <- colorRampPalette(c("blue", "yellow", "red"))
scatterplot(X, data.var=c("AST", "TOV"), z.var = "PTS", 
            labels=1:nrow(X), palette= mypal)

#36
SAS <- which(Pbox.sel$Team == "San Antonio Spurs")
scatterplot(X, data.var = c("AST", "TOV"), z.var = "PTS", 
            labels= Pbox.sel$Player, palette = mypal,
            subset = SAS)
scatterplot(X, data.var = c("AST", "TOV"), z.var = "PTS", 
            labels= Pbox.sel$Player, palette = mypal,
            zoom = c(.2, .325, .05, .1))

#37
attach(Tbox)
X <- data.frame(T=Team, P2p, P3p, FTp, AS= P2A, P3A, FTA)
detach(Tbox)
labs <- c("2-point shots (% made)",
          "3-point shots (% made)",
          "free throws (% made)",
          "Total shots attempted")
bubbleplot(X, id="T", x = "P2p", y = "P3p", col = "FTp",
           size = "AS", labels = labs)

#38
## Cannot get viz to match, Chris Boucher will not go away
Pbox.GSW.CC <- subset(Pbox, Team == "Golden State Warriors" |
                        Team == "Cleveland Cavaliers" & MIN >= 500 &
                        Player != "Chris Boucher")
attach(Pbox.GSW.CC)
X <- data.frame(ID=Player, Team, v1 = DREB/MIN, v2 = STL/MIN, 
                v3 = BLK/MIN, v4 = MIN)
detach(Pbox.GSW.CC)
labs <- c("Defensive Rebounds", "Steals", "Blocks", "Total Minutes Played")
bubbleplot(X, id = "ID", x = "v1", y = "v2", col = "v3", size = "v4",
           text.col = "Team", labels = labs, title = "GSW and CC during the regular season",
           text.legend = TRUE, text.size = 3.5, scale = TRUE)

#41
Pbox.OKC <- subset(Pbox, Team=="Oklahoma City Thunder" & MIN >= 500)
vrb1 <- variability(data=Pbox.OKC, data.var = "P3p", size.var = "P3A")

#43 
vrb2 <- variability(data = Pbox.OKC, data.var = c("P2p", "P3p", "FTp"),
                    size.var = c("P2A", "P3A", "FTA"), weight = TRUE)
plot(vrb2, title = "Variability diagram - OKC")

#46-47
Pbox.BN <- subset(Pbox, Team == "Brooklyn Nets")
ineqBN <- inequality(Pbox.BN$PTS, nplayers = 8)
Pbox.MB <- subset(Pbox, Team == "Milwaukee Bucks")
ineqMB <- inequality(Pbox.MB$PTS, nplayers = 8)
p1 <- plot(ineqBN, title = "Brooklyn Nets")
p2 <- plot(ineqMB, title = "Milwaukee Bucks")
grid.arrange(p1, p2, nrow = 1)

#47-48
no.teams <- nrow(Tbox)
INEQ <- array(0, no.teams)
for (k in 1:no.teams) {
  Teamk <- Tbox$Team[k]
  Pbox.sel <- subset(Pbox, Team == Teamk)
  index <- inequality(Pbox.sel$PTS, nplayers = 8)
  INEQ[k] <- index$Gini
}
dts <- data.frame(INEQ, PTS = Tbox$PTS,
                  CONF = Tadd$Conference)
mypal <- colorRampPalette(c("blue", "red"))
scatterplot(dts, data.var = c("INEQ", "PTS"), z.var = "CONF",
            labels = Tbox$Team, palette = mypal, repel_labels = TRUE)

#49
PbP.GSW <- subset(pbp, team=="GSW")
lineup <- c("Stephen Curry", "Kevin Durant", "Klay Thompson",
            "Draymond Green", "Zaza Pachulia")
filt5 <- apply(PbP.GSW[,4:13], 1, 
               function(x) {
                 x <- as.character(x)
                 sum(x %in% lineup) == 5
               })
subPbP.GSW <- PbP.GSW[filt5, ]
PTS5 <- sapply(lineup, 
               function(x) {
                 filt <- subPbP.GSW$player==x
                 sum(subPbP.GSW$points[filt], na.rm = TRUE)
               })
inequality(PTS5, nplayers = 5)

#50
PbP.GSW.DET <- subset(pbp, team == "GSW" & oppTeam == "DET")
filt51 <- apply(PbP.GSW.DET[,4:13], 1, 
               function(x) {
                 x <- as.character(x)
                 sum(x %in% lineup) == 5
               })
subPbP.GSW.DET <- PbP.GSW.DET[filt51, ]
PTS51 <- sapply(lineup, 
               function(x) {
                 filt <- subPbP.GSW.DET$player==x
                 sum(subPbP.GSW.DET$points[filt], na.rm = TRUE)
               })
inequality(PTS51, nplayers = 5)

#51
subdata <- subset(pbp, player == "Kevin Durant")
subdata$xx <- subdata$original_x/10
subdata$yy <- subdata$original_y/10-41.75

shotchart(data=subdata, x="xx", y = "yy", scatter = TRUE)

#52
shotchart(data=subdata, x = "xx", y = "yy", z = "playlength",
          num.sect = 5, type = "sectors", scatter = TRUE)
shotchart(data=subdata, x = "xx", y = "yy",
          nbins = 15, type = "density-hexbin", palette = "mixed")
