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

