install.packages("BasketballAnalyzeR")
library(BasketballAnalyzeR)
library(gridExtra)
RNGkind(sample.kind = "Rounding")

Pbox.sel <- subset(Pbox, MIN >= 500)
X <- Pbox.sel$AST/Pbox.sel$MIN
Y <- Pbox.sel$TOV/Pbox.sel$MIN
Pl <- Pbox.sel$Player
out <- simplereg(x=X, y=Y, type = "lin")
xtitle <- "AST per Minute"
ytitle <- "TOV per Minute"
plot(out, xtitle = xtitle, ytitle = ytitle)

#159
selp <- which(Pl=="Damian Lillard")
plot(out, labels = Pl, subset = selp, xtitle = xtitle, ytitle = ytitle)

plot(out, labels = Pl, subset = "quant", Lx = 0, Ux = .97,
     Ly = 0, Uy = .97, xtitle = xtitle, ytitle = ytitle)

#161
Pbox.sel <- subset(Pbox, MIN >= 500)
X <- (Pbox.sel$DREB + Pbox.sel$OREB)/Pbox.sel$MIN
Y <- Pbox.sel$P3M/Pbox.sel$MIN
Pl <- Pbox.sel$Player

#162
out <- simplereg(x=X, y=Y, type = "lin")
xtitle <- "REB per Minute"
ytitle <- "P3M per Minute"
plot(out, xtitle = xtitle, ytitle = ytitle)

#163
out <- simplereg(x=X, y=Y, type = "pol")
plot(out, labels=Pl, subset = "quant", Lx = 0,
     Ux = .9, Ly = 0, Uy = .9, xtitle = xtitle, ytitle = ytitle)

#164
data <- subset(Pbox, MIN >= 500)
x <- data.frame(data$PTS, data$P3M, data$P2M, 
                REB = data$OREB + data$DREB, data$AST)/data$MIN
scatterplot(x, data.var = 1:5, lower = list(continuous = "smooth_loess"),
            diag = list(continous = "barDiag"))

#166
PbP.GSW <- subset(pbp, team == "GSW" & result != "")
p1 <- scoringprob(data = PbP.GSW, shot.type = "3P",
                  var = "playlength")
p2 <- scoringprob(data = PbP.GSW, shot.type = "3P",
                  var = "periodTime", bw = 300)
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

pl1 <- c("Kevin Durant", "Draymond Green", "Klay Thompson")
p1 <- scoringprob(data= PbP.GSW, shot.type = "2P",
                  players = pl1, var = "shot_distance",
                  col.team = "gray")
pl2 <- c("Kevin Durant", "Draymond Green")
p2 <- scoringprob(data = PbP.GSW, shot.type = "2P",
                  players = pl2, var = "totalTime", 
                  bw = 1500, col.team = "gray")
grid.arrange(p1, p2, ncol = 2)

#168
pl <- c("Stephen Curry", "Kevin Durant")
mypal <- colorRampPalette(c("Red", "Green"))
expectedpts(data = PbP.GSW, players = pl,
            col.team = "gray", palette = mypal,
            col.hline = "gray")
#170
Pbox.GSW <- subset(Pbox, PTS >= 500 & Team == "Golden State Warriors")
pl <- Pbox.GSW$Player
expectedpts(data = PbP.GSW, players = pl,
            col.team = "gray", palette = mypal,
            col.hline = "gray")

#171
expectedpts(data = PbP.GSW, bw = 300, players = pl, 
            col.team = "gray", palette = mypal,
            col.hline = "gray", var = "periodTime",
            xlab = "Period Time")

#172
top <- subset(Tadd, Playoff == "Y" & Team != "GSW")$team
bot <- subset(Tadd, Playoff == "N")$team

bot_top <- function(x, k) {
  dts <- subset(subset(x, oppTeam %in% get(k)),
                team == "GSW")
  dts$player <- paste(dts$player, k)
  return(dts)
}
PbP.GSW <- rbind(bot_top(pbp, "top"),
                 bot_top(pbp, "bot"))
p1 <- c("Stephen Curry top", "Stephen Curry bot",
        "Kevin Durant top", "Kevin Durant bot")
mypal <- colorRampPalette(c("red", "green"))
expectedpts(data = PbP.GSW, bw = 1200, players = p1,
            col.team = "gray", palette = mypal, 
            col.hline = "gray", var = "totalTime", 
            xlab = "Total Time")
