rm(list=ls())
Data = list(1)
Teams = list(1)
UTeams = list(1)
SixTeams = list(1)
UQSixTeams = list(1)

#importing and formating data
for(i in 1960:2010){
  fwf= paste("http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf",i,"gms.txt",sep="")

  Data[[i]]= read.fwf(fwf,c(11,28,2,30,2))
  names(Data[[i]]) = c("date", "awayteam", "awayscore", "hometeam", "homescore")
  Data[[i]]$awayteam = gsub(" ","",Data[[i]]$awayteam)
  Data[[i]]$hometeam = gsub(" ","",Data[[i]]$hometeam)
  Teams[[i]] = sort(unlist(list(Data[[i]]$awayteam,Data[[i]]$hometeam)))
  UTeams[[i]] = unique(Teams[[i]])
  
  #Teams that played over 6 games in the season.
  SixTeams[[i]] = UTeams[[i]][which(table(Teams[[i]])>6)]
  
  #SHOWS teams that played each other and both had over 6 games played. 
  Data[[i]] = Data[[i]][which(Data[[i]]$awayteam %in% SixTeams[[i]] & Data[[i]]$hometeam %in% SixTeams[[i]]),]
  UQSixTeams[[i]] = unique(sort(unlist(list(Data[[i]]$awayteam,Data[[i]]$hometeam))))
}

#putting all data into one frame
AllData = Data[[1960]]
for(i in 1961:2010){
  AllData = rbind(AllData, Data[[i]])
}
#removing tie games
AllData = AllData[!(AllData$awayscore==AllData$homescore),]

Win1 = list(1)
Loss1 = list(1)
Win2 = list(1)
Loss2 = list(1)
Win = list(1)
Loss = list(1)
CountW = list(1)
CountL = list(1)
WLData = list(1)

for(i in 1960:2010){
  Win1[[i]] = Data[[i]][which(Data[[i]]$awayscore > Data[[i]]$homescore),]$awayteam
  Loss1[[i]] = Data[[i]][which(Data[[i]]$awayscore > Data[[i]]$homescore),]$hometeam
  
  Win2[[i]] = Data[[i]][which(Data[[i]]$awayscore < Data[[i]]$homescore),]$hometeam
  Loss2[[i]] = Data[[i]][which(Data[[i]]$awayscore < Data[[i]]$homescore),]$awayteam
}

for(i in 1960:2010){
  season = i
  Win[[i]] = list(Win1[[i]],Win2[[i]])
  Win[[i]] = unlist(Win[[i]])
  Win[[i]] = data.frame(Team = Win[[i]],Season = season)

  Loss[[i]] = list(Loss1[[i]],Loss2[[i]])
  Loss[[i]] = unlist(Loss[[i]]) 
  Loss[[i]] = data.frame(Team = Loss[[i]],Season = season)
  
  Win[[i]] = droplevels(Win[[i]])
  Loss[[i]] = droplevels(Loss[[i]])
  CountW[[i]] = as.vector(table(Win[[i]]))
  CountL[[i]] = as.vector(table(Loss[[i]]))
 
  Win[[i]] = data.frame(table(Win[[i]]))
  Loss[[i]] = data.frame(table(Loss[[i]]))
  names(Win[[i]]) = c("Team","Season","Win")
  names(Loss[[i]]) = c("Team","Season","Loss")
  
  WLData[[i]] = merge(Win[[i]],Loss[[i]],all=TRUE)
  WLData[[i]][is.na(WLData[[i]])] = 0
  temp = gsub(" ","",WLData[[i]]$Team)
  WLData[[i]] = WLData[[i]][order(temp),]
}

#Win-Loss Data Frame
WLD = WLData[[1960]]
for(i in 1961:2010){
  WLD = rbind(WLD, WLData[[i]])
}

WLD = data.frame(WLD)
WLD = WLD[(order(WLD$Team)),]
names(WLD) = c("Team","Season", "Win","Loss")

UniqueAll = unique(WLD$Team)
UniqueAll = gsub(" ","",UniqueAll)
UniqueAll = sort(UniqueAll)
ID = c(1:length(UniqueAll))
IDFrame = data.frame(ID, Teams = UniqueAll)
names(IDFrame) = c("ID","Team")

#ID is added for each team 
WLD2 = merge(IDFrame, WLD, "Team", all.y=TRUE)
WLD2 = data.frame(WLD2$ID, WLD2$Season, WLD2$Team, WLD2$Win, WLD2$Loss)
names(WLD2) = c("ID", "Season","Team", "Wins", "Losses")

#Who played who
Played = list(1)
TeamPlayed = list(1)
TeamPlayedID = list(1)
Opponents = list(1)
DF= list(1)
L=1

for(i in 1960:2010){
  L=L
  for(j in UQSixTeams[[i]]){
     Played[[L]]=c(Data[[i]][which(Data[[i]]$hometeam == j),]$awayteam,
                   Data[[i]][which(Data[[i]]$awayteam == j),]$hometeam)
     TeamPlayed[[L]]= cbind(j, i, Played[[L]])
     colnames(TeamPlayed[[L]]) = c("Actual", "Season", "Team")
     TeamPlayedID[[L]]=merge(TeamPlayed[[L]],IDFrame,"Team",all.x=TRUE)
     Opponents[[L]]=as.vector(TeamPlayedID[[L]]$ID)
     DF[[L]]= list(Team = TeamPlayedID[[L]][1,2],Season = TeamPlayedID[[L]][1,3],Opponents = Opponents[[L]])
     names(DF[[L]]) = c("Team", "Season","Opponents")
     DF[[L]]=c(as.vector(unlist(DF[[L]][1:2])),DF[[L]][3])
     L=L+1
  }
}

AllDF = DF[[1]]
for(i in 2:length(DF)){
  AllDF = rbind(AllDF, DF[[i]])
}
colnames(AllDF) = c("Team", "Season","Opponents")
row.names(AllDF) = 1:nrow(AllDF)

DF2 = merge(WLD2,AllDF,by = c("Season","Team"))
colnames(DF2) = c("Season","Team","ID","Win","Loss","Opponents")
DF = DF2[,c("Season", "Team","Win","Loss","Opponents")]
#Data is now in the correct form


colley1 = function(YEAR){
  nw = DF[which(DF$Season==YEAR),]$Win
  nl = DF[which(DF$Season==YEAR),]$Loss
  ntot = nw+nl
  opp = DF[which(DF$Season==YEAR),]$Opponents
  
  b = 1+(nw-nl)/2
  cii = 2+ ntot
  
  C = diag(cii)
  
  for (i in 1:length(which(DF$Season==YEAR))) {
    t = table(opp[i])
    for (j in 1:length(which(DF$Season==YEAR)))
      if (i!=j){
        C[i,j]= ifelse(j %in% opp[[i]], -t[names(t)==j], 0) 
      }
  }
  
  r = solve(C,b)
  
  SolutionOrg = data.frame(Team = DF[which(DF$Season==YEAR),]$Team, Score = r) 
  Solutions = SolutionOrg[order(-r),]
  rownames(Solutions) = 1:length(which(DF$Season==YEAR))
  
  View(Solutions)
}