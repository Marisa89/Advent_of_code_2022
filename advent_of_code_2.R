setwd("Advent_code/2/")
results<- read.csv("ac_2_1", sep = " ", header = F)
#A for Rock, B for Paper, and C for Scissors
#X for Rock, Y for Paper, and Z for Scissors
results$V2<-results$V2 %>% gsub("Z", "SCISSORS", .) %>% gsub("X", "ROCK", .) %>% gsub("Y", "PAPER", .)
results$V1<-results$V1 %>% gsub("C", "SCISSORS", .)%>% gsub("A", "ROCK", .) %>% gsub("B", "PAPER", .) 

#Add column with wins, draw, loss
#add column with value for wins/draw/loss
#add value of you game

 
for (i in 1:nrow(results)){
  if(results$V1[i] == results$V2[i]){
    results$output[i]<- print("draw")}
  else{if (results$V1[i] == "ROCK" & results$V2[i]=="SCISSORS"||results$V1[i] == "PAPER" & results$V2[i]=="ROCK"||results$V1[i] == "SCISSORS" & results$V2[i]=="PAPER"){
    results$output[i]<- print("loss")}
    else{results$output[i]<- print("win")}}} 

#points
#1 for Rock, 2 for Paper, and 3 for Scissors
results$points_my_game<-as.numeric(results$V2 %>% gsub( "SCISSORS","3", .)%>% gsub("ROCK", "1", .) %>% gsub( "PAPER","2",  .))
#0 if you lost, 3 if the round was a draw, and 6 if you won
results$points_output<-as.numeric(results$output %>% gsub( "loss","0", .)%>% gsub("win", "6", .) %>% gsub( "draw","3",  .))

#sum the value of the points obtained for your game and the results for each game
for (i in 1:nrow(results)){
results$points[i]<-sum(results$points_my_game[i], results$points_output[i], na.rm = TRUE)}

#sum all the points
sum(results$points)


##SECOND PART
results<- read.csv("ac_2_1", sep = " ", header = F)
#X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win
results$output<-results$V2 %>% gsub("X", "loss", .) %>% gsub("Y", "draw", .) %>% gsub("Z", "win", .)
results$V1<-results$V1 %>% gsub("C", "SCISSORS", .)%>% gsub("A", "ROCK", .) %>% gsub("B", "PAPER", .) 

for (i in 1:nrow(results)){
  if(results$output[i] == "draw"){
    results$V2[i]<- results$V1[i]}
  else{if(results$output[i] == "win" & results$V1[i] == "SCISSORS"){
    results$V2[i]<- print("ROCK")}
    if(results$output[i] == "win" & results$V1[i] == "ROCK"){
      results$V2[i]<- print("PAPER")}
    if(results$output[i] == "win" & results$V1[i] == "PAPER"){
      results$V2[i]<- print("SCISSORS")}
    if(results$output[i] == "loss" & results$V1[i] == "SCISSORS"){
      results$V2[i]<- print("PAPER")}
    if(results$output[i] == "loss" & results$V1[i] == "ROCK"){
      results$V2[i]<- print("SCISSORS")}
    if(results$output[i] == "loss" & results$V1[i] == "PAPER"){
      results$V2[i]<- print("ROCK")}}}


#1 for Rock, 2 for Paper, and 3 for Scissors
results$points_my_game<-as.numeric(results$V2 %>% gsub( "SCISSORS","3", .)%>% gsub("ROCK", "1", .) %>% gsub( "PAPER","2",  .))
#0 if you lost, 3 if the round was a draw, and 6 if you won
results$points_output<-as.numeric(results$output %>% gsub( "loss","0", .)%>% gsub("win", "6", .) %>% gsub( "draw","3",  .))

for (i in 1:nrow(results)){
  results$points[i]<-sum(results$points_my_game[i], results$points_output[i], na.rm = TRUE)}

sum(results$points)
