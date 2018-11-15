#Lab3 - Challenge (Optional)
#Author: Leila Erbay
#Purpose: Test R Knowledge

setwd("/Users/LeilaErbay/Desktop/LevelNeu2018/Labs/Lab3")
library(readxl)
pkmn2 <- read_excel("pokemon.xlsx", 2)
pkmn3 <- read_excel("pokemon.xlsx", 3)
pkmn4 <- read_excel("pokemon.xlsx", 4)

########### CHALLENGE PART 1  #########################
bestMove <- pkmn2[order(-pkmn2$PP),][1:12,]
highestLevels <-pkmn3[order(-pkmn3$Level),][,]
mostEffective <- pkmn4[pkmn4$Effectiveness=="Super Effective",][1:12,]

#*****ASSUMING THAT THE LEVELS in Sheet 3 are the current levels of the Pokemon****
#with greater level is more power thus prioritize:
  # Level
  # Type (Diversify)
  # Effectiveness

type1 <- pkmn[pkmn$Name == "Zweilous",][1,] #type1 = DARK
type2 <- pkmn[pkmn$Name == "Larvesta",][1,] #type2 = BUG
type3 <- pkmn[pkmn$Name == "Dragonair",][1,] #type3 = DRAGON
type4 <- pkmn[pkmn$Name == "Pupitar",][1,]   #type4 = ROCK
type5 <- pkmn[pkmn$Name == "Vullaby",][1,] #type5 = DARK
type6 <-pkmn[pkmn$Name == "Rufflet",][1,] #type6 = NORMAL 

#Team: Zweilous, Larvesta, Dragonair, Pupitar, Vullaby, Rufflet
team <- rbind(type1, type2, type3, type4, type5, type6 )
sumTotal <- sum(team$Total)   # total = 2330, team works
  
################ CHALLENGE PART 2 ##########################
#Best Choices:
# https://www.eurogamer.net/articles/2018-01-15-pokemon-go-type-chart-effectiveness-weaknesses
      # Rock, Ghost, Ice, Dragon, Fairy, Dark

#COLLECTING ALL PKMNs of a certain Type
rocks <- pkmn [pkmn$Type == "ROCK",][,] 
ghosts <- pkmn[pkmn$Type == "GHOST",][,]
ices <- pkmn[pkmn$Type == "ICE",][,]
dragons <-pkmn[pkmn$Type == "DRAGON",][,]
fairys <- pkmn[pkmn$Type == "FAIRY",][,]
darks <- pkmn[pkmn$Type == "DARK",][,]

#Finding the highest level of each type inside rocks
rock <- data.frame(highestLevels$`Evolving from` %in% rocks$Name) #Pupitar, #4 in Levels
ghost <- data.frame(highestLevels$`Evolving from` %in% ghosts$Name) #Litwick, #29 in Levels
ice <- data.frame(highestLevels$`Evolving from` %in% ices$Name) #Vanillish  #18 in Level
dragon <- data.frame(highestLevels$`Evolving from` %in% dragons$Name) #Dragonair, #3 in levels 
fairy <- data.frame(highestLevels$`Evolving from` %in% fairys$Name) # Snubbull, #182 in Levels
dark <- data.frame(highestLevels$`Evolving from` %in% darks$Name) #Zweilous #1 in Levels
#5 Dragon = Sligoo
#7 Dragon = Rufflet
#10 Dragon= Shelgon

#Ghost is Low

#IDEAL TEAM: Zweilous, Dragonair, Pupitar, Sligoo, Rufflet, Shelgon
#Backup Pokemon: Litwick, Vanillish, Snubbull


