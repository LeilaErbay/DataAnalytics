#Lab3 - Pokemon
#Author: Leila Erbay
#Purpose: Practice R basics


#Do we need to explicitly install the package in the R script?
#install.packages("readxl")
library(readxl)       # load readxl
setwd("/Users/LeilaErbay/Desktop/LevelNeu2018/Labs/Lab3")

#pkmn : var pointing to the xl file
pkmn <- read_excel("pokemon.xlsx", 1) 

head(pkmn)
tail(pkmn)

duplicated(pkmn$Name)   #finding duplicated name
pkmn$pkmn_dupes <- duplicated(pkmn$Name)  #assigning pkmn_dupes as logical of duplicated Names
head(pkmn)

#filtering out duplicates: if duplicated == false leave it in pkmn
pkmn <- pkmn[pkmn$pkmn_dupes == "FALSE",] 

#make table of Type of Pokemon
table(pkmn$Type)


#show median, min, max, first and third quartiles
summary(pkmn$Attack)

#if pkmn type == poison, give the attack summary on poisonous pokemon
summary(pkmn[pkmn$Type == "POISON",]$Attack)

#6 highest attack Pokemon
  #order attack col by descending
#order (-attribute) = descending
#order (attribute) = ascending
highAttack <- pkmn[order(-pkmn$Attack),]
highAttack

#first 6 of the highest attack fxn
highAttack[1:6,]
head(highAttack)  #better view of 6 poken with the highest attack

#first 6 with highest attack in 1 line:
highAttack <- pkmn[order(-pkmn$Attack),][1:6,]
head(highAttack)
#Pokemon with highest attack (Names):
#1 Mega Mewtwo X
#2 Mega Heracross
#3 Deoxys- Attack
#4 Mega Garchomp
#5 Black Kyurem
#6 Mega Banette

highAttack <-pkmn[order(-pkmn$Attack),][1:2,]
#Top 2 High Attack
  #1 Mega Mewtwo X
  #2 Mega Heracross

highDefense <- pkmn[order(-pkmn$Defense),][1:2,]
# Top 2 High Defense
  #1 Shuckle
  #2 Mega Aggron

highSpeed <- pkmn[order(-pkmn$Speed),][1:2,]
#Top 2 High Speed
  #1 Deoxys - Speed Forme
  #2 Ninjask

topPkmn <- rbind(highAttack, highDefense, highSpeed)
topPkmn
#1 Overall Pokemon based on Attack, Speed, Defense: 
  #Mega Mewtwo X

#psychic pokemon
psychic <- pkmn[pkmn$Type == "PSYCHIC",]

highAttackPsychic <- pkmn[order(-pkmn$Attack),][1:2,]
#Top 2 Psychic with high attack
  #1 Mega Mewtwo X
  #2 Mega Heracross 

highDefensePsychic <- pkmn[order(-pkmn$Defense),][1:2,]
# Top 2 Psychic based on defense
  #1 Schuckle
  #2 Mega Aggron

highSpeedPsychic <- pkmn[order(-pkmn$Speed),][1:2,]
# Top 2 Psychic based on Speed
  #1 Deoxys - Speed Forme
  #2 Ninjask

topPsychic <- rbind(highAttackPsychic, highDefensePsychic, highSpeedPsychic)
topPsychic
#Top Psychic : Mega Mewtwo X

#grass pokemon
grass <- pkmn[pkmn$Type == "GRASS"]
slow <- pkmn[order(pkmn$Speed),][1:6,]
slow
#Slowest Grass Pokemon
  #1 Shuckle
  #2 Munchlax
  #3 Trapinch
  #4 Bonsly
  #5 Ferroseed
  #6 Slowpoke

#worst pokemon: lowest Total using order function
worst <- pkmn[order(pkmn$Total),][1,]
worst

worstPkmn <- pkmn[pkmn$Total== min(pkmn$Total),]
worstPkmn
#Worst pkmn : Sunkern

medPkmn <- pkmn[pkmn$Total == median(pkmn$Total),]
medPkmn
#Tie of Mediocre Pokemon based on Total
  #1 Persian
  #2 Seadra
  #3 Vigoroth
  #4 Luntone
  #5 Solrock
  #6 Kecleon
  #7 Rotom
  #8 Klang

#Ideal Pokemon Lineup
# Best in Attack, Special Attack, Speed = injure enemy faster
bestAttack <- pkmn[order(-pkmn$Attack),][1,]
bestSpecAttack <- pkmn[pkmn$`Special Attack` == max(pkmn$`Special Attack`),][1,]
bestSpeed <- highSpeed
bestTotal <- pkmn[pkmn$Total == max(pkmn$Total),][1,]
medAllAround <- medPkmn
#Ideal Pkmn Team (top 5):
  #1 Mega Mewtwo X
  #2 Mega Mewtwo Y
  #3 Deoxys - Speed Forme
  #4 Persian
  #5 Seadra
  #6 Vigoroth







