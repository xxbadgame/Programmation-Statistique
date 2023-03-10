library(readxl)
pokemon <- read_excel("pokemon.xlsx")

## Exercise 1

# Les dimensions

Dismensions = dim(pokemon)
NomColumn = names(x = pokemon)
AllTypes = pokemon$type

# Conversion en factor

pokemon$generation = as.factor(pokemon$generation)
factor(pokemon$is_legendary)
factor(pokemon$type)

# Les levels

levels(factor(pokemon$generation))
levels(factor(pokemon$is_legendary))
levels(factor(pokemon$is_legendary))

str(pokemon)

## Exercise 2

moyennePoids = mean(pokemon$weight_kg, na.rm = T)
mediannePoids = median(pokemon$weight_kg, na.rm = T)
quartileTailles = quantile(x = pokemon$height_m, na.rm = T)
décilesTailles = quantile(x = pokemon$height_m, probs = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), na.rm = T)
variancePoids = var(x=pokemon$height_m, na.rm = T)
ecartTypePoids = sd(x=pokemon$height_m, na.rm = T)

EffectifGeneration <- sort(table(x = pokemon$generation), decreasing = TRUE)
EffectifIs_Legendary <- sort(table(x = pokemon$is_legendary), decreasing = TRUE)
EffectifType <- sort(table(x = pokemon$type), decreasing = TRUE)


print(EffectifType)

## Exercise 3

requete_0 <- pokemon[,1:2]
dim(requete_0)

requete_1 <- pokemon[,c("nom", "is_legendary")]
requete_1
dim(requete_1)

requete_2 <- pokemon[1:50,1:2]
requete_2
dim(requete_2)

requete_3 <- pokemon[1:10,]
requete_3
dim(requete_3)

requete_4 <- pokemon[,1:ncol(pokemon)-1]
requete_4 
dim(requete_4)

requete_5 <- data.frame(sort(pokemon$nom, decreasing = F))
requete_5 
dim(requete_5[1,])

requete_6 <- data.frame(sort(pokemon$weight_kg, decreasing = T))
requete_6
dim(requete_6[1,])

requete_7 <- pokemon[order(-pokemon$attack,pokemon$speed),]
requete_7
dim(requete_7[1:10,])

## Exercise 4

requete_40 <- pokemon[ pokemon$type == "fire", c("nom","type")]
requete_40
dim(requete_40)

filtre <- pokemon[pokemon$attack >= 150,c("nom","attack")]
requete_41 = filtre[order(-filtre$attack),]
requete_41
dim(requete_41)

requete_42 <- pokemon[pokemon$type %in% c("dragon","ghost","psychic","dark"),c("nom","type")]
requete_42
dim(requete_42)

filtre <- pokemon[pokemon$type == "fire" & pokemon$attack > 100, c("nom","attack", "type")]
requete_43 = filtre[order(-filtre$attack),]
requete_43
dim(requete_43)


filtre <- pokemon[pokemon$speed > 100 & pokemon$speed < 150, c("nom", "speed")]
requete_44 = filtre[order(-filtre$speed),]
requete_44
dim(requete_44)

requete_45 <- pokemon[is.na(pokemon$height_m),]
requete_45
dim(requete_45)

requete_46 <- pokemon[!is.na(pokemon$height_m) & !is.na(pokemon$weight_kg),]
requete_46
dim(requete_46)

requete_47 <- pokemon[!is.na(pokemon$weight_kg) & pokemon$weight_kg > 250 , c("nom", "weight_kg")]
requete_47

## Exercise 5

requete_50 <- aggregate(x = speed ~ generation, data = pokemon , FUN = mean)
requete_50
dim(requete_50)

MoyTypeAttack <- aggregate(x = speed ~ type, data = pokemon , FUN = mean)
filtre <- MoyTypeAttack[order(-MoyTypeAttack$speed),]
requete_51 = filtre[1:3,]
requete_51

requete_52 <- aggregate(x = pokedex_number ~ type, data = pokemon, FUN = length)
requete_52
dim(requete_52)

requete_53 <- aggregate(x = weight_kg ~ type, data = pokemon, FUN = median)
requete_53
dim(requete_53)

requete_54 <- aggregate(x = pokedex_number ~ type + generation, data = pokemon, FUN = length)
requete_54
dim(requete_54)

requete_55 <- aggregate(x = weight_kg + height_m + attack + defense + speed ~ type, data = pokemon , FUN = mean)
requete_55
dim(requete_55)
