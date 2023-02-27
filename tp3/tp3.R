library(gplots)

## Erxercice 1

population <- rnorm(n = 10000000,mean = 171,sd = 9)

p = mean(population)
et = sd(population)

hist(population, main="Distribution d'une loi normale",
     probability = TRUE)


requete_1 <- population[population > 190]
nb190 = length(requete_1)
prob1 = nb190/length(population)

p190 = 1 - pnorm(190,171,9)

requete_2 <- population[population < 144]
nb144 =length(requete_2)
prob2 = nb144/length(population)

p144 = pnorm(144,171,9)

## Erxercice 2

echant = sample(population, 100, replace = T)
p = mean(echant)
ec = sd(echant)

# Oui, elles sont proches de celles de la population

borneD = p+1.96*ec/sqrt(100)
borneG = p-1.96*ec/sqrt(100)

df = data.frame(replicate(1000, sample(population, 100, replace = T)))
moyCol = apply(df,2, mean, na.rm=T)
ecCol = apply(df,2, sd, na.rm=T)

hist(moyCol, main="Distribution d'une loi normale",
     probability = TRUE)

hist(ecCol, main="Distribution d'une loi normale",
     probability = TRUE)

MoyDesMoy = mean(moyCol)
ecDesMoy = sd(moyCol)

length(moyCol[moyCol > 172.8])
p172 = 1 - pnorm(172.8,MoyDesMoy,9)

demiInter = 1.96 * ecDesMoy/sqrt(100)

BorG = MoyDesMoy-demiInter
BorD = MoyDesMoy+demiInter

plotCI(x=MoyDesMoy, y=ecDesMoy)
