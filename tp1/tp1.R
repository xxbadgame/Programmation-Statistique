# 2.1

BrutToNet1 <- function(SalaireBrut){
  if (is.numeric(SalaireBrut)){
    calcul <- SalaireBrut*(1-0.22) # Salaire brut déduis de 23% pour le salaire net
    print(calcul)
  }else{
    print("ERROR : type not expected")
  }
}


BrutToNet2 <- function(SalaireBrut, StatutContrat){
  if (is.numeric(SalaireBrut)){
    
    SalaireMoinsSoucre <- SalaireBrut*(1-0.075) # Salaire brut déduis de 23% pour le salaire net
    
    if (StatutContrat == "cadre"){
      
      SalaireCadre = SalaireMoinsSoucre*(1-0.25)
      print(SalaireCadre)
      
    }else if(StatutContrat == "non-cadres"){
      
      SalaireNonCadre = SalaireMoinsSoucre*(1-0.22)
      print(SalaireNonCadre)
    }
    
  }else{
    
    print("ERROR : type not expected")
    
  }
}


BrutToNet3 <- function(SalaireBrut, StatutContrat, Source = 7.5, tempsTravaille = 100){
  
  if (0<tempsTravaille & tempsTravaille<=100 & 0<Source & Source<=100){
    
      if (is.numeric(SalaireBrut)){
        
        SalaireMoinsSourceTT <- SalaireBrut*(1-Source/100)*(tempsTravaille/100) # Salaire net avant impot
        
        if (StatutContrat == "cadre"){
          
          Salaire = SalaireMoinsSourceTT*(1-0.25) # Net après impot cadre
          
        }else if(StatutContrat == "non-cadres"){
          
          Salaire = SalaireMoinsSoucreTT*(1-0.22) # Net après impot non-cadre
        }
        
      }else{
        print("ERROR : type not expected")
      }
    
    }else{
        print("ERROR : rate and time must be in range(0,100)")
    }
  return(c(SalaireBrut,Salaire))
}

BrutToNet3(2000,"cadre",10,90)


# 2.2

netAnnuelToImpot <- function(SNA){
  impot = 0
  
  tranche1 = 10777
  tranche2 = 27478-10778
  tranche3 = 78570-27479
  tranche4 = 168994-78571
  tranche5 = 168994
  
  # Tranche 1 
  if (SNA <= tranche1){
    return("Vous n'etes pas imposable")
  }
  
  SNA = SNA - tranche1
  
  # Tranche 2
  if (SNA >= tranche2){
    impot = impot + tranche2*(11/100)
  }else{
    impot = impot + (SNA - tranche1)*(11/100)
    return(impot)
  }
  
  # Tranche 3
  if (SNA >= tranche3){
     impot = impot + tranche3*(30/100)
   }else{
     impot = impot + (SNA - tranche2)*(30/100)
     return(impot)
   }
   
  # Tranche 4
  if (SNA >= tranche4){
     impot = impot + tranche4*(41/100)
   }else{
     impot = impot + (SNA - tranche3)*(41/100)
     return(impot)
   }
   
  # Tranche 5
  if (SNA >= tranche5){
    impot = impot + (SNA - tranche4)*(45/100)
    return(impot)
  }
  
}

# Ex 2.3

JustePrix <- function(){
  ModeDif <- as.integer(readline(prompt = "Difficulté : Easy(1)/Medium(2)/Hard(3) : "))
  NbTentatives <- 0
  
  Prix <- sample(x = 1:100,size = 1)
  
  print(Prix)
  
  EstimPrix <- 0
  
  if (ModeDif == 1){
    
    while(EstimPrix != Prix){
      EstimPrix <- as.integer(readline(prompt = "Saisissez un prix : "))
      NbTentatives = NbTentatives - 1
      if (EstimPrix > Prix){
        cat("Le prix est plus bas")
      }else if (EstimPrix < Prix){
        cat("Le prix est plus haut")
      }else{
        return(cat("bravo, vous avez trouvez le prix !", Prix, "euros"))
      }
    }
    
  }else if(ModeDif == 2){
    NbTentatives = 20
  }else if(ModeDif == 3){
    NbTentatives = 10
  }
  
  
  while(EstimPrix != Prix & NbTentatives > 0){
    EstimPrix <- as.integer(readline(prompt = "Saisissez un prix : "))
    NbTentatives = NbTentatives - 1
    
    if (EstimPrix > Prix){
      cat("Le prix est plus bas, plus que",NbTentatives, "Tentatives")
    }else if (EstimPrix < Prix){
      cat("Le prix est plus haut, plus que",NbTentatives, "Tentatives")
    }else{
      return(cat("bravo, vous avez trouvez le prix !", Prix, "euros"))
    }
  }
  return(cat("\nDommage, vous n'avez plus d'essaie possible..."))
}

JustePrix()

# Ex 2.4

PFC <- function(nb_partie){
  cat("Vous avez choisi de jouer", nb_partie,"parties")
  
  while (nb_partie > 0) {
    CoupUser <- readline(prompt = "Pierre, Feuille ou Ciseaux ? ")
    ai_choice <- sample(x = c("Pierre","Feuille","Ciseaux"),size = 1)
    
    if (CoupUser == "Pierre" & ai_choice == "Ciseaux"){
      print(c(ai_choice, "USER à Gagner"))
    }else if (CoupUser == "Pierre" & ai_choice == "Feuille"){
      print(c(ai_choice, "IA à Gagner"))
    }else if (CoupUser == "Feuille" & ai_choice == "Pierre"){
      print(c(ai_choice, "USER à Gagner"))
    }else if (CoupUser == "Feuille" & ai_choice == "Ciseaux"){
      print(c(ai_choice, "IA à Gagner"))
    }else if (CoupUser == "Ciseaux" & ai_choice == "Feuille"){
      print(c(ai_choice, "USER à Gagner"))
    }else if (CoupUser == "Ciseaux" & ai_choice == "Pierre"){
      print(c(ai_choice, "IA à Gagner"))
    }else if (CoupUser == ai_choice){
      print(c(ai_choice, "Match Nul"))
    }else{
      return("Error")
    }
    
    nb_partie = nb_partie - 1
    cat("il reste ", nb_partie, "parties")
    
  }
  return(cat("\nAu revoir"))
}

PFC(3)








