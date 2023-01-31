# Brut vers Net avant impôt pour les non-cadres uniquement

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
        
        SalaireMoinsSoucreTT <- SalaireBrut*(1-Source/100)*(tempsTravaille/100) # Salaire net avant impot
        
        if (StatutContrat == "cadre"){
          
          SalaireCadre = SalaireMoinsSoucre*(1-0.25)
          print(SalaireCadre) # Net après impot cadre
          
        }else if(StatutContrat == "non-cadres"){
          
          SalaireNonCadre = SalaireMoinsSoucre*(1-0.22)
          print(SalaireNonCadre) # Net après impot non-cadre
        }
        
      }else{
        print("ERROR : type not expected")
      }
    
    }else{
        print("ERROR : rate and time must be in range(0,100)")
    }
}

BrutToNet3(2000,"cadre",10,90)

