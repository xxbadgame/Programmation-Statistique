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

netAnnuelToImpot <- function(salaireNetAnnuel){
  if (salaireNetAnnuel <= 10777){
    return("Vous n'etes pas imposable")
    
  }else if(10778<salaireNetAnnuel & salaireNetAnnuel<27478){
    return(salaireNetAnnuel*(1-(11/100)))
    
  }else if(27479<salaireNetAnnuel & salaireNetAnnuel<78570){
    return(salaireNetAnnuel*(1-(30/100)))
  
  }else if(78571<salaireNetAnnuel & salaireNetAnnuel<168994){
    return(salaireNetAnnuel*(1-(41/100)))
    
  }else{
    return(salaireNetAnnuel*(1-(45/100)))
  }
}

netAnnuelToImpot(20000)



