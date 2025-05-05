
Wagner_Whitin <-function(n_pp,n_dp,fl_cs,fl_cp,fl_ci,matrix_cp) {
  # j rappresenta il periodo
  for (j in 1:n_pp){
    #
    for (k in j:n_pp){
      # costo iniziale periodo 
      fl_cip <- fl_ci[j]
      # costo produzione periodo
      fl_cpp <- sum(n_dp[j:k])*fl_cp[j]
      # azzero il costo stoccaggio periodo per poter calcolare i prossimi
      fl_csp <- 0
      if (j < k){ 
        for (x in j:(k-1)){
          # memoria domanda periodo
          mn_dp <- cumsum(n_dp[j:k])
          # controllo quanti elementi sono presenti nel vettore in modo da allineare le variabili k e x
        if (length(mn_dp)<k){
          # aggiungo  x zeri all'inizio del vettore in base alla differenza di lunghezza del vettore mn_dp e il contatore k
          n_dpp <- c(rep(0,times=k-length(mn_dp)),mn_dp)
        } else{
          n_dpp <- c(mn_dp)
        }
        # calcolo il numero della domanda per quel periodo 
        n_domanda <- n_dpp[k]-n_dpp[x]
        # calcolo il costo del stoccaggio del periodo j
        fl_csp <- fl_csp + (n_domanda * fl_cs[(x)])
        #
        }  
      #
      }
      else {
      # se produco solo per quel periodo non ho nessun costo di stoccaggio
      fl_csp <- 0
    }
    # costo totale
    fl_ctp <- fl_cip + fl_cpp + fl_csp
    cat(sprintf("Il costo totale del periodo %d-%d è: %.2f\n", j, k, fl_ctp))
    # registro il costo nella matrice
    matrix_cp [j,k]<-fl_ctp
    #
    }
  #
  }
  # stampo la matrice ottenuta
  print(matrix_cp)
}
#
#
#
#
Calcolo_dei_costi_Minimi<-function(matrix_cp,n_pp){

  mem_num<-numeric(n_pp)
  fl_min_costi<-numeric(n_pp)
  
  for(k in 1:n_pp){
    #
    # Resetto tutti i valori all'interno del vettore
    mem_costi<-rep(Inf,times=n_pp)
    # Resetto la variabile su cui vado a memorizzare il valore preso dalla matrice
    mem_num<-0
    for(j in 1:n_pp){
    #
    if(j==k) {
        if(k>1){
            mem_num<- matrix_cp[j,k]+fl_min_costi[(j-1)]
        }else{
            mem_num<- matrix_cp[j,k]
        }
    }else if (j==1 && k!= j){
        mem_num<- matrix_cp[j,k]
    }else{
        mem_num<- matrix_cp[j,k]+fl_min_costi[(j-1)]
    }
    mem_costi[j]<-mem_num
    #
    }
    cat(sprintf("\n"))
    print(mem_costi)
    fl_min_costi[k]<-min(mem_costi)
    if (k==n_pp){
    cat(sprintf("Per il periodo %d e'consigliato produrre nel periodo: %d\n",k,which.min(mem_costi)))
      cat(sprintf("\nLa funzione Obiettivo e': %.2f\n",min(mem_costi)))
    }else{
      cat(sprintf("Per il periodo %d e'consigliato produrre nel periodo: %d\n",k,which.min(mem_costi)))
    }
    #
  }
  cat(sprintf("\nI costi minimi per i vari periodi sono:\n"))
  print(fl_min_costi)
#
}
#

# Variabili 
n_pp <- as.numeric(readline("Inserisci il numero di periodi produttivi: "))
n_dp <- numeric(n_pp)
fl_cs <- numeric(n_pp)
fl_cp <- numeric(n_pp)
fl_ci <- numeric(n_pp)

#Input utente
for (i in 1:n_pp) {
  # Inserimento domanda produzione
  n_dp[i] <- as.numeric(readline(prompt = paste("Inserisci la domanda per il periodo", i, ":")))
  
  # Inserimento costo stoccaggio
  fl_cs[i] <- round(as.numeric(gsub(",", ".", readline(prompt = paste("Inserisci il costo di stoccaggio per il periodo", i, ":")))), 2)
  
  # Inserimento costo produzione
  fl_cp[i] <- round(as.numeric(gsub(",", ".", readline(prompt = paste("Inserisci il costo di produzione per il periodo", i, ":")))), 2)
  
  # Inserimento costo iniziale
  fl_ci[i] <- round(as.numeric(gsub(",", ".", readline(prompt = paste("Inserisci il costo iniziale per il periodo", i, ":")))), 2)
}

# Creazione dei nomi delle righe usando paste() per formattare
i <- seq(1, n_pp)
row_names <- paste("P", i, sep = "")

# Visualizzazione dei dati raccolti da utente
df<-data.frame(Domanda =n_dp, 
               Costo_Stoccaggio= fl_cs,
               Costo_Produzione = fl_cp,
               Costo_Iniziale = fl_ci, 
               row.names = row_names)

print(df)

# Matrice dei costi dei vari periodi per registrare i vari costi
matrix_cp <- matrix(Inf,nrow=n_pp,ncol=n_pp)

# chiamo la funzione per applicare il teorema di Wagner Whithin per avere in uscita la matrice
matrix_cp <-Wagner_Whitin(n_pp,n_dp,fl_cs,fl_cp,fl_ci,matrix_cp)

# chiamo la funzione per il calcolo dei periodi minimi per la produzione
fl_min_costi<-Calcolo_dei_costi_Minimi(matrix_cp,n_pp)




