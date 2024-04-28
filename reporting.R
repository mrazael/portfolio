# Print the h2, c2, t2, e2, rG, rC, rTw, rE matrices (note: in the summary output you will find them with 95% CI)
h2 <- mxEval(ACTEhypo.h2, CholACTEhypoFit)
c2 <- mxEval(ACTEhypo.c2, CholACTEhypoFit)
t2 <- mxEval(ACTEhypo.tw2, CholACTEhypoFit)
e2 <- mxEval(ACTEhypo.e2, CholACTEhypoFit)

Acor <- mxEval(ACTEhypo.Acor, CholACTEhypoFit)
Phcor <- mxEval(ACTEhypo.Phcor, CholACTEhypoFit)

sA <- mxEval(ACTEhypo.std_a, CholACTEhypoFit)
sC <- mxEval(ACTEhypo.std_c, CholACTEhypoFit)
sT <- mxEval(ACTEhypo.std_tw, CholACTEhypoFit)
sE <- mxEval(ACTEhypo.std_e, CholACTEhypoFit)


ACTE <- data.frame(h2 = character(), c2 = character(), t2 = character(), e2 = character())
corTable <- data.frame(matrix(NA, nrow = 6, ncol = 6))
pos <- c(1, 8, 15, 22, 29, 36)

for(i in 1:nv){
  ACTE[i,1] <- paste0(sprintf("%.2f", h2[i,i]), " [", sprintf("%.3f", ACTEh2[pos[i],2]), " – ", sprintf("%.3f", ACTEh2[pos[i],3]), "]")
  ACTE[i,2] <- paste0(sprintf("%.2f", c2[i,i]), " [", sprintf("%.3f", ACTEc2[pos[i],2]), " – ", sprintf("%.3f", ACTEc2[pos[i],3]), "]")
  ACTE[i,3] <- paste0(sprintf("%.2f", t2[i,i]), " [", sprintf("%.3f", ACTEt2[pos[i],2]), " – ", sprintf("%.3f", ACTEt2[pos[i],3]), "]")
  ACTE[i,4] <- paste0(sprintf("%.2f", e2[i,i]), " [", sprintf("%.3f", ACTEe2[pos[i],2]), " – ", sprintf("%.3f", ACTEe2[pos[i],3]), "]")
}

for(i in 1:nv){
  for(k in i:nv){
    if(i == k){
      corTable[i,k] <- paste0(sprintf("%.2f", h2[i,i]), " [", sprintf("%.2f", ACTEh2[pos[i],2]), " – ", sprintf("%.2f", ACTEh2[pos[i],3]), "]")
    } else{
      corTable[k,i] <- paste0(sprintf("%.2f", Acor[i,k]), " [", sprintf("%.2f", ACTEAcor[((i-1)*6+k),2]), " – ", sprintf("%.2f", ACTEAcor[((i-1)*6+k),3]), "]")
      corTable[i,k] <- paste0(sprintf("%.2f", Phcor[i,k]), " [", sprintf("%.2f", ACTEPhcor[((i-1)*6+k),2]), " – ", sprintf("%.2f", ACTEPhcor[((i-1)*6+k),3]), "]")
    }
  }
}

estTable <- data.frame(matrix(NA, nrow = 84, ncol = 4))

n = 1

for(s in 1:4){
  if(s == 1){
    for(i in 1:nv){
      for(k in i:nv){
        estTable[n, 2:4] <- ACTEstda[(i-1)*6+k,]
        estTable[n, 1] <- sA[k,i]
        n = n+1
      }
    }
  }
  if(s == 2){
    for(i in 1:nv){
      for(k in i:nv){
        estTable[n, 2:4] <- ACTEstdc[(i-1)*6+k,]
        estTable[n, 1] <- sC[k,i]
        n = n+1
      }
    }
  }
  if(s == 3){
    for(i in 1:nv){
      for(k in i:nv){
        estTable[n, 2:4] <- ACTEstdt[(i-1)*6+k,]
        estTable[n, 1] <- sT[k,i]
        n = n+1
      }
    }
  }
  if(s == 4){
    for(i in 1:nv){
      for(k in i:nv){
        estTable[n, 2:4] <- ACTEstde[(i-1)*6+k,]
        estTable[n, 1] <- sE[k,i]
        n = n+1
      }
    }
  }
}

estTable <- estTable %>%
  filter(X1 != 0 & X2 != 0)

write.csv(ACTE, "ACTE.csv")
write.csv(corTable, "corTable.csv")
write.csv(estTable, "estTable.csv")
