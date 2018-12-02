# Baca File Dataset
test <- read.csv("Data/DataTest_Tugas3_AI.csv", sep = ",")
train <- read.csv("Data/DataTrain_Permute.csv")

# Perhitungan jarak menggunakan Euclidean
distance <- function(p, q){
  # Euclidean Distance
  ((p$X1-q$X1)^2 + (p$X2-q$X2)^2 + (p$X3-q$X3)^2 +
     (p$X4-q$X4)^2 + (p$X5-q$X5)^2) -> point
  
  return(sqrt(point))   
}

# Hitung akurasi untuk satu nilai K pada parameter 'k'
accuracy.calculate <- function(test, train, k){
  # Inisialisasi vector untuk menyimpan prediksi kelas
  rowclass <- c()
  for(j in 1:nrow(test)){
    # Hitung jarak antara 1 baris datatest dengan seluruh dataset
    # Gabungkan label asli dari datatest dengan jarak menjadi satu dataframe 
    rowvalue <- data.frame("dist" = distance(test[j,], train), "Y" = train$Y)
    
    # Urutkan dataframe berdasarkan jarak secara ASCENDING
    rowvalue <- rowvalue[order(rowvalue$dist),]
    
    # Ambil hasil urut dari 1..k dan hitung frekuensi kemunculan label yang ada
    class.predict <- as.data.frame(table(rowvalue[1:k,]$Y))
    # Urutkan dataframe label berdasarkan frekuensi secara DESCENDING 
    class.predict <- class.predict[order(class.predict$Freq, decreasing = TRUE),]

    # Append ke vector label dengan frekuensi maksimal
    rowclass <- c(rowclass, as.numeric(as.character(class.predict$Var1[1])))
  }

  # Hitung akurasi antara vector prediksi label dengan label asli
  acc <- rowclass == test$Y
  return((length(acc[acc == TRUE]) / length(acc)) * 100)
}


## MAIN DRIVE
# Pencarian K terbaik dari K = 1..23
# Jumlah lipatan(FOLD) yang digunakan adalah 8 lipatan
K <-100
FOLD <- 8
SPLIT <- nrow(train) / FOLD
result <- c()

for(i in 1:FOLD){
  cat('\nFOLD', i)
  
  # Ambil data test dan train baru untuk validasi dengan memilah index
  test.validation <- train[c((1 + SPLIT*(i-1)) : (SPLIT*i)),]
  train.validation <- train[-c((1 + SPLIT*(i-1)) : (SPLIT*i)),]

  # Inisialisasi vector yang akan diisi dengan akurasi tiap K
  accuracy.per.test <- c()
  for(k in 1:K){
    # Hitung akurasi tiap K
    accuracy.per.test <-
      c(accuracy.per.test, accuracy.calculate(test.validation, train.validation, k))
  }

  result <- rbind(result, accuracy.per.test)
}

colnames(result) <- 1:K

# Untuk seluruh K, hitung rata-rata akurasi dari seluruh lipatan
result <- colSums(result) / nrow(result)
print(result)

# Ambil K dengan akurasi terbaik
optimum.K <- as.numeric(names(result[result == max(result)]))
optimum.K