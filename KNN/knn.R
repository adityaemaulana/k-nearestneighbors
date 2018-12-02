source("KNN/kfoldvalidation.R")
options(max.print = 10000)
test <- read.csv("DataTest_Tugas3_AI.csv", sep = ",")
train <- read.csv("DataTrain_Tugas3_AI.csv", sep = ",")

# K yang digunakan untuk prediksi label dari data test asli didapat dari hasil validasi
optimum.K

rowclass <- c()
for(j in 1:nrow(test)){
  rowvalue <- data.frame("dist" = distance(test[j,], train), "Y" = train$Y)
  rowvalue <- rowvalue[order(rowvalue$dist),]
  
  class.predict <- as.data.frame(table(rowvalue[1:optimum.K,]$Y))
  class.predict <- class.predict[order(class.predict$Freq, decreasing = TRUE),]
  
  rowclass <- c(rowclass, as.numeric(as.character(class.predict$Var1[1])))
}

rowclass

# Export CSV yang berisi hasil prediksi label pada data test
write.table(rowclass, "TebakanTugas3.csv", row.names = FALSE, col.names = FALSE)