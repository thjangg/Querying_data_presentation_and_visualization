library(BCA)
library(car)
library(corrplot)

Eggs<-read.csv("http://jolej.linuxpl.info/Eggs.csv", header=TRUE)
Eggs$Easter <- factor(Eggs$Easter)
Eggs$First.Week <- factor(Eggs$First.Week)
Eggs$Month <- factor(Eggs$Month)
a<-data.frame(Eggs$Easter, Eggs$Beef.Pr,Eggs$Cases,Eggs$Cereal.Pr,Eggs$Chicken.Pr,Eggs$Egg.Pr,Eggs$Pork.Pr)
View(a)
summary(Eggs)
str(Eggs)

## Graph 1
Eggs$Easter <- factor(Eggs$Easter, levels = c("Pre Easter", "Easter", "Post Easter", "Non Easter"))

Eggs$Color <- ifelse(Eggs$Easter == "Easter", "red",
                     ifelse(Eggs$Easter == "Pre Easter", "orange",
                            ifelse(Eggs$Easter == "Post Easter", "green", "blue")))
  
  
plot(Eggs$Week, Eggs$Cases, type = "n", xlab = "Week", ylab = "Volume of cases sold (cases)",
     main = "Volume of eggs sold per week")

for (i in 1:(nrow(Eggs) - 1)) {
  lines(Eggs$Week[i:(i+1)], Eggs$Cases[i:(i+1)], col = Eggs$Color[i], lwd = 2)
}
legend("topleft", legend = c("Easter", "Pre Easter", "Post Easter", "Non Easter"), 
       col = c("red", "orange", "green", "blue"), lwd = 2)

_________
library(lattice)
Eggs$Color <- ifelse(Eggs$Easter == "Easter", "red",
                     ifelse(Eggs$Easter == "Pre Easter", "orange",
                            ifelse(Eggs$Easter == "Post Easter", "green", "blue")))

panel.custom <- function(x, y, ...) {
  for (i in 1:(length(x) - 1)) {
    panel.lines(x[i:(i+1)], y[i:(i+1)], col = Eggs$Color[i], lwd = 2)
  }
}
xyplot( Eggs$Cases ~ Eggs$Week, main = "Volume of eggs sold per week",
        panel = panel.custom, 
        type = "n",
        col = c("red", "orange", "green", "blue"), 
        xlab = "Week", ylab = "Cases",
        auto.key = list(columns = 4, 
                        text = c("Easter", "Pre Easter", "Post Easter", "Non Easter"),
                        points = TRUE, lines = TRUE, 
                        col = c("red", "orange", "green", "blue"),
                        lwd = 2))


###Graph 2
library(corrplot)
data_matrix <- Eggs[, c("Cases", "Egg.Pr", "Beef.Pr", "Pork.Pr", "Chicken.Pr", "Cereal.Pr")]
cor_matrix <- cor(data_matrix, use = "complete.obs")

# Vẽ Heatmap
corrplot(cor_matrix, method = "color", addCoef.col = "darkgrey", type = "upper",
         tl.col = "blue", title = "Correlation Matrix between Protein Prices")
______

library(lattice)

# Tính toán ma trận tương quan
data_matrix <- Eggs[, c("Cases", "Egg.Pr", "Beef.Pr", "Pork.Pr", "Chicken.Pr", "Cereal.Pr")]
cor_matrix <- cor(data_matrix, use = "complete.obs")

# Chuyển đổi ma trận tương quan thành dataframe (dạng dài)
cor_matrix_melt <- as.data.frame(as.table(cor_matrix))

# Đảm bảo rằng tất cả các biến đều xuất hiện trên cả trục X và Y
levels_var <- colnames(cor_matrix)
cor_matrix_melt$Var1 <- factor(cor_matrix_melt$Var1, levels = levels_var)
cor_matrix_melt$Var2 <- factor(cor_matrix_melt$Var2, levels = levels_var)

# Loại bỏ các giá trị thuộc nửa dưới ma trận (Var1 >= Var2)
cor_matrix_melt <- cor_matrix_melt[as.numeric(cor_matrix_melt$Var1) <= as.numeric(cor_matrix_melt$Var2), ]

levelplot(Freq ~ Var1 * Var2, data = cor_matrix_melt | "Correlation Matrix by Lattice between Protein Prices",
          col.regions = colorRampPalette(c("darkred", "white", "darkblue"))(100), # Màu giống corrplot
          xlab = "", ylab ="",
          at = seq(-1, 1, length = 100), # Phạm vi giá trị
          panel = function(...) { # Panel để thêm văn bản
            panel.levelplot(...) # Vẽ heatmap
            panel.text(cor_matrix_melt$Var1, cor_matrix_melt$Var2, 
                       labels = round(cor_matrix_melt$Freq, 2), cex = 0.8) # Thêm giá trị tương quan
          })





###Graph 3
monthly_sales <- aggregate(Cases ~ Month, data = Eggs, sum)

Eggs$Month <- factor(Eggs$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", 
                                            "September", "October", "November", "December"))
barplot(monthly_sales$Cases, 
       names.arg = monthly_sales$Month, 
       col = "coral", 
       main = "Volume of eggs sold per month",
       xlab = "Month", 
       ylab = "Cases sold",
       las = 2)


library(lattice)
monthly_sales <- aggregate(Cases ~ Month, data = Eggs, sum)

Eggs$Month <- factor(Eggs$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", 
                                            "September", "October", "November", "December"))
y <- monthly_sales$Month
x <- monthly_sales$Cases
barchart( x ~ y | "Volume of eggs sold per month",
          ylab = "Numbers of cases",
          scales = list(x = list(rot = 45)),
          col = "coral")

