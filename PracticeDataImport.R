volley = read.table("Data/Volley.txt")
View(volley)

colnames(volley) <- c("PlayerID", "JumpHeight", "Reach", "Points")
View(volley)

plot(volley$JumpHeight,
     volley$Reach,
     main = "Jump Height vs Reach",
     xlab = "Jump Height (cm)",
     ylab = "Reach (cm)", col = "blue")

vollyHeaderTrue = read.table("Data/volley.txt", header = TRUE)
View(vollyHeaderTrue)

ebola = read.csv("Data/ebola.csv")
View(ebola)

ebolaNoHeader = read.csv("Data/ebola.csv", header = FALSE)
View(ebolaNoHeader)

plot(ebola, main = "Ebola")

# install.packages("corrplot")
# install.packages("RColorBrewer")
library(corrplot)
library(RColorBrewer)

# Finding Numerical Columns
numeric_cols = sapply(ebola, is.numeric)
E <- cor(ebola[,numeric_cols])
corrplot(E, type = "upper", addCoef.col = "white", order="hclust",
         col=brewer.pal(n=50, name="RdYlBu"))