library(corrplot)
base_est=estud % > %select(read, write, math, science, socst)
M=cor(base_est)
corrplot(M, tl.col = "red", bg = "White", tl.srt = 35, addCoef.col= "black", type = "full")

