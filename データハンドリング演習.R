# 6.2.1 基本的な計算と変数操作
## 1
rm(list=ls())

## 2
print("Hello, R World!")

## 3
5 + 3 * 2 - 10 / 2

## 4
(5 + 3) * (2 - 10) / 2

## 5
sqrt(841)

## 6
?sqrt

## 7
2 ^ 3

## 8
98 ^ 7

## 9
sqrt(-4)

## 10
x <- 42

## 11
x

## 12
hoge <- 1 : 10

# 6.2.2　データ構造の操作と確認
## 13
hoge

## 14
hoge2 <- hoge * 2

## 15
hoge2

## 16
str(hoge2)

## 17

## 18
hoge2[3]

## 19
hoge2[2:5]

## 20
hoge2[c(2,4,6,8,10)]

#6.2.3　行列操作とデータ型変換
## 21
matrix(hoge2, ncol=2)

## 22
hoge3 <- matrix(hoge2, ncol = 2, byrow =TRUE)

## 23
hoge3

## 24
dim(hoge3)

## 25
hoge3[1,]

## 26
hoge3[,2]

## 27
hoge3[2,2]

## 28
hoge3 <- as.data.frame(hoge3)

## 29
str(hoge3)

## 30
colnames(hoge3) <- c("A", "B")

# 6.2.4　tidyverse入門とファイル操作
## 31 -> 済

## 32
library(tidyverse)

## 33
hoge3 <-as_tibble(hoge3)

## 34
hoge3

## 35


## 36
getwd()




# 6.2.5