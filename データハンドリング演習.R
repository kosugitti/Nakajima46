# 6.2.1 基本的な計算と変数操作 -------------------------------------------------------
## 1
rm(list = ls())

## 2
print("Hello, R World!")

## 3
5 + 3 * 2 - 10 / 2

## 4
(5 + 3) * (2 - 10) / 2

## 5
sqrt(841)

## 6
? sqrt

## 7
2^3

## 8
98^7

## 9
sqrt(-4)

## 10
x <- 42

## 11
x

## 12
hoge <- 1:10

# 6.2.2　データ構造の操作と確認 -------------------------------------------------------
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
hoge2[c(2, 4, 6, 8, 10)]

# 6.2.3　行列操作とデータ型変換 -------------------------------------------------------
## 21
matrix(hoge2, ncol = 2)

## 22
hoge3 <- matrix(hoge2, ncol = 2, byrow = TRUE)

## 23
hoge3

## 24
dim(hoge3)

## 25
hoge3[1, ]

## 26
hoge3[, 2]

## 27
hoge3[2, 2]

## 28
hoge3 <- as.data.frame(hoge3)

## 29
str(hoge3)

## 30
colnames(hoge3) <- c("A", "B")

# 6.2.4　tidyverse入門とファイル操作 ------------------------------------------------
## 31
install.packages("tidyverse")

## 32
library(tidyverse)

## 33
hoge3 <- as_tibble(hoge3)

## 34
str(hoge3)

## 35

## 36
getwd()

## 37

## 38

## 39


# 6.2.5 データ読み込みと基本統計量 -----------------------------------------------------
## 41
dat <- read_csv("BaseballDecade.csv")

## 42
head(dat)

## 43
tail(dat)

## 44
dim(dat)

## 45
names(dat)
colnames(dat)

## 46
summary(dat)

## 47
str(dat)

## 48
class(dat)

## 49
dat.tb <- dat

## 50

# 6.2.6 データフレームの列・変数の操作 ---------------------------------------------------
## 51
dat.tb$Name

## 52
table(dat.tb$Name)

## 53

## 54
dat.tb$Name |> table() |> sort(decreasing = TRUE)

## 55
dat.tb$team |> unique()

## 56
dat.tb$team |> unique() |> length()

## 57
dat.tb$team <- dat.tb$team |> as.factor()
dat.tb$team

## 58
dat.tb$bloodType <- dat.tb$bloodType |> as.factor()
dat.tb$bloodType

## 59
dat.tb$position <- dat.tb$position |> as.factor()
dat.tb$position

## 60
dat.tb |> select(team, bloodType, position) |> summary()

# 6.2.7 数値データの集計と新しい変数の作成 -------------------------------------------------
## 61
dat.tb$height |> mean()

## 62
dat.tb$height |> var()

## 63
dat.tb$height |> sd()

## 64
dat.tb$height |> range()

## 65
dat.tb$salary |> quantile()

## 66
dat.tb$salary |> quantile(probs = c(0, 0.25, 0.33, 0.95, 1))

## 67
dat.tb <- dat.tb |> mutate(bmi = weight / (height/100)^2)

## 68
dat.tb$bmi |> summary()

## 69
dat.tb <- dat.tb |> mutate(bmi_category = ifelse(bmi >= 25, "HighBMI","Standard"))

## 70
dat.tb$bmi_category |> table()

# 6.2.8 データの分類と条件付き処理 -----------------------------------------------------
## 71
dat.tb <- dat.tb |> 
  mutate(position2 = case_when(position == "投手" ~ "投手", TRUE ~ "野手"))

## 72
dat.tb$position2 <- dat.tb$position2 |> as.factor()

## 73
dat.tb$position2 |> table()

## 74
table(dat.tb$position, dat.tb$position2)

## 75
dat.tb <- dat.tb |> 
  mutate(League = case_when(
    team %in% c("Giants", "Carp", "Tigers", "Swallows", "Dragons", "DeNA") ~ "Central",
    TRUE ~ "Pacific"
  ))

## 76
dat.tb$League <- dat.tb$League |> as.factor()

## 77
table(dat.tb$team, dat.tb$League)

## 78
dat.tb <- dat.tb |> 
  mutate(Year_num = Year |>  str_remove("年度") |> as.numeric())

## 79
dat.tb |> select(Year, Year_num)

## 80

# 6.2.9 データのフィルタリングと選択 ----------------------------------------------------
## 81
dat.tb |> filter(position2 == "野手") |> head()

## 82
dat.tb |> filter(position2 == "野手") |> summary()

## 83
dat.tb |> filter(Year_num <= 2015) |> head()

## 84
dat.tb |> filter(Year_num == 2020 & League == "Central") |> head()

## 85
dat.tb |> filter(Year_num == 2020 & League == "Central") |> nrow()

## 86
dat.tb |> select(Name, team, height, weight) |> head()

## 87
dat.tb |> select(Name, team, salary, Year_num) |> filter(Year_num == 2020) |> head()

## 88
dat.tb |> arrange(desc(salary)) |> head(1)
