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
dat.tb$Name |>
  table() |>
  sort(decreasing = TRUE)

## 55
dat.tb$team |> unique()

## 56
dat.tb$team |>
  unique() |>
  length()

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
dat.tb |>
  select(team, bloodType, position) |>
  summary()

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
dat.tb <- dat.tb |> mutate(bmi = weight / (height / 100)^2)

## 68
dat.tb$bmi |> summary()

## 69
dat.tb <- dat.tb |> mutate(bmi_category = ifelse(bmi >= 25, "HighBMI", "Standard"))

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
  mutate(Year_num = Year |> str_remove("年度") |> as.numeric())

## 79
dat.tb |> select(Year, Year_num)

## 80

# 6.2.9 データのフィルタリングと選択 ----------------------------------------------------
## 81
dat.tb |>
  filter(position2 == "野手") |>
  head()

## 82
dat.tb |>
  filter(position2 == "野手") |>
  summary()

## 83
dat.tb |>
  filter(Year_num <= 2015) |>
  head()

## 84
dat.tb |>
  filter(Year_num == 2020 & League == "Central") |>
  head()

## 85
dat.tb |>
  filter(Year_num == 2020 & League == "Central") |>
  nrow()

## 86
dat.tb |>
  select(Name, team, height, weight) |>
  head()

## 87
dat.tb |>
  select(Name, team, salary, Year_num) |>
  filter(Year_num == 2020) |>
  head()

## 88
dat.tb |>
  arrange(desc(salary)) |>
  head(1)

## 89
dat.tb |>
  filter(Year_num == 2020 & League == "Central") |>
  arrange(desc(salary) |> head(1))

## 90
dat.tb |>
  filter(team == "Giants") |>
  summarise(avg_height = mean(height), avg_weight = mean(weight))


# 6.2.10 グループ集計と要約統計量 ------------------------------------------------------------
## 91
dat.tb |>
  group_by(team) |>
  summarise(mean_salary = mean(salary)) |>
  arrange(desc(mean_salary))

## 92
dat.tb |>
  group_by(Year_num, team) |>
  summarise(mean_salary = mean(salary)) |>
  head(10)

## 93
dat.tb |>
  group_by(Year_num, team) |>
  summarise(
    mean_salary = mean(salary),
    max_salary = max(salary),
    min_salary = min(salary)
  ) |>
  head(10)

## 94
dat.tb |>
  group_by(bloodType) |>
  summarise(mean_bmi = mean(bmi)) |>
  arrange(desc(mean_bmi))

## 95
dat.tb |>
  group_by(League) |>
  summarise(
    mean_salary = mean(salary),
    median_salary = median(salary)
  )

## 96
dat.tb |>
  group_by(position) |>
  summarise(
    avg_height = mean(height),
    avg_weight = mean(weight)
  ) |>
  arrange(desc(avg_height))

## 97 年度ごとのHR総数
dat.tb |> group_by(Year_num) |> 
  summarise(total_HR =sum(HR, na.rm = TRUE))

## 98 コードの整形

## 99 変数を限定したデータフレームの作成
dat.tb |> select(Year_num, Name, height, weight) |> head()

## 100 
dat.tb |> select(Year_num,Name, height, weight) |> 
  filter(Year_num == 2020) |> head()


# 6.2.11 データ変形と再構成 --------------------------------------------------------
## 101 
dat.tb2 <- dat.tb |> select(Year_num, Name, height,weight) |> 
  filter(Year_num == 2020) |> select(-Year_num)

## 102
head(dat.tb2)

## 103
model <- lm(height ~ weight, data = dat.tb2)

## 104
summary(model)

## 105
dat.tb2 |> pivot_longer(-Name, names_to = "variable",values_to = "value") |> head()

## 106
dat.tb2_long <- dat.tb2 |> pivot_longer(-Name, names_to = "variable",values_to = "value") 

## 107
str(dat.tb2_long)

## 108
dat.tb2_long |> group_by(variable) |> summarise(mean_value = mean(value))

## 109
dat.tb2_long |> pivot_wider(names_from = variable, values_from = value) |> head()

## 110
bat_stats <- dat.tb |> filter(position2 == "野手") |> 
  select(Year_num, Name, AtBats, Hit, HR)
head(bat_stats)

# 6.2.12 データの加工と計算 --------------------------------------------------------
## 111
bat_stats <- bat_stats |> mutate(avg = Hit/AtBats)

## 112
head(bat_stats)

## 113
bat_stats |> arrange(desc(avg)) |> head(10)

## 114
bat_stats |> group_by(Year_num) |> summarise(avg_batting = mean(avg, na.rm =TRUE))

## 115
bat_stats_long <-bat_stats |> 
  pivot_longer(c(AtBats, Hit, HR, avg), names_to = "stat", values_to = "value")

## 116
head(bat_stats_long)

## 117
bat_stats_long |> group_by(Name, stat) |> 
  summarise(mean_value = mean(value, na.rm = TRUE)) |> head()

## 118
player_avg <- bat_stats_long |> group_by(Name, stat) |> 
  summarise(means_value = mean(value, na.rm = TRUE)) |> 
  pivot_wider(names_from = stat, values_from = means_value)

## 119
player_avg |> arrange(desc(avg)) |> head(1)


# 6.2.13 データ可視化の基礎 --------------------------------------------------------
## 121
ggplot()

## 122
ggplot(dat.tb, aes(x = height)) + geom_histogram()

## 123
ggplot(dat.tb, aes(x = height)) + geom_histogram(binwidth = 2)

## 124
ggplot(dat.tb, aes(x = height)) + geom_histogram(fill = "blue", color = "black")

## 125
ggplot(dat.tb, aes(x = height,y = weight, color = bloodType))+geom_point()

## 126
ggplot(dat.tb, aes(x = height, y = weight, shape = bloodType)) + geom_point()

