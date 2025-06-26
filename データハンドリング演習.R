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
ggplot(dat.tb, aes(x = height, y = weight, color = bloodType)) + geom_point()

## 127
ggplot(dat.tb, aes(x = height, y = weight, shape = bloodType)) + geom_point()

## 128
ggplot(dat.tb, aes(x = height, y = weight, color = bloodType)) +
  geom_point(size = 3)

## 129
ggplot(dat.tb, aes(x = height, y = weight, color = bloodType)) +
  geom_point(size = 3) + labs(title = "身長と体重の関係",
                              x = "身長(cm)", y = "体重(kg)" )


# 6.2.14 グループ別の可視化 --------------------------------------------------------
## 131
ggplot(dat.tb, aes(x = height, y = weight)) +
  geom_point() + facet_wrap(~ team)

## 132
ggplot(dat.tb, aes(x = height, y = weight)) +
  geom_point() + geom_smooth(method ="lm")

## 133 平滑化曲線
ggplot(dat.tb, aes(x = height, y = weight)) +
  geom_point() + geom_smooth()

## 134
ggplot(dat.tb, aes(x = height, y = weight, color = League)) +
  geom_point() + geom_smooth(method = "lm")

## 135
ggplot(dat.tb, aes(x = position, y = height)) + geom_boxplot()

## 136
ggplot(dat.tb, aes(x = position2, y = height)) + geom_boxplot() 

## 137
ggplot(dat.tb, aes(x = position2, y = height)) +
  geom_boxplot() + geom_jitter(width = 0.2, alpha = 0.5)

## 138
ggplot(dat.tb, aes(x = bloodType, y = weight))+ geom_violin()

## 139
ggplot(dat.tb, aes(x = bloodType, y = weight)) +
  geom_violin() + geom_boxplot(width = 0.1)

## 140
ggplot(dat.tb, aes(x = bloodType, y = weight, fill = League))+
  geom_violin()


# 6.2.15 グラフのカスタマイズと複合グラフ -------------------------------------------------
## 141
ggplot(dat.tb, aes(x = bloodType, y = weight)) +
  geom_violin() + facet_wrap(~ League)

## 142 上下に配置
ggplot(dat.tb, aes(x = bloodType, y = weight)) +
  geom_violin() + facet_grid(League ~ .)

## 143
ggplot(dat.tb,aes (x = bloodType, y = weight))+
  geom_violin() + facet_grid(League ~ Year_num)

## 144
dat.tb |> group_by(Year_num) |> 
  summarise(avg_weight = mean(weight)) |> 
  ggplot(aes(x = Year_num, y = avg_weight))+ geom_line()

## 145
dat.tb |> group_by(Year_num) |> 
  summarise(avg_weight = mean(weight)) |> 
  ggplot(aes(x = Year_num, y = avg_weight)) + geom_line() + geom_point()

## 146
dat.tb |> group_by(Year_num) |> 
  summarise(avg_weight = mean(weight)) |> 
  ggplot(aes(x = Year_num, y = avg_weight)) +
  geom_line() + geom_point() + theme_minimal()

## 147
dat.tb |> group_by(Year_num) |> 
  summarise(avg_weight = mean(weight)) |> 
  ggplot(aes(x = Year_num, y = avg_weight)) +
  geom_line() + geom_point() + theme_light()

## 148
dat.tb |> group_by(Year_num) |> 
  summarise(total_HR = sum(HR, na.rm = TRUE)) |> 
  ggplot(aes(x = Year_num, y = total_HR)) + geom_col()

## 149
dat.tb |> group_by(Year_num) |> 
  summarise(total_HR = sum(HR, na.rm = TRUE)) |> 
  ggplot(aes(x = factor(Year_num), y = total_HR, fill = factor(Year_num))) +
  geom_col()

## 150 一旦休憩

# 6.2.16 高度なグラフと保存 --------------------------------------------------------
## 151 凡例無くす
dat.tb |> group_by(Year_num) |> 
  summarise(total_HR = sum(HR, na.rm = TRUE)) |> 
  ggplot(aes(x = factor(Year_num), y = total_HR, fill = factor(Year_num))) +
  geom_col() + theme(legend.position = "none")

## 152
dat.tb |> group_by(Year_num) |> 
  summarise(total_HR = sum(HR, na.rm = TRUE)) |> 
  ggplot(aes(x = factor(Year_num), y = total_HR, fill = factor(Year_num))) +
  geom_col() + scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none")

## 153 
RColorBrewer::display.brewer.all()

## 154
dat.tb |> group_by(Year_num) |> 
  summarise(total_HR = sum(HR, na.rm = TRUE)) |> 
  ggplot(aes(x = factor(Year_num), y = total_HR, fill = factor(Year_num)))+
  geom_col() + scale_fill_grey() +
  theme(legend.position = "none")

## 155
dat.tb |> group_by(Year_num) |> 
  summarise(total_HR = sum(HR, na.rm = TRUE)) |> 
  ggplot(aes(x = factor(Year_num), y = total_HR, fill = factor(Year_num)))+
  geom_col() + scale_fill_brewer(palette = "Blues") +
  theme_bw() + theme(legend.position = "none")

## 156
g <- dat.tb |> group_by(Year_num) |> 
  summarise(total_HR = sum(HR, na.rm = TRUE)) |> 
  ggplot(aes(x = factor(Year_num),y = total_HR,  fill = factor(Year_num))) +
  geom_col() + scale_fill_brewer(palette = "Blues") +
  theme_bw() + theme(legend.position = "none")

## 157
print(g)

## 158
ggsave("yearly_hr.png", plot = g, width = 8, height = 6, dpi =300)

#159
getwd()

## 160 確認

# 6.2.17 より高度なデータの可視化 ------------------------------------------------------------
## 161 
dat.tb |> group_by(Year_num) |> 
  summarise(total_HR = sum(HR, na.rm = TRUE)) |> 
  ggplot(aes(x = Year_num, y = total_HR)) +
  geom_line() + geom_point() +
  labs(title ="年間HR総数の推移", x = "年度", y = "総HR数")

## 162
dat.tb |> group_by(Year_num, team) |> 
  summarise(total_HR = sum(HR, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = Year_num, y = total_HR, color = team)) +
  geom_line() + geom_point()

## 163
dat.tb |> group_by(Year_num, team) |> 
  summarise(total_HR = sum(HR, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = Year_num, y = total_HR, color = team)) +
  geom_line(linewidth = 1) + geom_point(size = 3)

## 164
dat.tb |> filter(team %in% c("Giants", "Tigers", "Carp")) |> 
  group_by(Year_num, team) |> 
  summarise(total_HR = sum(HR, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = Year_num, y = total_HR, color = team)) +
  geom_line(linewidth = 1) + geom_point(size = 3)
  
## 165
dat.tb |> filter(Name == "鈴木誠也") |> 
  ggplot(aes(x = Year_num, y = HR)) +
  geom_line() + geom_point() +
  labs(title = "鈴木誠也選手のHR数推移")

## 166
library(GGally)
GGally::ggpairs(dat.tb |> select(height, weight,HR, salary))

## 167
dat.tb |> select(League, height, weight,HR,salary) |> 
  GGally::ggpairs(mapping = aes(color = League))

## 168
library(summarytools)
summarytools::dfSummary(dat.tb) |> summarytools::view()

## 169
library(plotly)
if(!require(plotly)) install.packages("plotly")
plotly::plot_ly(dat.tb, x = ~height, y = ~weight, z =~salary,
                color = ~League, type = "scatter3d", mode = "markers")


# 6.2.18 データのネスト化と応用 ----------------------------------------------------
## 171
nested_data <- dat.tb |> group_by(Year_num) |> nest()

## 172
nested_data

## 173
nested_data$data[1]

## 174
nested_data |> mutate(data_size = map_int(data, nrow))

## 175
nested_data |> mutate(avg_salary = map_dbl(data, ~mean(.$salary)))

## 176
nested_data |> mutate(avg_salary = map(data, ~mean(.$salary)))

## 177
nested_data |> mutate(avg_salary = map_dbl(data, ~ mean(.$salary)))

## 178
nested_data <- nested_data |> 
  mutate(model = map(data, ~lm(height ~ weight, data = .)))

## 179
nested_data |> 
  mutate(coef = map(model, ~ coef(.))) |> 
  mutate(intercept = map_dbl(coef, ~ .[1]),
  slop = map_dbl(coef, ~ .[2]))

## 180
nested_data |> 
  mutate(model_summary = map(model, ~ summary(.))) |> 
  mutate(r_squared = map_dbl(model_summary, ~ .$r.squared))

# 6.2.19 データの結合と操作 --------------------------------------------------------
## 181
top_players <- dat.tb |> 
  group_by(team) |> 
  filter(salary == max(salary)) |> 
  select(Year_num, team, Name, salary)

## 182
team_data <- dat.tb |> 
  group_by(team) |> 
  summarise(avg_salary = mean(salary),
            avg_height = mean(height),
            avg_weight = mean(weight))

## 183
team_data |> inner_join(top_players, by = "team")

## 184
team_data |> left_join(top_players, by = "team")

## 185
team_data |> full_join(top_players, by = "team")

## 186
team_data |> anti_join(top_players, by = "team")

## 187
yearly_team_avg <- dat.tb |> 
  group_by(Year_num, team) |> 
  summarise(avg_salary = mean(salary),groups = "drop")

## 188
df1 <- dat.tb |> filter(Year_num == 2015) |> select(Name, team, salary)
df2 <- dat.tb |> filter(Year_num == 2020) |> select(Name, team, salary)
bind_rows(df1, df2)

## 189
bind_rows("2015" = df1, "2020" = df2, .id = "year") 

## 190 休憩
# 6.2.20 データ分析の最終まとめ ------------------------------------------------------
## 191
if(!require(psych)) install.packages("psych")
psych::describe(dat.tb |> select(height, weight,salary,HR))

## 192
if(!require(corrplot)) install.packages("corrplot")
cor_matrix <- cor(dat.tb |> select(height, weight, salary, HR), use = "complete.obs")
corrplot::corrplot(cor_matrix,method = "circle")

## 193
pca_result <- prcomp(dat.tb |> select(height, weight, salary, HR) |> na.omit(),
                     scale. = TRUE, center = TRUE)
summary(pca_result)

## 194
if(!require(factoextra)) install.packages("factoextra")
factoextra::fviz_pca_biplot(pca_result)

## 195
scaled_data <- scale(dat.tb |> select(height, weight))
kmeans_result <- kmeans(scaled_data, centers = 3)
dat.tb$cluster <-as.factor(kmeans_result$cluster) 

## 196
ggplot(dat.tb, aes(x = height, y = weight, color = cluster)) +
  geom_point() +
  labs(title = "KMeans クラスタリング結果")

## 197
if(!require(DT)) install.packages("DT")
DT::datatable(dat.tb |> select(Name, team, position, height, weight, salary))

## 198
if(!require(gifski)) install.packages("gifski")
library(gifski)
if(!require(gganimate)) install.packages("gganimate")
library(gganimate)
hr_by_year <- dat.tb |> 
  group_by(Year_num, team) |> 
  summarise(total_HR = sum(HR, na.rm = TRUE), .groups = "drop")
anim <- ggplot(hr_by_year, aes(x = team, y = total_HR, fill = team)) +
  geom_col() +
  labs(title = "Year: {frame_time}") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  transition_time(Year_num)
animate(anim, nframes = 100)
