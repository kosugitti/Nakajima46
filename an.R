# 環境設定 --------------------------------------------------------------------
rm(list = ls())
pacman::p_load(summarytools,tidyverse,psych,GPArotation)## 複数も未所持のパッケージも使える
source("nri_data_code.R")　## コードの全てを実行する
dfm <- dfSummary(main)
dfm$Variable <- vimain$ラベル
dfm |> stview()

# 消費者価値 -------------------------------------------------------------------
# 1. 項目を選択して数値化（2値化）
Skachi_item <- main %>%
  select(matches("^SEN_\\d{2}_MA$")) %>%
  mutate(across(everything(), ~ recode(.,
                                       "ある" = 1,
                                       "ない" = 0
  )))

# 2. 相関行列
corSkachi <- cor(Skachi_item, use = "pairwise")

# 3. 固有値（Kaiserの基準、スクリープロット用）
eigen(corSkachi)

# 4. 並列分析（因子数の推奨を確認）
fa.parallel(Skachi_item, fa = "fa", fm = "ml")

# 5. 因子分析（推奨因子数を使って調整）
result1 <- fa(Skachi_item, nfactors = 7, fm = "ml", rotate = "geominQ", scores = "regression")
print(result1, sort = TRUE)

# 6. 因子スコア取得＋欠損除外
factor_scores1 <- as.data.frame(result1$scores) %>% na.omit()
colnames(factor_scores1) <- c("Factor1", "Factor2", "Factor3", "Factor4")

# 7. クラスタ数の妥当性（エルボー法）を確認
wss <- numeric()
for (k in 1:10) {
  k_result_tmp <- kmeans(factor_scores1, centers = k)
  wss[k] <- k_result_tmp$tot.withinss
}
plot(1:10, wss, type = "b", pch = 19,
     xlab = "クラスタ数", ylab = "WSS（クラスタ内平方和）",
     main = "エルボー法による最適クラスタ数の検討")

# 8. クラスタリング実行（上記で最適数を確認後に調整）
k_result1 <- kmeans(factor_scores1, centers = 4)

# 9. クラスタ列を追加
factor_scores1$Cluster <- as.factor(k_result1$cluster)

# 10. 因子スコアのクラスタ可視化（主に1-2軸）
ggplot(factor_scores1, aes(x = Factor1, y = Factor2, color = Cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "クラスタ分析（因子スコアに基づく）",
       x = "因子1",
       y = "因子2")

# 各クラスタの因子スコア平均を確認
aggregate(. ~ Cluster, data = factor_scores1, mean)

# クラスタラベル追加
factor_scores1 <- factor_scores1 %>%
  mutate(GroupLabel = case_when(
    Cluster == 1 ~ "感覚過敏型",
    Cluster == 2 ~ "鈍感・選択型",
    Cluster == 3 ~ "非該当",
    Cluster == 4 ~ "混合型",
    TRUE ~ "未分類"
  ))


# 認知欲求尺度 ------------------------------------------------------------------
ninchi_item <- main%>%
  select(matches("^NC_Scale_\\d{2}_MX$")) %>% 
  mutate(across(everything(), ~recode(.,
                                      "非常にそうである"=7,
                                      "そうである"=6,
                                      "少しそうである"=5,
                                      "どちらでもない"=4,
                                      "あまりそうでない"=3,
                                      "そうでない"=2,
                                      "全くそうでない"=1)))
corninchi <- cor(ninchi_item,use="pairwise")
corninchi
eigen(corninchi)
fa.parallel(ninchi_item)

result2 <- fa(ninchi_item, nfactors = 3, fm = "ml", rotate = "geominQ")
print(result2,sort = T)

factor_scores2 <- as.data.frame(result1$scores) |> na.omit()
colnames(factor_scores2) <- c("Factor1", "Factor2", "Factor3")

k_result2 <- kmeans(factor_scores2, centers = 3)

factor_scores2$Cluster <- as.factor(k_result2$cluster)

ggplot(factor_scores2, aes(x = Factor1, y = Factor2, color = Cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "クラスタ分析（因子スコアに基づく）",
       x = "因子1",
       y = "因子2")


#RECスケール -------------------------------------------------------------------
# 1. 項目を選択して数値化
rec_item <- main %>%
  select(matches("^REC_Scale_\\d{2}_MX$")) %>% 
  mutate(across(everything(), ~recode(.,
                                      "その通り" = 5,
                                      "だいたいその通り" = 4,
                                      "どちらともいえない" = 3,
                                      "やや違う" = 2,
                                      "違う" = 1
  )))

# 2. 相関行列
corREC <- cor(rec_item, use = "pairwise")

# 3. 固有値（Kaiserの基準、スクリープロット用）
eigen(corREC)

# 4. 並列分析（因子数の推奨を確認）
fa.parallel(rec_item, fa = "fa", fm = "ml")

# 5. 因子分析（推奨数に応数に応じて調整）
result3 <- fa(rec_item, nfactors = 4, fm = "ml", rotate = "geominQ", scores = "regression")
print(result3, sort = TRUE)

# 6. 因子スコア取得＋欠損除外
factor_scores3 <- as.data.frame(result3$scores) %>% na.omit()
colnames(factor_scores3) <- c("Factor1", "Factor2", "Factor3", "Factor4")

# 7. クラスタ数の妥当性（エルボー法）を確認
wss <- numeric()
for (k in 1:10) {
  k_result_tmp <- kmeans(factor_scores3, centers = k)
  wss[k] <- k_result_tmp$tot.withinss
}
plot(1:10, wss, type = "b", pch = 19,
     xlab = "クラスタ数", ylab = "WSS（クラスタ内平方和）",
     main = "エルボー法による最適クラスタ数の検討")

# 8. クラスタリング実行（上記で最適数を確認後に調整）
k_result3 <- kmeans(factor_scores3, centers = 4)

# 9. クラスタ列を追加
factor_scores3$Cluster <- as.factor(k_result3$cluster)

# 10. 因子スコアのクラスタ可視化（主に1-2軸）
ggplot(factor_scores3, aes(x = Factor1, y = Factor2, color = Cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "クラスタ分析（因子スコアに基づく）",
       x = "因子1",
       y = "因子2")


# 制御焦点尺度 ------------------------------------------------------------------
# 1. 項目を選択して数値化
seigyo_item <- main %>%
  select(matches("^RF_Scale_\\d{2}_MX$")) %>% 
  mutate(across(everything(), ~recode(.,
                                      "非常にそうである" = 7,
                                      "そうである" = 6,
                                      "少しそうである" = 5,
                                      "どちらでもない" = 4,
                                      "あまりそうでない" = 3,
                                      "そうでない" = 2,
                                      "全くそうでない" = 1
  )))

# 2. 相関行列
corseigyo <- cor(seigyo_item, use = "pairwise")

# 3. 固有値（Kaiserの基準、スクリープロット用）
eigen(corseigyo)

# 4. 並列分析（因子数の推奨を確認）
fa.parallel(seigyo_item, fa = "fa", fm = "ml")

# 5. 因子分析（推奨数に応じて適宜変更）
result4 <- fa(seigyo_item, nfactors = 3, fm = "ml", rotate = "geominQ", scores = "regression")
print(result4, sort = TRUE)

# 6. 因子スコア取得＋欠損除外
factor_scores4 <- as.data.frame(result4$scores) %>% na.omit()
colnames(factor_scores4) <- c("Factor1", "Factor2", "Factor3")

# 7. クラスタ数の妥当性（エルボー法）を確認
wss <- numeric()
for (k in 1:10) {
  k_result_tmp <- kmeans(factor_scores4, centers = k)
  wss[k] <- k_result_tmp$tot.withinss
}
plot(1:10, wss, type = "b", pch = 19,
     xlab = "クラスタ数", ylab = "WSS（クラスタ内平方和）",
     main = "エルボー法による最適クラスタ数の検討")

# 8. クラスタリング実行（上記で最適数を確認後に調整）
k_result4 <- kmeans(factor_scores4, centers = 3)

# 9. クラスタ列を追加
factor_scores4$Cluster <- as.factor(k_result4$cluster)

# 10. 因子スコアのクラスタ可視化（主に1-2軸）
ggplot(factor_scores4, aes(x = Factor1, y = Factor2, color = Cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "クラスタ分析（因子スコアに基づく）",
       x = "因子1",
       y = "因子2")






# 統合 ----------------------------------------------------------------------
## 全ての尺度の統合
all_items <- bind_cols(Skachi_item, seigyo_item,ninchi_item, rec_item)

cor_all <- cor(all_items, use = "pairwise.complete.obs")
eigen(cor_all)

# 並列分析で因子数を検討
fa.parallel(all_items, fa = "fa", fm = "ml")

# 因子分析（例：5因子）
result_all <- fa(all_items, nfactors = 10, fm = "ml", rotate = "geominQ", scores = "regression")
print(result_all, sort = TRUE)

factor_scores_all <- as.data.frame(result_all$scores) %>% na.omit()

# クラスタ数をエルボー法で確認
wss <- numeric()
for (k in 1:10) {
  wss[k] <- kmeans(factor_scores_all, centers = k)$tot.withinss
}
plot(1:10, wss, type = "b", pch = 19,
     xlab = "クラスタ数", ylab = "WSS（クラスタ内平方和）",
     main = "エルボー法：統合因子スコアのクラスタ数")

# 例：4クラスタで実施
k_result_all <- kmeans(factor_scores_all, centers = 4)
factor_scores_all$Group <- as.factor(k_result_all$cluster)

group_labels <- c("あああ型", "いいい型", "ううう型", "えええ型")
factor_scores_all$Group <- group_labels[as.numeric(factor_scores_all$Cluster)]

# 行数が一致していれば直接結合
main <- bind_cols(main, factor_scores_all %>% select(Group))

table(main$Group)
head(main %>% select(Group))

