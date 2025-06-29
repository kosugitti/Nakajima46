# 環境設定 --------------------------------------------------------------------
rm(list = ls())
pacman::p_load(summarytools,tidyverse,psych,GPArotation,forcats)## 複数も未所持のパッケージも使える
source("nri_data_code.R")　## コードの全てを実行する
dfm <- dfSummary(main)
dfm$Variable <- vimain$ラベル
dfm |> stview()

# RECスケール -----------------------------------------------------------------

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
### ML1:2,4,6,7,11　流行重視
### ML2:3,10 慎重性
### ML3:1,5,12 コスパ重視
### ML4:8,9　実用性重視

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
       x = "流行に影響",
       y = "慎重度")

ggplot(factor_scores3, aes(x = Factor3, y = Factor4, color = Cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "クラスタ分析（因子スコアに基づく）",
       x = "コスパ重視",
       y = "実用性を重視")

factor_scores3 %>%
  group_by(Cluster) %>%
  summarise(
    across(starts_with("Factor"), mean, na.rm = TRUE)
  )


factor_scores3 %>%
  group_by(Cluster) %>%
  summarise(across(starts_with("Factor"), mean)) %>%
  pivot_longer(cols = starts_with("Factor"), names_to = "因子", values_to = "スコア") %>%
  ggplot(aes(x = 因子, y = スコア, fill = Cluster)) +
  geom_col(position = "dodge") +
  labs(title = "クラスタごとの因子スコアの平均", y = "因子スコア") +
  theme_minimal()

factor_scores3 <- factor_scores3 %>%
  mutate(ClusterLabel = case_when(
    Cluster == 1 ~ "無関心型",
    Cluster == 2 ~ "戦略的バランス型",
    Cluster == 3 ~ "実利堅実型",
    Cluster == 4 ~ "ミーハー型"
  ))


##クラスター１：無関心(390人)
##クラスター２：買い物大好き(427人)
##クラスター３：堅実(832人)
##クラスター４：ミーハー(837人)

factor_scores3 %>%
  count(Cluster)


# 年齢との関連 ------------------------------------------------------------------

mean(main$AGE)
# main から該当する行だけ抽出
df_with_age <- factor_scores3 %>%
  bind_cols(age = main$AGE[!is.na(result3$scores[,1])])  # 年齢列の位置に注意

# クラスタごとの平均・標準偏差を見る
df_with_age %>%
  group_by(Cluster) %>%
  summarise(
    平均年齢 = mean(age, na.rm = TRUE),
    SD = sd(age, na.rm = TRUE),
    N = n()
  )

# まず平均とSDをまとめる
age_summary <- df_with_age %>%
  group_by(Cluster) %>%
  summarise(
    平均年齢 = mean(age, na.rm = TRUE),
    SD = sd(age, na.rm = TRUE)
  )

# ggplotで可視化


ggplot(age_summary, aes(x = Cluster, y = 平均年齢, fill = Cluster)) +
  geom_col(width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = 平均年齢 - SD, ymax = 平均年齢 + SD), width = 0.2) +
  theme_minimal() +
  labs(
    title = "クラスタ別 平均年齢と標準偏差",
    y = "平均年齢",
    x = "クラスタ"
  )

# 性別との関連 ------------------------------------------------------------------
# 因子スコアに性別列を追加
df_with_sex <- factor_scores3 %>%
  bind_cols(sex = main$SEX_CD[!is.na(result3$scores[, 1])])

# クロス集計
table(df_with_sex$Cluster, df_with_sex$sex)

df_with_sex %>%
  count(Cluster, sex) %>%
  group_by(Cluster) %>%
  mutate(割合 = n / sum(n) * 100)

df_with_sex %>%
  count(Cluster, sex) %>%
  ggplot(aes(x = Cluster, y = n, fill = sex)) +
  geom_col(position = "dodge") +
  labs(title = "クラスタごとの性別分布", x = "クラスタ", y = "人数")



# 趣味 ------------------------------------------------------------------
hobby_items <- main %>%
  select(matches("^HOB_\\d{2}_MA$")) %>%
  mutate(across(everything(), ~recode(., "ある" = 1, "ない" = 0)))

hobby_with_cluster <- factor_scores3 %>%
  select(Cluster) %>%
  bind_cols(hobby_items[!is.na(result3$scores[,1]), ])  # スコアのある人と揃える

hobby_summary <- hobby_with_cluster %>%
  group_by(Cluster) %>%
  summarise(across(everything(), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-Cluster, names_to = "趣味", values_to = "割合") %>%
  mutate(割合 = round(割合 * 100, 1))  # %

# 名前マッピング（必要に応じて続けて追加）
name_map <- c(
  HOB_01_MA = "スポーツ・フィットネス",
  HOB_02_MA = "ゴルフ",
  HOB_03_MA = "スキー・スノボ",
  HOB_04_MA = "釣り",
  HOB_05_MA = "アウトドア・キャンプ",
  HOB_06_MA = "スポーツ観戦",
  HOB_07_MA = "音楽鑑賞",
  HOB_08_MA = "映画・演劇・美術鑑賞",
  HOB_09_MA = "ビデオ・DVD鑑賞",
  HOB_10_MA = "写真・ビデオ撮影",
  HOB_11_MA = "楽器演奏・合唱",
  HOB_12_MA = "書道・茶道・華道・絵画・俳句",
  HOB_13_MA = "編み物・料理",
  HOB_14_MA = "日曜大工・機械・模型いじり",
  HOB_15_MA = "園芸・庭いじり",
  HOB_16_MA = "読書",
  HOB_17_MA = "パソコン",
  HOB_18_MA = "テレビゲーム",
  HOB_19_MA = "マッサージ・エステ",
  HOB_20_MA = "カラオケ",
  HOB_21_MA = "外食・グルメ・食べ歩き",
  HOB_22_MA = "囲碁・将棋・麻雀",
  HOB_23_MA = "競馬",
  HOB_24_MA = "パチンコ",
  HOB_25_MA = "宝くじ",
  HOB_26_MA = "国内旅行",
  HOB_27_MA = "海外旅行",
  HOB_28_MA = "ドライブ",
  HOB_29_MA = "遊園地・テーマパーク",
  HOB_30_MA = "ボランティア活動",
  HOB_31_MA = "その他",
  HOB_32_MA = "特にない"
)

hobby_summary_named <- hobby_summary %>%
  mutate(趣味 = name_map[趣味]) %>%
  filter(!is.na(趣味))  # name_mapに登録されていない列を除外


ggplot(hobby_summary_named, aes(x = as.factor(Cluster), y = fct_reorder(趣味, -割合), fill = 割合)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "クラスタ別 趣味（ある）割合", x = "クラスタ", y = "趣味（項目）") +
  theme_minimal(base_size = 13) +
  theme(axis.text.y = element_text(size = 9))



# 製品ごとの購入頻度と意向 ------------------------------------------------------------


# 購入頻度（共通の文字列パターンで処理）
data <- data %>%
  mutate(across(starts_with("PS_CAT_"), ~ case_when(
    . == "ほとんど毎日" ~ 5,
    . == "週に２～３回程度" ~ 4,
    . == "週に１回程度" ~ 3,
    . == "月に１～２回程度" ~ 2,
    . == "ここ１ヶ月では飲んでいない" ~ 1,
    TRUE ~ NA_real_  # 上記に該当しない場合はNAにする
  )))

# 購入意向も同様に
data <- data %>%
  mutate(across(starts_with("PL_CAT"), ~ case_when(
    . == "ぜひ飲みたい" ~ 4,
    . == "飲みたい" ~ 3,
    . == "どちらともいえない" ~ 2,
    . == "飲みたくない" ~ 1,
    TRUE ~ NA_real_
  )))


