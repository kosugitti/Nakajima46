# 環境設定 --------------------------------------------------------------------
rm(list = ls())
pacman::p_load(summarytools,tidyverse,psych,GPArotation,forcats,tidyr,dplyr,forcats,purrr,tibble)## 複数も未所持のパッケージも使える
source("nri_data_code.R")　## コードの全てを実行する
dfm <- dfSummary(main)
dfm$Variable <- vimain$ラベル
dfm |> stview()

# RECスケール -----------------------------------------------------------------
# 1. 項目を選択して数値化
# 行番号を保持（mainのインデックスをretain）
# 数値化（カテゴリを数値にしてから）
# スクリュープロット（ポリコリック相関を使用）
rec_item <- main %>%
  select(matches("^REC_Scale_\\d{2}_MX$")) %>%
  mutate(across(everything(), ~ recode(.,
                                       "その通り" = 5,
                                       "だいたいその通り" = 4,
                                       "どちらともいえない" = 3,
                                       "やや違う" = 2,
                                       "違う" = 1))) %>%
  as.data.frame() |> 
  mutate(across(everything(), as.numeric))%>%
  mutate(row_id = row_number()) %>%
  drop_na()

result3 <- fa(rec_item_num, nfactors = 4, fm = "ml", 
              rotate = "geominQ", scores = "regression",cor ="poly")



# 2. 相関行列
corREC <- cor(rec_item, use = "pairwise")

# 3. 固有値（Kaiserの基準、スクリープロット用）
eigen(corREC)

# 4. 並列分析（因子数の推奨を確認）
fa.parallel(rec_item_num, 
            fa = "fa", 
            cor = "poly", 
            fm = "ml", 
            n.iter = 100, 
            main = "Scree Plot (Polychoric)")

# 5. 因子分析（推奨数に応数に応じて調整）
result3 <- fa(rec_item_complete, nfactors = 4, fm = "ml", rotate = "geominQ", scores = "regression")
print(result3, sort = TRUE)

### ML1(2,4,6,7,11)：流行重視
### ML2(3,10)：慎重性
### ML3(1,5,12)：コスパ重視
### ML4(8,9)：実用性重視

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
# クラスタリング（4クラスタ）
set.seed(123)  # 任意の固定値
k_result3 <- kmeans(factor_scores3, centers = 4, nstart = 25)


# 元データにクラスタ番号を結合
factor_scores3_raw <- factor_scores3 %>%
  mutate(Cluster_raw = k_result3$cluster)

# クラスタごとの Factor1 平均で並び替え（※他の基準でもOK）
cluster_order <- factor_scores3_raw %>%
  group_by(Cluster_raw) %>%
  summarise(mean_F1 = mean(Factor1)) %>%
  arrange(desc(mean_F1)) %>%
  mutate(Cluster = as.factor(row_number()))  # 新しいクラスタ番号（1,2,3,4）

# 新しいクラスタ番号を factor_scores3 に追加
factor_scores3 <- factor_scores3_raw %>%
  left_join(cluster_order, by = c("Cluster_raw")) %>%
  select(-Cluster_raw, -mean_F1)

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
    Cluster == 2 ~ "実利堅実型",
    Cluster == 3 ~ "戦略的バランス型",
    Cluster == 4 ~ "ミーハー型"
  ))

##クラスター１：買い物大好き
##クラスター２：ミーハー
##クラスター３：堅実
##クラスター４：無関心

##人数 
factor_scores3 %>%
  count(Cluster)



# 年齢との関連 ------------------------------------------------------------------
mean(main$AGE)
# main から該当する行だけ抽出
# row_id を利用して main から該当 AGE を引っ張る
df_with_age <- factor_scores3 %>%
  mutate(row_id = rec_item_complete$row_id) %>%
  mutate(age = main$AGE[row_id])

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

TukeyHSD(aov(age ~ Cluster, data = df_with_age))
kruskal.test(age ~ Cluster, data = df_with_age)


### 全部の要素が高い買い物好きは若い人が多い？


# 性別との関連 ------------------------------------------------------------------
# 因子スコアに性別列を追加
# row_id を使って main の性別データを引く
df_with_sex <- factor_scores3 %>%
  mutate(row_id = rec_item_complete$row_id) %>%
  mutate(sex = main$SEX_CD[row_id])

# クロス集計
# クロス集計
tbl <- table(df_with_sex$Cluster, df_with_sex$sex)

# カイ二乗検定（全体）
chisq.test(tbl)

test <- chisq.test(tbl)
test$residuals  
# 標準化残差を見る
### クラスタ２と４で有意な差？

residuals_df <- as.data.frame(as.table(test$residuals))
colnames(residuals_df) <- c("Cluster", "Sex", "StdResidual")

ggplot(residuals_df, aes(x = Cluster, y = Sex, fill = StdResidual)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "black", high = "white", mid = "gray", midpoint = 0) +
  labs(title = "クラスタ × 性別 の標準化残差（χ²検定）",
       fill = "標準化残差")

## クラスタ２　男性に多い→ミーハー
## クラスタ３　女性に多い→堅実


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
# 1. 趣味データの数値化（"ある"=1, "ない"=0）
hobby_items <- main %>%
  select(matches("^HOB_\\d{2}_MA$")) %>%
  mutate(across(everything(), ~ recode(., "ある" = 1, "ない" = 0)))

# 2. クラスタ情報と結合（欠損スコア除外に注意）
# hobby_items の中で row_id に対応する行を取得
hobby_with_cluster <- factor_scores3 %>%
  mutate(row_id = rec_item_complete$row_id) %>%
  bind_cols(hobby_items[rec_item_complete$row_id, ])

# 3. 名前マッピング（趣味コード → 日本語名）
name_map <- c(
  HOB_01_MA = "スポーツ・フィットネス", HOB_02_MA = "ゴルフ", HOB_03_MA = "スキー・スノボ",
  HOB_04_MA = "釣り", HOB_05_MA = "アウトドア・キャンプ", HOB_06_MA = "スポーツ観戦",
  HOB_07_MA = "音楽鑑賞", HOB_08_MA = "映画・演劇・美術鑑賞", HOB_09_MA = "ビデオ・DVD鑑賞",
  HOB_10_MA = "写真・ビデオ撮影", HOB_11_MA = "楽器演奏・合唱", HOB_12_MA = "書道・茶道・華道・絵画・俳句",
  HOB_13_MA = "編み物・料理", HOB_14_MA = "日曜大工・機械・模型いじり", HOB_15_MA = "園芸・庭いじり",
  HOB_16_MA = "読書", HOB_17_MA = "パソコン", HOB_18_MA = "テレビゲーム", HOB_19_MA = "マッサージ・エステ",
  HOB_20_MA = "カラオケ", HOB_21_MA = "外食・グルメ・食べ歩き", HOB_22_MA = "囲碁・将棋・麻雀",
  HOB_23_MA = "競馬", HOB_24_MA = "パチンコ", HOB_25_MA = "宝くじ", HOB_26_MA = "国内旅行",
  HOB_27_MA = "海外旅行", HOB_28_MA = "ドライブ", HOB_29_MA = "遊園地・テーマパーク",
  HOB_30_MA = "ボランティア活動", HOB_31_MA = "その他", HOB_32_MA = "特にない"
)

# 4. クラスタ別 趣味の「ある」割合集計と上位10抽出
hobby_summary <- hobby_with_cluster %>%
  group_by(Cluster) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-Cluster, names_to = "趣味", values_to = "割合") %>%
  mutate(
    割合 = round(割合 * 100, 1),
    趣味 = name_map[趣味]
  ) %>%
  filter(!is.na(趣味))

top10_ranked <- hobby_summary %>%
  group_by(Cluster) %>%
  arrange(desc(割合)) %>%
  mutate(Rank = row_number()) %>%
  filter(Rank <= 10)

# 5. クラスタ別 上位10趣味の棒グラフ
plot_cluster_hobbies <- ggplot(top10_ranked,
                               aes(x = fct_reorder(趣味, -Rank), y = 割合, fill = as.factor(Cluster))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Cluster, scales = "free_y") +
  coord_flip() +
  labs(title = "クラスタ別 上位10趣味（順位順）", x = "趣味（1位が上）", y = "割合（%）") +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 10))

print(plot_cluster_hobbies)

# 7. データをロング形式に変換（クラスタ別×趣味×0/1の値）
hobby_long <- hobby_with_cluster %>%
  select(Cluster, starts_with("HOB_")) %>%
  pivot_longer(-Cluster, names_to = "趣味コード", values_to = "値") %>%
  mutate(趣味 = name_map[趣味コード]) %>%
  filter(!is.na(趣味))

# 8. カイ二乗検定でクラスタ間の趣味差を調べる
chi_results <- hobby_long %>%
  group_by(趣味) %>%
  summarise(
    p値 = tryCatch(
      chisq.test(table(Cluster, 値))$p.value,
      error = function(e) NA_real_
    )
  ) %>%
  mutate(有意 = ifelse(p値 < 0.05, "＊有意", "")) %>%
  arrange(p値)

print(chi_results %>% filter(p値 < 0.05))

# 9. 標準化残差を計算してクラスタ間の偏りを可視化するための関数
get_residuals <- function(hobby_name) {
  tbl <- table(hobby_long$Cluster[hobby_long$趣味 == hobby_name],
               hobby_long$値[hobby_long$趣味 == hobby_name])
  if (nrow(tbl) >= 2 && ncol(tbl) >= 2) {
    chisq <- suppressWarnings(chisq.test(tbl))
    stdres_df <- as.data.frame.matrix(chisq$stdres) %>%
      rownames_to_column("Cluster") %>%
      pivot_longer(-Cluster, names_to = "回答", values_to = "残差") %>%
      mutate(趣味 = hobby_name)
    return(stdres_df)
  } else {
    return(NULL)
  }
}

# 10. 有意な趣味（p<0.05）を抽出し、標準化残差を取得
top_diff_hobbies <- chi_results %>%
  filter(p値 < 0.05) %>%
  slice_min(p値, n = 32) %>%
  pull(趣味)

residuals_all <- map_dfr(top_diff_hobbies, get_residuals)

# 11. 「ある」に該当する回答だけ抽出（数値が1に変換済みなら "1"）
residuals_filtered <- residuals_all %>%
  filter(回答 == "1")

# 12. クラスタ別の標準化残差を可視化
ggplot(residuals_filtered,
       aes(x = Cluster, y = 残差, fill = Cluster)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ fct_reorder(趣味, -abs(残差)), scales = "free_y") +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "red") +
  labs(title = "クラスタ別 趣味の偏り（標準化残差）", y = "標準化残差", x = "クラスタ") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(size = 10),
        strip.text = element_text(size = 9))

##クラスター１：買い物大好き　多趣味(外)
##クラスター２：ミーハー　ギャンブルとゴルフ
##クラスター３：堅実　室内趣味
##クラスター４：無関心　趣味少ない

# 製品ごとの購入頻度と意向 ------------------------------------------------------------
## ノンアルコール飲料
# --- STEP 1: 行番号を追加（indexとして使う） ---
main <- main %>%
  mutate(row_id = row_number())

# --- STEP 2: PS/PIを0-1に変換（購買頻度・意向） ---
intent_freq_data <- main %>%
  select(row_id, PS_CAT_01, PI_CAT_01) %>%
  mutate(
    PS_CAT_01 = case_when(
      is.na(PS_CAT_01) ~ NA_real_,
      PS_CAT_01 == "ここ１ヶ月では飲んでいない" ~ 0,
      TRUE ~ 1
    ),
    PI_CAT_01 = case_when(
      is.na(PI_CAT_01) ~ NA_real_,
      PI_CAT_01 %in% c("ぜひ飲みたい", "飲みたい") ~ 1,
      TRUE ~ 0
    )
  )

# --- STEP 3: クラスタ情報と結合し、gapフラグ追加 ---
combined <- factor_scores3 %>%
  mutate(row_id = rec_item_complete$row_id) %>%
  left_join(intent_freq_data, by = "row_id") %>%
  mutate(
    一致 = ifelse(PS_CAT_01 == PI_CAT_01, 1, 0),
    gap_flag = ifelse(PS_CAT_01 == 0 & PI_CAT_01 == 1, 1, 0)
  ) %>%
  drop_na(一致)

# --- STEP 4: 一致率と「欲しいけど買っていない」割合の集計 ---
一致率集計 <- combined %>%
  group_by(ClusterLabel) %>%
  summarise(
    一致率 = mean(一致),
    件数 = n()
  )

gap_only <- combined %>%
  group_by(ClusterLabel) %>%
  summarise(
    欲しいけど買っていない数 = sum(gap_flag == 1),
    全体数 = n(),
    割合 = 欲しいけど買っていない数 / 全体数 * 100
  )

print(一致率集計)
print(gap_only)

# --- STEP 5: カイ二乗検定（クラスタ×gap） ---
tbl <- table(combined$ClusterLabel, combined$gap_flag)
chi_test <- chisq.test(tbl)
std_resid <- chi_test$stdres

print(tbl)
print(chi_test)
print(std_resid)

# --- STEP 6: ロジスティック回帰（gap ~ クラスタ） ---
combined$ClusterLabel <- factor(combined$ClusterLabel)

model <- glm(gap_flag ~ ClusterLabel, data = combined, family = binomial)
summary(model)

ggplot(gap_only, aes(x = ClusterLabel, y = 割合, fill = ClusterLabel)) +
  geom_col() +
  geom_text(aes(label = paste0(round(割合,1), "%")), vjust = -0.5) +
  labs(title = "クラスタ別 欲しいけど買っていない割合(ノンアル)",
       x = "クラスタ",
       y = "割合 (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# ヘルシア

intent_freq_data2 <- main %>%
  select(row_id, PS_CAT_02, PI_CAT_02) %>%
  mutate(
    PS_CAT_02 = case_when(
      is.na(PS_CAT_02) ~ NA_real_,
      PS_CAT_02 == "ここ１ヶ月では飲んでいない" ~ 0,
      TRUE ~ 1
    ),
    PI_CAT_02 = case_when(
      is.na(PI_CAT_02) ~ NA_real_,
      PI_CAT_02 %in% c("ぜひ飲みたい", "飲みたい") ~ 1,
      TRUE ~ 0
    )
  )
# --- STEP 3: クラスタ情報と結合し、gapフラグ追加 ---
combined2 <- factor_scores3 %>%
  mutate(row_id = rec_item_complete$row_id) %>%
  left_join(intent_freq_data2, by = "row_id") %>%
  mutate(
    一致 = ifelse(PS_CAT_02 == PI_CAT_02, 1, 0),
    gap_flag = ifelse(PS_CAT_02 == 0 & PI_CAT_02 == 1, 1, 0)
  ) %>%
  drop_na(一致)

# --- STEP 4: 一致率と「欲しいけど買っていない」割合の集計 ---
一致率集計2 <- combined2 %>%
  group_by(ClusterLabel) %>%
  summarise(
    一致率 = mean(一致),
    件数 = n()
  )

gap_only2 <- combined2 %>%
  group_by(ClusterLabel) %>%
  summarise(
    欲しいけど買っていない数 = sum(gap_flag == 1),
    全体数 = n(),
    割合 = 欲しいけど買っていない数 / 全体数 * 100
  )

print(一致率集計2)
print(gap_only2)

# --- STEP 5: カイ二乗検定（クラスタ×gap） ---
tbl2 <- table(combined2$ClusterLabel, combined2$gap_flag)
chi_test2 <- chisq.test(tbl2)
std_resid2 <- chi_test2$stdres

print(tbl2)
print(chi_test2)
print(std_resid2)

# --- STEP 6: ロジスティック回帰（gap ~ クラスタ） ---
combined2$ClusterLabel <- factor(combined2$ClusterLabel)

model2 <- glm(gap_flag ~ ClusterLabel, data = combined2, family = binomial)
summary(model2)

ggplot(gap_only2, aes(x = ClusterLabel, y = 割合, fill = ClusterLabel)) +
  geom_col() +
  geom_text(aes(label = paste0(round(割合,1), "%")), vjust = -0.5) +
  labs(title = "クラスタ別 欲しいけど買っていない割合(ヘルシア)",
       x = "クラスタ",
       y = "割合 (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# レッドブル
intent_freq_data3 <- main %>%
  select(row_id, PS_CAT_03, PI_CAT_03) %>%
  mutate(
    PS_CAT_03 = case_when(
      is.na(PS_CAT_03) ~ NA_real_,
      PS_CAT_03 == "ここ１ヶ月では飲んでいない" ~ 0,
      TRUE ~ 1
    ),
    PI_CAT_03 = case_when(
      is.na(PI_CAT_03) ~ NA_real_,
      PI_CAT_03 %in% c("ぜひ飲みたい", "飲みたい") ~ 1,
      TRUE ~ 0
    )
  )
# --- STEP 3: クラスタ情報と結合し、gapフラグ追加 ---
combined3 <- factor_scores3 %>%
  mutate(row_id = rec_item_complete$row_id) %>%
  left_join(intent_freq_data3, by = "row_id") %>%
  mutate(
    一致 = ifelse(PS_CAT_03 == PI_CAT_03, 1, 0),
    gap_flag = ifelse(PS_CAT_03 == 0 & PI_CAT_03 == 1, 1, 0)
  ) %>%
  drop_na(一致)

# --- STEP 4: 一致率と「欲しいけど買っていない」割合の集計 ---
一致率集計3 <- combined3 %>%
  group_by(ClusterLabel) %>%
  summarise(
    一致率 = mean(一致),
    件数 = n()
  )

gap_only3 <- combined3 %>%
  group_by(ClusterLabel) %>%
  summarise(
    欲しいけど買っていない数 = sum(gap_flag == 1),
    全体数 = n(),
    割合 = 欲しいけど買っていない数 / 全体数 * 100
  )

print(一致率集計3)
print(gap_only3)

# --- STEP 5: カイ二乗検定（クラスタ×gap） ---
tbl3 <- table(combined3$ClusterLabel, combined3$gap_flag)
chi_test3 <- chisq.test(tbl3)
std_resid3 <- chi_test3$stdres

print(tbl3)
print(chi_test3)
print(std_resid3)

# --- STEP 6: ロジスティック回帰（gap ~ クラスタ） ---
combined3$ClusterLabel <- factor(combined3$ClusterLabel)

model3 <- glm(gap_flag ~ ClusterLabel, data = combined3, family = binomial)
summary(model3)

ggplot(gap_only3, aes(x = ClusterLabel, y = 割合, fill = ClusterLabel)) +
  geom_col() +
  geom_text(aes(label = paste0(round(割合,1), "%")), vjust = -0.5) +
  labs(title = "クラスタ別 欲しいけど買っていない割合(レッドブル)",
       x = "クラスタ",
       y = "割合 (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# 乳製品
intent_freq_data4 <- main %>%
  select(row_id, PS_CAT_04, PI_CAT_04) %>%
  mutate(
    PS_CAT_04 = case_when(
      is.na(PS_CAT_04) ~ NA_real_,
      PS_CAT_04 == "ここ１ヶ月では食べていない" ~ 0,
      TRUE ~ 1
    ),
    PI_CAT_04 = case_when(
      is.na(PI_CAT_04) ~ NA_real_,
      PI_CAT_04 %in% c("ぜひ食べたい", "食べたい") ~ 1,
      TRUE ~ 0
    )
  )
# --- STEP 3: クラスタ情報と結合し、gapフラグ追加 ---
combined4 <- factor_scores3 %>%
  mutate(row_id = rec_item_complete$row_id) %>%
  left_join(intent_freq_data4, by = "row_id") %>%
  mutate(
    一致 = ifelse(PS_CAT_04 == PI_CAT_04, 1, 0),
    gap_flag = ifelse(PS_CAT_04 == 0 & PI_CAT_04 == 1, 1, 0)
  ) %>%
  drop_na(一致)

# --- STEP 4: 一致率と「欲しいけど買っていない」割合の集計 ---
一致率集計4 <- combined4 %>%
  group_by(ClusterLabel) %>%
  summarise(
    一致率 = mean(一致),
    件数 = n()
  )

gap_only4 <- combined4 %>%
  group_by(ClusterLabel) %>%
  summarise(
    欲しいけど買っていない数 = sum(gap_flag == 1),
    全体数 = n(),
    割合 = 欲しいけど買っていない数 / 全体数 * 100
  )

print(一致率集計4)
print(gap_only4)

# --- STEP 5: カイ二乗検定（クラスタ×gap） ---
tbl4 <- table(combined4$ClusterLabel, combined4$gap_flag)
chi_test4 <- chisq.test(tbl4)
std_resid4 <- chi_test4$stdres

print(tbl4)
print(chi_test4)
print(std_resid4)

# --- STEP 6: ロジスティック回帰（gap ~ クラスタ） ---
combined4$ClusterLabel <- factor(combined4$ClusterLabel)

model4 <- glm(gap_flag ~ ClusterLabel, data = combined4, family = binomial)
summary(model4)

ggplot(gap_only4, aes(x = ClusterLabel, y = 割合, fill = ClusterLabel)) +
  geom_col() +
  geom_text(aes(label = paste0(round(割合,1), "%")), vjust = -0.5) +
  labs(title = "クラスタ別 欲しいけど買っていない割合(乳製品)",
       x = "クラスタ",
       y = "割合 (%)") +
  theme_minimal() +
  theme(legend.position = "none")



# まとめて実施(おためし) ------------------------------------------------------------------

analyze_gap_by_category <- function(main, factor_scores3, rec_item_complete, ps_col, pi_col, label) {
  # PS/PIを0-1に変換
  intent_data <- main %>%
    select(row_id, all_of(c(ps_col, pi_col))) %>%
    mutate(
      !!ps_col := case_when(
        is.na(.data[[ps_col]]) ~ NA_real_,
        .data[[ps_col]] %in% c("ここ１ヶ月では飲んでいない", "ここ1ヶ月では食べていない", "")  ~ 0,
        TRUE ~ 1
      ),
      !!pi_col := case_when(
        is.na(.data[[pi_col]]) ~ NA_real_,
        .data[[pi_col]] %in% c("ぜひ飲みたい", "飲みたい", "ぜひ食べたい", "食べたい") ~ 1,
        TRUE ~ 0
      )
    )
  
  # 結合して gap flag 計算
  combined <- factor_scores3 %>%
    mutate(row_id = rec_item_complete$row_id) %>%
    left_join(intent_data, by = "row_id") %>%
    mutate(
      一致 = ifelse(.data[[ps_col]] == .data[[pi_col]], 1, 0),
      gap_flag = ifelse(.data[[ps_col]] == 0 & .data[[pi_col]] == 1, 1, 0)
    ) %>%
    drop_na(一致)
  
  # 集計
  gap_summary <- combined %>%
    group_by(ClusterLabel) %>%
    summarise(
      一致率 = mean(一致),
      欲しいけど買っていない割合 = mean(gap_flag) * 100,
      件数 = n()
    )
  
  # カイ二乗検定
  tbl <- table(combined$ClusterLabel, combined$gap_flag)
  chi <- chisq.test(tbl)
  residuals <- chi$stdres
  
  # ロジスティック回帰
  model <- glm(gap_flag ~ ClusterLabel, data = combined, family = binomial)
  
  # 可視化
  p <- combined %>%
    group_by(ClusterLabel) %>%
    summarise(割合 = mean(gap_flag) * 100) %>%
    ggplot(aes(x = ClusterLabel, y = 割合, fill = ClusterLabel)) +
    geom_col() +
    geom_text(aes(label = paste0(round(割合, 1), "%")), vjust = -0.5) +
    labs(title = paste0("クラスタ別 欲しいけど買っていない割合 (", label, ")"),
         x = "クラスタ", y = "割合 (%)") +
    theme_minimal() +
    theme(legend.position = "none")
  
  list(
    summary = gap_summary,
    chi_table = tbl,
    chi_result = chi,
    chi_residuals = residuals,
    model_summary = summary(model),
    plot = p
  )
}


main <- main %>% mutate(row_id = row_number())  # 行番号追加（1回だけ）

results <- list(
  "ノンアル" = analyze_gap_by_category(main, factor_scores3, rec_item_complete, "PS_CAT_01", "PI_CAT_01", "ノンアル"),
  "ヘルシア" = analyze_gap_by_category(main, factor_scores3, rec_item_complete, "PS_CAT_02", "PI_CAT_02", "ヘルシア"),
  "レッドブル" = analyze_gap_by_category(main, factor_scores3, rec_item_complete, "PS_CAT_03", "PI_CAT_03", "レッドブル"),
  "乳製品" = analyze_gap_by_category(main, factor_scores3, rec_item_complete, "PS_CAT_04", "PI_CAT_04", "乳製品"),
  "インスタント食品" = analyze_gap_by_category(main, factor_scores3, rec_item_complete, "PS_CAT_05", "PI_CAT_05", "インスタント食品")
)


# たとえばノンアルの分析結果を表示
results$ノンアル$summary          # ギャップの集計
results$ノンアル$chi_result       # カイ二乗検定
results$ノンアル$model_summary    # ロジスティック回帰
results$ノンアル$plot             # グラフ

library(patchwork)
results$ノンアル$plot + results$ヘルシア$plot +
  results$レッドブル$plot + results$乳製品$plot + 
  results$インスタント食品$plot

