analyze_polychoric_cor <- function(df) {
  library(psych)
  library(dplyr)
  library(tidyr)
  library(corrplot)

  
   
  # 1. ポリコリック相関行列を計算
  poly_result <- polychoric(df)
  rho <- poly_result$rho
  
  # 2. 相関行列を tidy（ペアごと）形式に変換
  cor_pairs <- as.data.frame(as.table(rho)) %>%
    filter(Var1 != Var2) %>%  # 自己相関を除く
    filter(as.integer(Var1) < as.integer(Var2)) %>%  # 重複を防ぐ（1 vs 2 だけ）
    rename(var1 = Var1, var2 = Var2, correlation = Freq) %>%
    arrange(desc(abs(correlation)))  # 絶対値で並べ替え
  
  # 3. 相関行列をヒートマップで表示
  corrplot(rho, method = "color", type = "upper", order = "hclust", 
           addCoef.col = "black", tl.cex = 0.8, number.cex = 0.7,
           title = "Polychoric Correlation Heatmap", mar = c(0,0,2,0))
  
  return(cor_pairs)
}

analyze_polychoric_cor(rec_item_num)
rec_item_trim <- rec_item_num %>%
  +     select(-REC_Scale_05_MX)
> 
  > # ポリコリック相関で因子分析（必要ならfa.parallelも再実行）
  > result_trim <- fa(rec_item_trim, nfactors = 4, fm = "ml", rotate = "geominQ", cor = "poly")
> 
  > # 結果表示（負荷量が改善されたかチェック）
  > print(result_trim$loadings, cutoff = 0.3, sort = TRUE)

library(psych)







# 順序尺度として変換（必要に応じて）
rec_item_ord_trim <- main %>%
  select(matches("^REC_Scale_\\d{2}_MX$")) %>%
  select(-REC_Scale_01_MX, -REC_Scale_05_MX, -REC_Scale_08_MX) %>%
  mutate(across(everything(), ~ factor(.,
                                       levels = c("違う", "やや違う", "どちらともいえない", "だいたいその通り", "その通り"),
                                       ordered = TRUE))) %>%
  drop_na()

# ポリコリック相関行列（psych::polychoric を使う）
poly_result <- polychoric(rec_item_ord_trim)
cor_mat <- poly_result$rho  # 相関行列

# nearcor で「最も近い正定値行列」に補正
cor_mat_near <- nearcor(cor_mat, conv.tol = 1e-7)$cor

# この相関行列を使って因子分析（faに raw data の代わりに相関行列を渡す）
result_fixed <- fa(r = cor_mat_near, 
                   nfactors = 4, 
                   fm = "ml", 
                   rotate = "geominQ")
rec_items <- main %>%
  select(matches("^REC_Scale_\\d{2}_MX$"))

# すべての選択肢を確認（ユニーク値を一括表示）
lapply(rec_items, unique)
