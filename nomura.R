## 野村練習
rm(list = ls())
pacman::p_load(summarytools,tidyverse)## 複数も未所持のパッケージも使える
source("nri_data_code.R")　## コードの全てを実行する
dfm <- dfSummary(main)
dfm$Variable <- vimain$ラベル
dfm |> stview()

# Yamaguchi ---------------------------------------------------------------

main |> 
  as_tibble() |>
  select(4,12,13) |> 
  group_by(MARRIAGE, FAMILY_CD) |> 
  summarise(n = n()) |> 
  arrange(-n)

main |> select(430) |> 
  table()

main %>% 
  select(430, 492, 493:508) %>%
  mutate(across(3:17, ~ as.numeric(.) - 1)) %>%
  mutate(radio = rowSums(select(., 3:17), na.rm = TRUE)) %>% 
  select(1, RADIO_TIME) %>% 
  group_by(HOB_05_MA, RADIO_TIME) %>% 
  summarise(n = n(), group = "drop") %>% 
  ggplot(aes(x = RADIO_TIME, y = n, fill = HOB_05_MA)) +
  geom_col(position = "dodge") + 
  facet_wrap(~HOB_05_MA, scale = "free_y") 


# 購買意欲の変化 -----------------------------------------------------------------

main %>%
  select(matches("03\\/08"))

vimain %>% 
  filter(str_detect(ラベル, "\\d{2}/\\d{2}")) %>%
  select(ラベル, 変数名) %>%
  mutate(
    ItemID = str_sub(変数名, 7,16), ##文字列が整っている場合(そうでない場合はextractでbの文字から始まる言葉をのような感じで)
    date= str_extract(ラベル,"\\d{2}/\\d{2}"),##正規表現(文字列をパターンで抜き出す)
    type = str_sub(変数名, 1, 2),
    product = str_extract(ラベル, "^[^（]+")) %>%
  select(ItemID, date, type, product, 変数名)

## まだできてない
main %>%
  select(SampleID, selected_vars$変数名) %>%
  mutate(across(-SampleID, as.numeric)) %>%
  pivot_longer(-SampleID) %>%
  right_join(selected_vars, by = c("name" = "変数名")) %>%
  select(-name) %>%
  mutate(date = lubridate::ymd(paste0("2024/", date))) %>%
  filter( type == "PS") %>% 
  group_by(ItemID) %>%
  mutate(
    period = case_when(
      date == min(date) ~ "pre",
      date == max(date) ~ "post",
    )
  ) %>% 
  ungroup() %>% 
  unite("type_period", type, period, sep = "") %>%
  pivot_wider(
    id_cols = c(SampleID, ItemID, product),
    names_from = type_period,
    values_from = value
  ) %>% 
  mutate(diff = PS_pre - PS_post) %>% 
  group_by(product) %>% 
  summarise(diff_mean = mean(diff,na.rm = T),
            diff_sd = sd(diff,na.rm = T)) %>% 
  arrange(-diff_mean,diff_sd)
