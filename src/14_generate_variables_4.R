##### 0. 前準備 ========================================================
# library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(tibble)
library(readr)

# データのファイルパス。各自のパソコンのディレクトリ構造に合わせて書き替える。
DATA_PATH <- "~/Data/JAHEAD/Process_Files/"

# これまでのデータの読み込み
load(str_c(DATA_PATH, "data_after_13.rda"))

##### 1. デモグラフィック情報を作成 =================================
# ラベルの共通化やカテゴリの再編を行う

# 年齢と性別
data_long <- data_long %>% 
    mutate(t_gender = fct_collapse(t_gender,
                                   男性 = c("男性", "Male"),
                                   女性 = c("女性", "Female")
                                   )) %>% 
    # 年齢を計算する(生まれた年と調査の年から単純計算)
    mutate(int_year = case_when(wave == 5 ~ 1998,
                                wave == 6 ~ 2002,
                                wave == 7 ~ 2006),
           t_age = int_year - t_birth_y)

# 居住地の都市規模
data_long <- data_long %>% 
    mutate(city_size_c = fct_collapse(city_size,
                                      町村 = "町村",
                                      `10万未満の市` = "10万未満の市",
                                      `10万以上の市` = c("10万以上の市",  "20万以上の市"),
                                      政令市と特別区 = c("13大市（千葉）", "13大市（札幌、仙台、川崎、神戸、広島、福岡）",
                                                  "13大市（東京23区、大阪）", "13大市（横浜、名古屋、京都、北九州）",
                                                  "16大市（さいたま）", "16大市（千葉、堺）",
                                                  "16大市（札幌、仙台、川崎、静岡、神戸、広島、福岡）",
                                                  "16大市（東京23区、大阪）", "16大市（横浜、名古屋、京都、北九州）")
                                      )) %>% 
    mutate(city_size_c = fct_relevel(city_size_c, c("町村", "10万未満の市", "10万以上の市", "政令市と特別区")))

##### 2. 世帯の情報を作成 =================================
# 世帯の人数を数値型に
data_long <- data_long %>% 
    mutate(num_hh_member = as.numeric(num_hh_member))

# 同じ世帯に配偶者が住んでいるか
# 世帯構成員に配偶者がいるかどうかで判断し、そもそも未回答はNAにする
tmp <- data_long %>%
    select(id_personyear, contains("hh_mem")) %>% 
    gather(key=key, value=value, -id_personyear, -num_hh_member) %>% 
    filter(str_detect(key, "relation")) %>% 
    # カテゴリの整理
    mutate(value = case_when(value == "NA" ~ NA_character_,
                             value == "DK/NA" ~ NA_character_,
                             value == "非該当" ~ NA_character_,
                             TRUE ~ value)) %>% 
    mutate(value = fct_collapse(value,
                               living_respondent = c("本人"),
                               living_spouse = c("配偶者"),
                               living_childs = c("子ども", "子供"),
                               living_childs_spouse = c("子どもの配偶者", "子供の配偶者", "子の配偶者"),
                               living_grand_childs = c("孫", "孫の配偶者"),
                               living_other = c("その他", "兄弟姉妹", "父母", "配偶者の父母"))) %>% 
    # 人数を数える
    filter(!is.na(value)) %>% 
    count(id_personyear, value) %>% 
    spread(key = value, value = n) %>% 
    select(id_personyear, living_respondent, living_spouse, living_childs,
           living_childs_spouse, living_grand_childs, living_other) %>% 
    # NAを0に変更。他はそのまま維持。livingから始まる列全てに適用。
    # dplyrの新し目のバージョンでは別の書き方が推奨されてるようなので調べて直した方がいいかも?
    mutate_at(vars(starts_with("living_")), funs(replace_na(., 0)))

# 世帯員の情報を結合
data_long <- data_long %>% 
    left_join(tmp, by="id_personyear")

# 他で上のtmpがうっかり使われないよう削除
rm(tmp)

##### Fin. 作成したファイルを保存 ================================================
# 作成したファイルを保存し、これまで作ったオブジェクトはいったん全て削除
save(data_long, file=str_c(DATA_PATH, "data_after_14.rda"))
rm(list = ls())
