##### このファイルについて ========================================================
# Stanに渡すデータを特定する
# 利用するケースを抜き出してデータフレームを作成。
# ここで欠損が全くないデータフレームを作っておく。
# Stanに渡すリスト形式のデータは、ここで作成したデータフレームから削り出して使う。

# [注意]
# パッケージとして呼び出していないが、rethinkingというパッケージを用いているので、再現するにあたって導入する。
# rethinkingはCRANにないのでdevtoolsで経由でGitHubからインストールする。
# rethinking::mapという名前の関数があるらしく、purrrと干渉するので関数ごとに呼び出して使う。

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
load(str_c(DATA_PATH, "data_after_14.rda"))
load(str_c(DATA_PATH, "data_after_23.rda"))

##### 1. Stanに渡せる、欠損のないケースをここで完全に特定する ======================================================
# ここで作成したデータから必要なものを、モデルごとに切り出してリスト化する
# リストにするのは別のファイルで行う。

data_complete_cases <- data_united %>% 
    # Stanに投入する変数だけ抜き出す
    # ここで抽出したケースが分析のサンプルサイズを決めるので、モデルで使わない変数は落とす
    # 子どもの婚姻状態は使うかかなり迷っているので、念のため残しておく
    select(id_personyear_child_d, id_personyear_child, id_personyear, id_text, wave,
           do_care_parents_adl, #ADLで支援のある人を抜き出す
           do_care_parents_iadl, #IADLで支援のある人を抜き出す
           do_care_parents_iadl_only,
           is_child, ch_sex, ch_kinship, ch_married, ch_dist_living, ch_dist_living_l,
           # 親の情報でモデルに含める変数
           t_age, lim_adl, lim_iadl, use_dayservice_d, use_dayservice_n, use_homehelp_d, use_homehelp_n, use_publicservices_d, living_spouse,
           t_gender
           ) %>% 
    # ここから加工するべき変数を加工する
    # 「NA」という文字列で入っているものを除いたりするので、後には回せない。
    mutate(ch_female = case_when(ch_sex == "男性" ~ 0,
                                 ch_sex == "女性" ~ 1),
           ch_married = case_when(ch_married == "はい" ~ 1,
                                  ch_married == "いいえ" ~ 0),
           is_real_child = case_when(is_child == "子ども" ~ 1,
                                     is_child == "子の配偶者" ~ 0),
           is_chounan = case_when(ch_kinship == "長男" ~ 1,
                              is.na(ch_kinship) ~ NA_real_,
                              TRUE ~ 0),
           is_other_son = case_when(ch_kinship == "次男以降息子" ~ 1,
                                  is.na(ch_kinship) ~ NA_real_,
                                  TRUE ~ 0),
           is_daughter = case_when(ch_kinship == "娘" ~ 1,
                                   is.na(ch_kinship) ~ NA_real_,
                                   TRUE ~ 0),
           is_chounan_yome = case_when(ch_kinship == "長男ヨメ" ~ 1,
                                    is.na(ch_kinship) ~ NA_real_,
                                    TRUE ~ 0),
           is_other_yome = case_when(ch_kinship == "次男以降ヨメ" ~ 1,
                                       is.na(ch_kinship) ~ NA_real_,
                                       TRUE ~ 0),
           is_daughters_husband = case_when(ch_kinship == "ムコ" ~ 1,
                                    is.na(ch_kinship) ~ NA_real_,
                                    TRUE ~ 0),
           is_ch_live_samehh = if_else(ch_dist_living == "同居", 1, 0),
           is_ch_live_near = if_else(ch_dist_living == "10分未満"|ch_dist_living == "1時間未満", 1, 0),
           is_ch_live_far = if_else(ch_dist_living == "1時間以上", 1, 0),
           ch_dist_living_time = recode(ch_dist_living_l,
                                        `1` = 0,
                                        `2` = 10,
                                        `3` = 60,
                                        `4` = 120)
           ) %>% 
    mutate(lim_functions = lim_adl + lim_iadl) %>% 
    mutate(t_female = case_when(t_gender == "女性" ~ 1,
                                t_gender == "男性" ~ 0)) %>%
    # ここでStanに渡すべきデータが完全に特定
    filter(complete.cases(.)) %>% 
    # Stanで使う共通IDを作成。各種のIDを1からの連番にする。
    arrange(id_personyear_child_d) %>% 
    mutate(id_personyear_child_d_n = as.factor(id_personyear_child_d),
           id_personyear_child_n = as.factor(id_personyear_child),
           id_personyear_n = as.factor(id_personyear),
           id_n = as.factor(id_text)) %>% 
    mutate(id_personyear_child_d_n = unclass(id_personyear_child_d_n),
           id_personyear_child_n = unclass(id_personyear_child_n),
           id_personyear_n = unclass(id_personyear_n),
           id_n = unclass(id_n)) %>% 
    # アウトカムの変数を数字にする(レファレンスは一番小さい数字になるようにしている)
    mutate(careing = case_when(do_care_parents_adl == 1 ~ "a_ADLに対して支援",
                               do_care_parents_iadl_only == 1 ~ "b_IADLのみ支援",
                               do_care_parents_adl == 0 & do_care_parents_iadl_only == 0 ~ "0_ケアなし", # これをレファレンスカテゴリーに。
                               TRUE ~ NA_character_)) %>% 
    mutate(careing_numeric = rethinking::coerce_index(careing))

# チェック用
data_complete_cases %>% count(careing, careing_numeric)

# 念のためIDが問題なく作れているかチェック(データ名変わっているので使うとき修正)
# max(data_complete_cases$id_personyear_child_n)
# max(data_complete_cases$id_personyear_n)
# cor.test(1:nrow(data_stan_child), data_stan_child$id_personyear_child_n)
# cor.test(1:nrow(data_stan_panel), data_stan_panel$id_personyear_n)

##### Fin. 作成したファイルを保存 ================================================
# 作成したファイルを保存し、これまで作ったオブジェクトはいったん全て削除
save(data_complete_cases, file = str_c(DATA_PATH, "data_after_41_data_frame.rda"))
rm(list = ls())
