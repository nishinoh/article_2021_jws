##### このファイルについて ========================================================
# 子どものノードに、親の情報を付け足していく

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
load(str_c(DATA_PATH, "data_after_21b.rda"))

##### 1. データを統合

# 回答者(親)の情報のうち必要なものだけ抜き出し
tmp <- data_long %>% 
    select(id, id_text, id_personyear, wave, int_year, t_age, t_gender,
           num_hh_member, living_spouse,
           use_dayservice, use_shortstay, use_homehelp,
           use_dayservice_d, use_shortstay_d, use_homehelp_d,
           use_publicservices_d,
           use_dayservice_n, use_shortstay_n, use_homehelp_n,
           lim_adl, lim_iadl, lim_ability, needs_type,
           exist_helper_adl, exist_helper_adl_l, exist_helper_iadl, exist_helper_iadl_l,
           who_helped_1_c, who_helped_1_ch_id, who_helped_2_c, who_helped_2_ch_id)

keys <- c("id_text", "id_personyear", "wave")
data_united <- data_child_dyad %>% 
    left_join(tmp, by=keys)

##### Fin. 作成したファイルを保存 ================================================
# 作成したファイルを保存し、これまで作ったオブジェクトはいったん全て削除
save(data_united, file=str_c(DATA_PATH, "data_after_22.rda"))
rm(list = ls())