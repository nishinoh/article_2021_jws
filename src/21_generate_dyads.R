##### このファイルについて ========================================================
# 子ども1人1人について独立したデータとして再構成する

##### 0. 前準備 ========================================================
# library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(tibble)
library(readr)
library(purrr)

# データのファイルパス。各自のパソコンのディレクトリ構造に合わせて書き替える。
DATA_PATH <- "~/Data/JAHEAD/Process_Files/"

# これまでのデータの読み込み
load(str_c(DATA_PATH, "data_after_14.rda"))

##### 1. 子ども1人の1年分の情報で1行になるようにデータを作成する関数 ===========
# 変数名のキー(共通の名前)を指定したら、子ども1人1年分で1行になるように変換する
# 質問ごとにデータを作成して後で統合する

generateChilds <- function(data, variable){
    quote_varname <- deparse(substitute(variable))
    output_varname <- str_c("ch_", quote_varname)
    data %>%
        # まずは必要な変数だけ抜き出す
        # containsだけで絞れないかもしれないので、先に子どもの変数だけ抜き出して、2段階で抜き出す
        select(id_text, wave, id_personyear, starts_with("ch_")) %>% 
        select(id_text, wave, id_personyear, contains(quote_varname)) %>% 
        gather(key=question, value=value, -id_text, -wave, -id_personyear) %>% 
        # 下で4～5文字目を抜き出すというのは、「ch_01_age」などの形式の変数名となっているのが前提
        # 変数名は「00_bariable_names.csv」のファイルを参照
        mutate(ch_number = str_sub(question, 4,5)) %>% 
        arrange(id_personyear) %>% 
        filter(value != "NA",
               value != "非該当") %>% 
        #子ども番号が、別のWaveでは一貫しないので、id_personyearごとにだけIDを作る
        mutate(id_personyear_child = str_c(id_personyear, "-C", ch_number),
               id_child = str_c(id_text, "-C", ch_number)) %>% 
        select(id_text, id_personyear, id_personyear_child, id_child, wave, ch_number, value) %>% 
        mutate(ch_number = as.integer(ch_number)) %>% 
        rename(!!output_varname := value)
}

##### 2. 関数を使ってデータを作成 ==================================================

# 実際に関数を適用
dyad_ch_sex <- generateChilds(data_long, sex) # 性別　ch005_xx - 1:20
dyad_ch_age <- generateChilds(data_long, age) # 年齢
dyad_ch_married <- generateChilds(data_long, married) # 婚姻状態
dyad_ch_working <- generateChilds(data_long, working) # 働いているか
dyad_ch_dist_living <- generateChilds(data_long, dist_living)

# 結合
id_var <- c("id_text", "id_personyear", "id_personyear_child", "id_child", "wave", "ch_number")
data_child_dyad <- dyad_ch_sex %>%
    left_join(dyad_ch_age, by=id_var) %>% 
    left_join(dyad_ch_married, by=id_var) %>% 
    left_join(dyad_ch_working, by=id_var) %>% 
    left_join(dyad_ch_dist_living, by=id_var)

##### 3. 不具合修正 ==================================================
# ラベルの中身がWaveごとに全角半角混在していたので揃える
data_child_dyad <- data_child_dyad %>% 
    mutate(ch_dist_living = case_when(ch_dist_living == "１０分未満" ~ "10分未満",
                                      ch_dist_living == "１時間未満" ~ "1時間未満",
                                      ch_dist_living == "１時間以上" ~ "1時間以上",
                                      ch_dist_living == "DK" ~ NA_character_,
                                      ch_dist_living == "DK/NA" ~ NA_character_,
                                      TRUE ~ ch_dist_living)) %>% 
    mutate(ch_dist_living = fct_relevel(ch_dist_living,
                                        c("同居", "10分未満", "1時間未満", "1時間以上"))) %>% 
    mutate(ch_dist_living_l = unclass(ch_dist_living))

##### 4. 長子ダミーを作成 ======================================
# 子どもの年齢をベースに長子(ここでは健在の子どもで一番年齢が高い子)のダミーを作成
# 子どもの番号が必ずしも年齢順に並んでいるとは限らないというようなことを聞いた気がする。

# 下のmap()の中で使う関数
maxAge <- function(data){
    max(data$ch_age, na.rm=TRUE) #グループ全てでNAだと-Infが返されるけど、それ以外のことを考えるとna.rmはあった方がよいと思う。
}

data_child_dyad <- data_child_dyad %>% 
    # 年齢情報が"DK/NA"なども含む文字列なので、数値に変換(一応元の変数も残しておく)
    mutate(ch_age_char = ch_age,
           ch_age = parse_number(ch_age, na=c("DK", "DK/NA"))) %>% 
    # 回答者ごとに、子どもの年齢の最大値を求める(上の関数を利用)
    # 子の性別ごとにグルーピングし、その中での最大値を求める。
    # 健在の息子の中で年齢が一番高い場合を長男、健在の娘の中で年齢が一番高い場合を長女として扱う。
    group_by(id_personyear, ch_sex) %>% 
    nest() %>% 
    mutate(oldest_samesex_siblings_age = map(data, maxAge)) %>% 
    unnest() %>% 
    ungroup() %>% 
    # 上の処理でグループ内全てNAだと-Infが返されるので(たぶん仕方ない)、-InfをNAに変える
    mutate(oldest_samesex_siblings_age = if_else(oldest_samesex_siblings_age < 0,
                                         NA_real_, oldest_samesex_siblings_age)) %>% 
    # 長男/長女ダミーを作成
    mutate(is_chounan_choujo = if_else(ch_age == oldest_samesex_siblings_age, 1, 0)) %>% 
    mutate(is_chounan = if_else(ch_sex == "男性" & is_chounan_choujo == 1, 1, 0),
           is_choujo = if_else(ch_sex == "女性" & is_chounan_choujo == 1, 1, 0)) %>% 
    select(-is_chounan_choujo)


##### Fin. 作成したファイルを保存 ================================================
# 作成したファイルを保存し、これまで作ったオブジェクトはいったん全て削除
save(data_child_dyad, file=str_c(DATA_PATH, "data_after_21.rda"))
rm(list = ls())
