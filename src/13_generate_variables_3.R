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
load(str_c(DATA_PATH, "data_after_12.rda"))

##### 1. 公的ケアの変数を準備 =================================
# DKなどを欠損にして、数値型に変更する

recodePublicService <- function(data, variable){
    variable_new <- str_c(variable, "_n")
    data <- data %>% 
        # しばらくは文字列型で操作して、最後に数値型にする
        mutate(!!(variable) := as.character(.[[variable]])) %>%
        mutate(!!(variable_new) := case_when(.[[variable]]=="利用していない" ~ "0",
                                             .[[variable]]=="DK" ~ NA_character_,
                                             .[[variable]]=="DK/NA" ~ NA_character_,
                                             .[[variable]]=="非該当" ~ "0",
                                             TRUE ~ .[[variable]])) %>% 
        mutate(!!(variable_new) := as.numeric(.[[variable_new]]))
}

data_long <- recodePublicService(data_long, "use_dayservice")
data_long <- recodePublicService(data_long, "use_shortstay")
data_long <- recodePublicService(data_long, "use_homehelp")

# 公的サービスを利用しているか否かのダミー変数を作成
data_long <- data_long %>% 
    mutate(use_dayservice_d = if_else(use_dayservice_n > 0, 1, 0),
           use_shortstay_d = if_else(use_shortstay_n > 0, 1, 0),
           use_homehelp_d = if_else(use_homehelp_n > 0, 1, 0),
           use_publicservices_d = if_else(use_dayservice_d + use_shortstay_d + use_homehelp_d >= 1, 1, 0))

##### Fin. 作成したファイルを保存 ================================================
# 作成したファイルを保存し、これまで作ったオブジェクトはいったん全て削除
save(data_long, file=str_c(DATA_PATH, "data_after_13.rda"))
rm(list = ls())
