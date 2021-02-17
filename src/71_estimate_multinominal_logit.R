##### 0. 前準備 ========================================================
library(tidyverse)
library(rstan)
library(brms)
library(tidybayes)

#Stanの高速化につながるオプション
rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())

# データのファイルパス。各自のパソコンのディレクトリ構造に合わせて書き替える。
DATA_PATH <- "~/Data/JAHEAD/Process_Files/"

# これまでのデータの読み込み
load(str_c(DATA_PATH, "data_after_41_data_frame.rda"))

##### 1. データの準備 =====================================
stan_data <- data_complete_cases

# 念のためアウトカムが意図通り作られているか確認
stan_data %>% count(careing, careing_numeric)


##### 2. 分析 =====================================
# 最終的に福祉社会学会に投稿したモデル
fit_hmnl_2levels_noninformative_nointercept <- brm(careing_numeric ~ -1 + is_chounan + is_daughter + is_chounan_yome + is_other_yome + is_daughters_husband +
                                                      ch_dist_living_l +
                                                       t_female + lim_functions + living_spouse + use_publicservices_d + 
                                                       (1|id_personyear_n),
                                                   data = stan_data, family = categorical(link = "logit"),
                                                   chains = 4,
                                                   iter = 5000,
                                                   warmup = 2500,
                                                   seed = 1234,
                                                   # 全パラメータの事前分布を無情報で推定
                                                   prior = c(prior("", class = "sd", dpar = "mu2"),
                                                             prior("", class = "sd", dpar = "mu3")))

save(fit_hmnl_2levels_noninformative_nointercept,
     file=str_c(DATA_PATH, "stan_result_varying_intercept_multinomial_logit_noninformative_nointercept.rda"))
summary(fit_hmnl_2levels_noninformative_nointercept)

# 使われたStanコードの確認
fit_hmnl_2levels_2services_noninformative_nointercept$model

# モデルで使われた事前分布の確認
# 全てのパラメータの事前分布を無情報としたので、
# priorの列が全て空白となっているはず
prior_summary(fit_hmnl_2levels_noninformative_nointercept)

# 推定結果のトレースプロットやヒストグラムをざっと確認
plot(fit_hmnl_2levels_noninformative_nointercept)
