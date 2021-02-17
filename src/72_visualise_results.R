##### このファイルについて ========================================================
# 推定されたStanのデータから、結果を報告するグラフを作成する。
# 結果を抜き出してデータフレームを作成し、そこからキャタピラープロットを作成する。

##### 0. 前準備 ========================================================
library(tidyverse)
library(rstan)
library(brms)
library(tidybayes)

# データのファイルパス。各自のパソコンのディレクトリ構造に合わせて書き替える。
DATA_PATH <- "~/Data/JAHEAD/Process_Files/"
# 分析結果を保存するファイルパス。
OUTPUT_PATH <- "~/Data/JAHEAD/Results"

# 推定結果のオリジナルファイルを読み込み
load(str_c(DATA_PATH, "stan_result_varying_intercept_multinomial_logit_noninformative_nointercept.rda"))

##### 1. 分析結果の作成 ================================
output <- brms::fixef(fit_hmnl_2levels_noninformative_nointercept) %>% 
    as.data.frame(.) %>% 
    mutate(term = row.names(.)) %>% 
    select(term, everything()) %>% 
    tibble(.) %>% 
    mutate(mean_odds = exp(Estimate),
           ymin_odds = exp(Q2.5),
           ymax_odds = exp(Q97.5)) %>% 
    mutate(num = row_number()) %>% 
    separate(term,
             into=c("mu", "predictor"),
             sep="_",
             extra="merge" #intoの変数ではみ出る場合の処理。2つ目意向のアンダーバーは無視し、内容はまとめる。
    ) %>% 
    # 日本語ラベルをつけていく
    mutate(outcome = case_when(mu == "mu2" ~ "アウトカム1\n身体的介護を提供\n(ref=援助なし)\n",
                               mu == "mu3" ~ "アウトカム2\n家事･生活的援助のみ提供\n(ref=援助なし)\n")) %>%
    mutate(predictor_j = recode(predictor,
                                `Intercept` = "切片",
                                `is_chounan` = "続柄：長男\n(ref=長男以外の息子)",
                                `is_daughter` = "続柄：娘\n(ref=長男以外の息子)",
                                `is_chounan_yome` = "続柄：長男の妻\n(ref=長男以外の息子)",
                                `is_other_yome` = "続柄：長男以外の妻\n(ref=長男以外の息子)",
                                `is_daughters_husband` = "続柄：娘の夫\n(ref=長男以外の息子)",
                                `ch_dist_living_l` = "居住場所の距離",
                                `lim_functions` = "親の生活動作の困難度",
                                `living_spouse` = "親が配偶者と同居\n(同居=1)",
                                `use_publicservices_d` = "公的サービス利用\n(あり=1)",
                                `t_female` = "親の性別\n(女性=1)",
                                `t_age` = "親の年齢"
    ))

table <- output %>% 
    mutate(mean_odds = round(mean_odds, 2),
           ymin_odds = round(ymin_odds, 2),
           ymax_odds = round(ymax_odds, 2))

##### 2. ggplot2で結果を図示 ================================
# 準備: 不要な小数点を削除する関数。後でscale_y_continuous()の中で使う。
formatDicimals <- function(x,...) {
    format(x, ..., scientific = FALSE, drop0trailing = TRUE)
}

# グラフ本体の作成
fig <- output %>% 
    ggplot(aes(x=reorder(predictor_j, -num),
               y=mean_odds, ymax=ymax_odds, ymin=ymin_odds,
               group=outcome, shape=outcome)) +
    geom_pointrange(size=1,
                    position = position_dodge2(width = 0.5, reverse = TRUE)) +
    geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
    scale_y_continuous(trans="log", #軸を対数変換
                       breaks = c(0, 0.05, 0.1, 0.25, 0.5, 1, 2, 4, 10), #軸に表示する数値を手動で設定
                       labels = formatDicimals) +
    coord_flip() + 
    labs(y="オッズ比",
         shape = "アウトカム") +
    # scale_shape_discrete(limits=c("身体的ケアの提供\n(ref=援助なし)", "家事的支援のみ提供\n(ref=援助なし)")) +
    # guides(shape = guide_legend(reverse = TRUE)) + # 凡例の表示順のコントロール
    theme_bw(base_size = 16, base_family = "HiraKakuProN-W3") +
    theme(axis.text=element_text(colour="black"),
          axis.title.y=element_blank(),
          # legend.position = "bottom", #凡例を上に持ってきたければこれを有効にする
    )
print(fig)

#グラフをPDFファイルとして保存
quartz(file=str_c(OUTPUT_PATH, "fig_result_paper_fukushishakaigakukenkyu.pdf"),
       type="pdf",  width=10,height=10)
fig
dev.off()
