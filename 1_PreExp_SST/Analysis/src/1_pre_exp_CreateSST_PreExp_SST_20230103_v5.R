#最終更新: 2023年2月8日 00:15
#v3とv4の間で項目の修正事項はなし

library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(stringr)
library(irr)
library(openxlsx)
library(psych)
library(DT)

#SSTの回答データの読み込みと列名の変更
dat_part <- read.xlsx("../edited_dat/20230213_PreExp_SST_ClassifyAnswer_v5.1.xlsx")
dat_part <- dat_part %>% rename("score" = "SHに基づく得点", "supp" = "SH：補足")

#2023/1/26追記
#得点が著しく低い1名(cwid: 215031)は除外
#minの調べ方はwhich.min(dat_part$sum_score)で求めた
dat_part <- dat_part %>% subset(dat_part$cwid != "2105031")

#孤独-砂漠, 鍵-答え, 学位-橋, 若者-鳥を除外する→クロンバックα=.62
dat_part_v5 <- dat_part %>% filter(!trial_index %in% c(12, 20, 23, 25))

## 項目ごとのを見る ----
#ref: https://community.rstudio.com/t/how-to-make-a-grid-with-psych-describeby/95957/4
describe_by_ti <- describeBy(dat_part_v5$score, group = dat_part_v5$trial_index)#scoreをtrial_groupごとに取り出す
describe_score_df <-do.call("rbind", describe_by_ti)

#word_pairを抽出して結合
#ref: https://mi-chan-nel.com/rbind-and-cbind/
column_word_pair <- data.frame(unique(dat_part_v5$word_pair))
names(column_word_pair) <- c("word_pair")
describe_score_df <- cbind(describe_score_df, column_word_pair)

#項目全体のヒストグラムを書く
score_item_mean <- dat_part_v5 %>% select(8, 4, 10) %>%
  dplyr::group_by(word_pair) %>%
  dplyr::summarise(mean_item_score = mean(score))

#項目平均のヒストグラムを書く
ggplot(data = score_item_mean, mapping = aes(x = mean_item_score)) +  # 使うデータやx軸として使う項目の設定
  geom_histogram(binwidth = 0.1)
#ggsave("../_images/score_item_mean_v5.pdf", device = "pdf")

## 参加者ごとの得点を見る ----
score_part_sum <- dat_part_v5 %>% select(4, 2, 8, 10) %>%
  dplyr::group_by(cwid) %>%
  dplyr::summarise(sum_score = sum(score))

# 参加者の合計得点の分布を確認する
describe(score_part_sum[["sum_score"]])
  
#参加者全体のヒストグラムを書く
ggplot(data = score_part_sum, mapping = aes(x = sum_score)) +  # 使うデータやx軸として使う項目の設定
  geom_histogram(binwidth = 1)
#ggsave("../_images/score_part_mean_v5.pdf", device = "pdf")

#クロンバックのαを求める
dat_part_wide <- dat_part_v5 %>%
  select(4, 2, 10) %>%
  tidyr::spread(trial_index, score)

#αを求める→.62
#ref: http://mizumot.com/handbook/?page_id=50
psych::alpha(dat_part_wide[, -1])

