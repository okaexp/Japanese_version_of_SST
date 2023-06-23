#最終更新: 2023年6月22日 21:55

# 解析方針 ----
# 1. 解析用データの作成（dat_aggregate）：CrowdworksIDを行に持ち、ターゲットキー（後述）を列に持つ
# 共通のキー: CrowdworksID(demographic), cwid(rpms, sst, vocab_and_tipij)
# dat_demographic:
# - Sex
# - Age
# - AcademicDegree
#
# dat_sst:survey_text(trial_type)のFinalRatingの合計点を求める 
# - SumSSTFinalRating(df)
#
# dat_rpms: survey_likert(trial_type)のCrrectの合計点を求める
# - SumRPMSCorrect(df)
#
# dat_vpcab_and_tipij
# - Vocab_Check_All
# - Vocab_Check_P1
# - Vocab_Check_P2
# - Vocab_Check_P3
# - Extraversion_Agg
# - Agreeableness_Agg
# - AConscientious_Agg
# - Neuroticism_Agg
# - Openess_Agg
#
# 2. 相関分析

library(tidyr)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(psych)

# 1. 解析用データの作成（dat_aggregate）----

# 各種データの読み込み ----
dat_demographic <- read.xlsx("../edited_dat/20230313_dat_target_start_to_demographic.xlsx")
dat_rpms <- read.csv("../edited_dat/20230313_dat_target_rpms_with_score.csv")
dat_sst <- read.xlsx("../edited_dat/20230412_edited_dat_after_aggregate_v0.1.xlsx")
dat_vacob_and_tipij <- read.xlsx("../edited_dat/20230313_dat_target_vocab_to_finish.xlsx")

# ターゲットキーのみを集計したデータフレーム(dat_aggregate)を作る ----
# dat_demographic:
# - Sex
# - Age
# - AcademicDegree
dat_demographic_for_agg <- dat_demographic %>%
  dplyr::select(CrowdworksID, Sex, Age, AcademicDegree)

# dat_sst:survey_text(trial_type)のFinalRatingの合計点を求める 
# - SumSSTFinalRating(df)
dat_sst_for_agg <- dat_sst %>%
  dplyr::filter(trial_type == "survey-text") %>% 
  dplyr::mutate(CrowdworksID = cwid) %>%
  dplyr::group_by(CrowdworksID) %>%
  dplyr::summarise(SumSSTFinalRating = sum(FinalRating))

# dat_rpms: survey_likert(trial_type)のCrrectの合計点を求める
# - SumRPMSCorrect(df)
dat_rpms_for_agg <- dat_rpms %>% 
  dplyr::filter(trial_type == "survey-likert") %>%
  dplyr::mutate(CrowdworksID = cwid) %>% 
  dplyr::group_by(CrowdworksID) %>%
  dplyr::summarise(SumRPSMCorrect = sum(Correct))

# dat_vpcab_and_tipij
# - Vocab_Check_All
# - Vocab_Check_P1
# - Vocab_Check_P2
# - Vocab_Check_P3
# - Extraversion_Agg
# - Agreeableness_Agg
# - AConscientious_Agg
# - Neuroticism_Agg
# - Openess_Agg
dat_vocab_and_tipij_for_agg <- dat_vacob_and_tipij %>%
  dplyr::mutate(CrowdworksID = cwid) %>% 
  dplyr::select(CrowdworksID, Vocab_Check_All, Vocab_Check_P1, Vocab_Check_P2, Vocab_Check_P3,
                Extraversion_Agg, Agreeableness_Agg, Conscientious_Agg, Neuroticism_Agg, Openess_Agg)

# 解析用データの作成（dat_aggregate）
dat_aggregate <- dat_demographic_for_agg %>%
  dplyr::inner_join(dat_sst_for_agg) %>%
  dplyr::inner_join(dat_rpms_for_agg) %>%
  dplyr::inner_join(dat_vocab_and_tipij_for_agg)

# 2. 相関分析----
source("http://aoki2.si.gunma-u.ac.jp/R/src/mycor.R", encoding="euc-jp") #mycorを持ってくる
dat_aggregate_cor <- dplyr::select(dat_aggregate, -CrowdworksID) %>%
  dplyr::mutate(Education = AcademicDegree) %>%
  dplyr::select(-AcademicDegree) %>%
  dplyr::select(SumSSTFinalRating, SumRPSMCorrect, Vocab_Check_All, Vocab_Check_P1,
                Extraversion_Agg, Agreeableness_Agg, Conscientious_Agg, Neuroticism_Agg, Openess_Agg,
                Age, Education)

mycor(1:11, dat_aggregate_cor, latex = FALSE) #いつもの#相関係数ようのデータ

# 3. 相関表とヒストグラムの可視化 ----
dat_aggregate_panels <- dplyr::select(dat_aggregate_cor, -Sex, -Age, -Education,
                                      -Extraversion_Agg, -Agreeableness_Agg, -Conscientious_Agg, -Neuroticism_Agg, -Openess_Agg)
psych::pairs.panels(dat_aggregate_panels)

# 4. 記述統計量と必要な相関分析 ----

#記述統計量
dat_aggregate_desc_and_cor <- dat_aggregate_cor %>% 
  dplyr::select(-Vocab_Check_P2, -Vocab_Check_P3)
describe(dat_aggregate_desc_and_cor)

#相関係数
mycor(2:length(dat_aggregate_desc_and_cor), dat_aggregate_desc_and_cor, latex = FALSE) #相関係数ようのデータ

# 5. 相関係数の差の検定 ----
# ref: https://blog.statsbeginner.net/entry/2021/02/24/192247

#r12: SumSSTScore(2) was weakly correlated with Vocabulary_all(1; r = .29)
#r13: SumRPMSScore(3) was weakly correlated with Vocabulary_all (1; r= .23)
#r23: SumSSTScore(2) and SumRPMSScore(3) (r = .36)
psych::r.test(n=100, r12=0.29, r23=0.36, r13=0.23)

#r12: SumSSTScore(2) was weakly correlated with Vocabulary_Test_1(1; r = .31)
#r13: SumRPMSScore(3) was weakly correlated with Vocabulary_Test_1 (1; r= .27)
#r23: SumSSTScore(2) and SumRPMSScore(3) (r = .36)
psych::r.test(n=100, r12=0.31, r23=0.36, r13=0.27)

# 6. 相関分析（語彙数の外れ値除外）----
#楠見先生のご助言を踏まえてvocab_check_Allの著しく高い/低い(+-2.5SD)参加者を除外
vocab_all_plus_three_sd = psych::describe(dat_aggregate$Vocab_Check_All)$mean + 2.5*psych::describe(dat_aggregate$Vocab_Check_All)$sd
vocab_all_minus_three_sd = psych::describe(dat_aggregate$Vocab_Check_All)$mean - 2.5*psych::describe(dat_aggregate$Vocab_Check_All)$sd
#vocab_one_plus_three_sd = psych::describe(dat_aggregate$Vocab_Check_P1)$mean + 2.5*psych::describe(dat_aggregate$Vocab_Check_P1)$sd
#vocab_one_minus_three_sd = psych::describe(dat_aggregate$Vocab_Check_P1)$mean - 2.5*psych::describe(dat_aggregate$Vocab_Check_P1)$sd

dat_aggregate_remove_outlier_vocab <- dat_aggregate %>%
  dplyr::filter(Vocab_Check_All < vocab_all_plus_three_sd, Vocab_Check_All > vocab_all_minus_three_sd)# %>%
  #dplyr::filter(Vocab_Check_P1 < vocab_one_plus_three_sd, Vocab_Check_P1 > vocab_one_minus_three_sd)

source("http://aoki2.si.gunma-u.ac.jp/R/src/mycor.R", encoding="euc-jp") #mycorを持ってくる
dat_aggregate_cor_remove_outlier <- dplyr::select(dat_aggregate_remove_outlier_vocab, -CrowdworksID) %>%
  dplyr::mutate(Education = AcademicDegree) %>%
  dplyr::select(-AcademicDegree) %>%
  dplyr::select(SumSSTFinalRating, SumRPSMCorrect, Vocab_Check_All, Vocab_Check_P1,
                Extraversion_Agg, Agreeableness_Agg, Conscientious_Agg, Neuroticism_Agg, Openess_Agg,
                Age, Education)

mycor(1:11, dat_aggregate_cor_remove_outlier, latex = FALSE) #いつもの#相関係数ようのデータ

# 7. 相関表とヒストグラムの可視化（語彙数の外れ値の除外） ----
dat_aggregate_panels_remove_outlier <- dplyr::select(dat_aggregate_cor_remove_outlier, -Age, -Education,
                                      -Extraversion_Agg, -Agreeableness_Agg, -Conscientious_Agg, -Neuroticism_Agg, -Openess_Agg)
psych::pairs.panels(dat_aggregate_panels_remove_outlier)
