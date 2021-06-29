# 08-1 --------------------------------------------------------------------

# 데이터 불러오기
library(dplyr)
library(readr)
bind_tweet <- bind_rows(
  read_csv("tweet_nak.csv") %>% mutate(candidate = "이낙연"),
  read_csv("tweet_jae.csv") %>% mutate(candidate = "이재명"))

glimpse(bind_tweet)


# -------------------------------------------------------------------------
install.packages("lubridate")
library(lubridate)
library(textclean)
library(stringr)

set.seed(1234)
tweet <- bind_tweet %>%

  mutate(text = replace_tag(str_to_lower(text)),  # id 태그 제거
         text = str_squish(replace_html(text)),   # html 특수 문자 제거
         date = date(created_at)) %>%             # 날짜 변수 생성

  filter(!str_detect(text, "https://")) %>%       # 광고 트윗 제거

  group_by(candidate) %>%                         # 중복 글 제거
  distinct(text, .keep_all = T) %>%

  group_by(candidate, date, screen_name) %>%      # 사용자별 하루 최대 5개 추출
  slice_sample(n = 5) %>%
  ungroup()

glimpse(tweet)


# -------------------------------------------------------------------------
# 날짜, 후보별 빈도
frequency_date <- tweet %>%
  count(date, candidate)

frequency_date

# 선 그래프
library(ggplot2)
ggplot(frequency_date, aes(x = date, y = n, col = candidate)) +
  geom_line()


# -------------------------------------------------------------------------
# 후보 색상 목록 생성
col_candidate <- c("#619CFF", "#B79F00")

ggplot(frequency_date, aes(x = date, y = n, col = candidate)) +  
  geom_line(size = 1) +
  geom_point(size = 2) +
  
  scale_x_date(date_labels = "%m/%d",                         # x축 날짜 포맷
               date_breaks  = "1 day") +                      # x축 날짜 간격
  scale_y_continuous(limits = c(0, 1200),                     # y축 범위
                     breaks = seq(0, 1200, 300)) +            # y축 간격
  scale_color_manual(values = col_candidate) +                # 선 색깔
  
  labs(title = "차기 대선주자 트위터 언급량 추이",            # 그래프 제목
       subtitle = "2020.8.13 ~ 2020.8.21",                    # 보조 제목
       x = NULL, y = NULL, col = NULL) +                      # 축 이름 삭제

  theme_minimal(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),  # 제목 폰트
        plot.subtitle = element_text(size = 12),              # 부제목 폰트
        panel.grid.minor.x = element_blank())                 # x축 보조축 삭제


# -------------------------------------------------------------------------
library(scales)
show_col(hue_pal()(6))


# -------------------------------------------------------------------------
# 영역 그래프
ggplot(frequency_date, aes(x = date, y = n, fill = candidate)) +
  geom_area(position = "dodge", alpha = 0.6)


# -------------------------------------------------------------------------
ggplot(frequency_date, aes(x = date, y = n, fill = candidate)) +
  geom_area(position = "dodge", alpha = 0.6) +
  geom_line(size = 0.5, alpha = 0.5) +
  
  scale_x_date(date_labels = "%m/%d", date_breaks  = "1 day") +
  scale_y_continuous(limits = c(0, 1200),
                     breaks = seq(0, 1200, 300)) +
  scale_fill_manual(values = col_candidate) +
  
  labs(title = "차기 대선주자 트위터 언급량 추이",
       subtitle = "2020.8.13 ~ 2020.8.21",
       x = NULL, y = NULL, fill = NULL) +
  
  theme_minimal(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),            
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())  # y축 보조축 삭제


# 08-2 --------------------------------------------------------------------

library(tidytext)
library(KoNLP)
word_tweet_raw <- tweet %>%
  unnest_tokens(input = text,
                output = word,
                token = "words",
                drop = F)


# -------------------------------------------------------------------------
frequency14 <- word_tweet_raw %>%
  mutate(category = ifelse(date == "2020-08-14", "target", "etc")) %>%
  filter(str_count(word) >= 2) %>%
  count(category, word, sort = T)

frequency14


# -------------------------------------------------------------------------
# Wide form으로 변환
library(tidyr)
wide14 <- frequency14 %>%
  pivot_wider(names_from = category,
              values_from = n,
              values_fill = list(n = 0))

# 로그 오즈비 변수 추가
wide14 <- wide14 %>%
  mutate(log_odds_ratio = log(((target + 1) / (sum(target + 1))) /
                                ((etc  + 1) / (sum(etc    + 1)))))

# log_odds_ratio 높은 순 출력
wide14 %>%
  arrange(-log_odds_ratio) %>%
  head(20)


# -------------------------------------------------------------------------
# 트윗 내용 확인
tweet %>%
  filter(date == "2020-08-14" & str_detect(text, "조사")) %>%
  head(10) %>%
  pull(text)


# -------------------------------------------------------------------------
frequency_nak1819 <- word_tweet_raw %>%
  mutate(category = ifelse(date >= "2020-08-18" &
                           date <= "2020-08-19", "target", "etc")) %>%
  filter(candidate == "이낙연" & str_count(word) >= 2) %>%
  count(category, word, sort = T)


# -------------------------------------------------------------------------
# Wide form으로 변환
wide_nak1819 <- frequency_nak1819 %>%
  pivot_wider(names_from = category,
              values_from = n,
              values_fill = list(n = 0))

# 로그 오즈비 변수 추가
wide_nak1819 <- wide_nak1819 %>%
  mutate(log_odds_ratio = log(((target + 1) / (sum(target + 1))) /
                                ((etc  + 1) / (sum(etc    + 1)))))


# log_odds_ratio 높은 순 출력
wide_nak1819 %>%
  arrange(-log_odds_ratio) %>%
  head(20)


# -------------------------------------------------------------------------
# 트윗 내용 확인
tweet %>%
  filter(date >= "2020-08-18" & date <= "2020-08-19" &
         candidate == "이낙연" & str_detect(text, "다행입니다")) %>%
  head(10) %>%
  pull(text)


# 08-3 --------------------------------------------------------------------

# 감정 사전 불러오기
dic <- read_csv("knu_sentiment_lexicon.csv")

# 감정 점수 부여, 감정 극성 분류
word_tweet <- word_tweet_raw %>%
  left_join(dic, by = "word") %>%                              # 감정 점수 부여
  mutate(polarity = ifelse(is.na(polarity), 0, polarity),      # NA를 0으로 변환
         sentiment = ifelse(polarity ==  2, "긍정",            # 감정 범주 분류
                     ifelse(polarity == -2, "부정", "중립")))


# -------------------------------------------------------------------------
# 자주 언급한 단어 추출
top10_word <- word_tweet %>%

  # 불용어 제거
  filter(!(candidate == "이낙연" & str_detect(word, "이낙연")) &
         !(candidate == "이재명" & str_detect(word, "이재명"))) %>%

  filter(str_count(word) >= 2) %>%
  count(candidate, sentiment, word) %>%

  group_by(candidate, sentiment) %>%
  slice_max(n, n = 10, with_ties = F)

top10_word


# -------------------------------------------------------------------------
ggplot(top10_word, aes(x = reorder_within(word, n, candidate),
                       y = n,
                       fill = sentiment)) +
  geom_col() +
  coord_flip() +
  facet_wrap(candidate ~ sentiment,  # 후보, 감정 범주별 그래프 생성
             scales = "free") +
  scale_x_reordered()


# -------------------------------------------------------------------------
col_sentiment <- c("#619CFF", "#00BA38", "#F8766D")  # 감정 색깔 목록
order_sentiment <- c("긍정", "중립", "부정")         # 감정 범주 목록

# 그래프 순서 지정
top10_word$sentiment <- factor(top10_word$sentiment,
                               levels = order_sentiment)

ggplot(top10_word, aes(x = reorder_within(word, n, candidate),
                       y = n,
                       fill = sentiment)) +
  geom_col() +
  coord_flip() +
  facet_wrap(candidate ~ sentiment,
             scales = "free") +
  scale_x_reordered() +
  scale_fill_manual(values = col_sentiment) +
  
  labs(title = "차기 대선주자 감정 단어",
       subtitle = "감정 극성별 빈도 Top 10",
       x = NULL, y = NULL, fill = NULL) +
  
  theme_minimal(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom")  # 범례 위치


# 08-4 --------------------------------------------------------------------

# 트윗 감정 점수 구하기
sentiment_tweet <- word_tweet %>%
  group_by(candidate, status_id) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

# 트윗 원문에 감정 점수 결합
tweet <- tweet %>%
  left_join(sentiment_tweet, by = c("candidate", "status_id"))

# 감정 점수 히스토그램
hist(tweet$score)


# -------------------------------------------------------------------------
ggplot(tweet, aes(x = score, fill = candidate)) +
  geom_density(adjust = 2, alpha = 0.6)


# -------------------------------------------------------------------------
ggplot(tweet, aes(x = score, fill = candidate)) +
  geom_density(adjust = 2, alpha = 0.6) +
  geom_vline(xintercept = 0,                   # 0점 위 세로선 표시
             linetype = "dashed",              # 점선 표시
             size = 0.5,
             alpha = 0.5) +
  
  scale_x_continuous(breaks = c(-5:5),         # x축 범위
                     limits = c(-5, 5)) +      # x축 간격
  scale_fill_manual(values = col_candidate) +
  
  labs(title = "차기 대선주자 감정 점수 분포",
       subtitle = "2020.8.13 ~ 2020.8.21",
       y = NULL, fill = NULL) +
  
  theme_minimal(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom",
        panel.grid = element_blank())          # 격자 삭제


# -------------------------------------------------------------------------
ggplot(tweet, aes(x = score, fill = candidate)) +
  geom_density() +
  facet_wrap(~ date)


# -------------------------------------------------------------------------
ggplot(tweet, aes(x = score, fill = candidate)) +
  geom_density(adjust = 2,
               alpha = 0.6) +
  
  geom_vline(xintercept = 0,
             linetype = "dashed",
             size = 0.5,
             alpha = 0.5) +
  
  facet_wrap(~ str_remove(date, "2020-"),                  # x축 년도 삭제
             scales = "free_y",
             ncol = 3,
             strip.position = "bottom") +
  
  scale_x_continuous(breaks = c(-5:5),
                     limits = c(-5, 5)) +
  scale_fill_manual(values = col_candidate) +
  
  labs(title = "차기 대선주자 일자별 감정 점수 분포",
       subtitle = "2020.8.13 ~ 2020.8.21",
       x = NULL, y = NULL, fill = NULL) +

  theme_bw(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.ticks = element_blank(),                      # 축 눈금 삭제
        axis.text = element_blank(),                       # 축 삭제
        strip.background = element_rect(colour = "black",  # 패널명 배경
                                        fill = "white"))  


# -------------------------------------------------------------------------
# 감정 분류 변수 생성
tweet <- tweet %>%
  mutate(sentiment = ifelse(score >= 1, "긍정",
                     ifelse(score <= -1, "부정", "중립")))

# 후보, 감정별 빈도 및 비율
frequency_sentiment <- tweet %>%
  group_by(candidate) %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n))

frequency_sentiment


# -------------------------------------------------------------------------
ggplot(frequency_sentiment, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~ candidate)


# -------------------------------------------------------------------------
# 순서 설정
frequency_sentiment$sentiment <- factor(frequency_sentiment$sentiment,
                                        levels = order_sentiment)


# -------------------------------------------------------------------------
library(scales)
ggplot(frequency_sentiment, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col(show.legend = F) +                   
  facet_wrap(~ candidate) +
  geom_text(aes(label = comma(n)), vjust = -0.5) +
  
  ylim(0, 3500) +
  scale_fill_manual(values = col_sentiment) +  # 막대 색깔
  
  labs(title = "차기 대선주자 트윗 감정 빈도",
       subtitle = "2020.8.13 ~ 2020.8.21",
       x = NULL, y = NULL) +
  
  theme_bw(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())


# -------------------------------------------------------------------------
ggplot(frequency_sentiment, aes(x = candidate, y = ratio, fill = sentiment)) +
  geom_col()


# -------------------------------------------------------------------------
# 막대 누적 순서 지정
frequency_sentiment$sentiment <- factor(frequency_sentiment$sentiment,
                                        levels = rev(order_sentiment))

ggplot(frequency_sentiment, aes(x = candidate, y = ratio, fill = sentiment)) +
  geom_col(show.legend = F, width = 0.7) +
  
  geom_text(aes(label = paste(sentiment, percent(ratio, accuracy = 0.1))),
            position = position_stack(vjust = 0.5)) +     # 수직 위치
  coord_flip() +
  scale_x_discrete(limits = c("이재명", "이낙연")) +
  
  labs(title = "차기 대선주자 트윗 감정 비율",
       subtitle = "2020.8.13 ~ 2020.8.21",
       x = NULL, y = NULL, fill = NULL) +
  
  theme_void(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom",
        axis.text.y = element_text(size = 12),            # y축 글자 크기
        plot.margin = margin(1, 1, 1, 1, unit = "line"))  # 여백 상우하좌, 단위


# 08-5 --------------------------------------------------------------------

# 날짜, 후보, 감정별 빈도
sentiment_candidate <- tweet %>%
  count(date, candidate, sentiment)

sentiment_candidate


# -------------------------------------------------------------------------
## 트윗 감정 추이 선 그래프
ggplot(sentiment_candidate, aes(x = date, y = n, col = sentiment)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ candidate, nrow = 2, scales = "free_x")


# -------------------------------------------------------------------------
# 중립 트윗 제외
tweet_polar <- sentiment_candidate %>%
  filter(sentiment != "중립")

ggplot(tweet_polar, aes(x = date, y = n, col = sentiment)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ candidate, nrow = 2, scales = "free_x")


# -------------------------------------------------------------------------
# 색깔 목록 생성
col_polar <- c("#619CFF", "#F8766D")

ggplot(tweet_polar, aes(x = date, y = n, col = sentiment)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ candidate, nrow = 2, scales = "free_x") +
  
  scale_x_date(date_labels = "%m/%d",
               date_breaks  = "1 day") +
  ylim(0, 250) +
  scale_color_manual(values = col_polar) +
  
  labs(title = "차기 대선주자 트윗 감정 추이",
       subtitle = "2020.8.13 ~ 2020.8.21",
       x = NULL, y = NULL, col = NULL) +
  
  theme_minimal(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        panel.grid.minor.x = element_blank(),
        panel.spacing = unit(2, "lines"))  # 그래프 간격
  

# -------------------------------------------------------------------------
ggplot(tweet_polar, aes(x = date, y = n, fill = sentiment)) +
  geom_area(position = "dodge", alpha = 0.7) +
  facet_wrap(~ candidate, nrow = 2, scales = "free_x")


# -------------------------------------------------------------------------
ggplot(tweet_polar, aes(x = date, y = n, fill = sentiment)) +
  geom_area(position = "dodge", alpha = 0.7) +
  facet_wrap(~ candidate, nrow = 2, scales = "free_x") +
  
  scale_x_date(date_labels = "%m/%d", date_breaks  = "1 day") +
  ylim(0, 250) +
  scale_fill_manual(values = col_polar) +
  
  labs(title = "차기 대선주자 트윗 감정 추이",
       subtitle = "2020.8.13 ~ 2020.8.21",
       x = NULL, y = NULL, fill = NULL) +
  
  theme_gray(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12), 
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))  # 그래프 간격 띄우기


# 08-6 --------------------------------------------------------------------

# 두 글자 이상 한글 단어 추출
word_sentiment_tweet <- word_tweet_raw %>%
  filter(str_detect(word, "[가-힣]") &
         str_count(word) >= 2) %>%

  # tweet 결합
  left_join(tweet %>% select(candidate, status_id, score, sentiment),
            by = c("candidate", "status_id"))

glimpse(word_sentiment_tweet)


# -------------------------------------------------------------------------
# 감정 범주 및 단어별 빈도 구하기
frequency_sentiment <- word_sentiment_tweet %>%  

  group_by(status_id) %>%                        # 트윗별 분리
  distinct(word, .keep_all = T) %>%              # 중복 단어 제거
  ungroup() %>%

  count(candidate, sentiment, word, sort = T)

frequency_sentiment


# -------------------------------------------------------------------------
# Wide form으로 변환
wide_pos <- frequency_sentiment %>%
  filter(sentiment == "긍정" & !str_detect(word, "이낙연|이재명")) %>%
  pivot_wider(names_from = candidate,
              values_from = n,
              values_fill = list(n = 0))

# 로그 오즈비 구하기
log_odds_pos <- wide_pos %>%
  mutate(log_odds_ratio = log(((이낙연 + 1) / (sum(이낙연 + 1))) /
                              ((이재명 + 1) / (sum(이재명 + 1)))))


# -------------------------------------------------------------------------
# 불용어 확인
log_odds_pos %>%
  group_by(candidate = ifelse(log_odds_ratio > 0, "이낙연", "이재명")) %>%
  slice_max(abs(log_odds_ratio), n = 15, with_ties = F) %>%
  select(word) %>%
  print(n = Inf)

# 불용어 목록 생성
stopword_pos <- c("것이고", "그건", "그는")


# -------------------------------------------------------------------------
# 로그 오즈비 상하위 10개 단어 추출
top10_pos <- log_odds_pos %>%
  filter(!word %in% stopword_pos) %>%
  group_by(candidate = ifelse(log_odds_ratio > 0, "이낙연", "이재명")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

# 막대 그래프 생성
ggplot(top10_pos, aes(x = reorder(word, log_odds_ratio),
                      y = log_odds_ratio,
                      fill = candidate)) +
  geom_col() +
  coord_flip()


# -------------------------------------------------------------------------
ggplot(top10_pos, aes(x = reorder(word, log_odds_ratio),
                      y = log_odds_ratio,
                      fill = candidate)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = col_candidate) +
  
  labs(title = "차기 대선주자 긍정 트윗 주요 단어",
       subtitle = "2020.8.13 ~ 2020.8.21",
       x = NULL, y = NULL, fill = NULL) +
  
  theme_minimal(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))


# -------------------------------------------------------------------------
# 단어 순서 지정해 factor 타입으로 변환
top10_pos <- top10_pos %>%
  ungroup() %>%
  mutate(word = reorder(word, log_odds_ratio))

# 롤리팝 차트 생성
ggplot(top10_pos, aes(x = word,
                      y = log_odds_ratio,
                      col = candidate)) +
  geom_segment(aes(x = word,                  # x축 시작점
                   xend = word,               # x축 끝점
                   y = 0,                     # y축 시작점
                   yend = log_odds_ratio)) +  # y축 끝점
  geom_point() +
  coord_flip()


# -------------------------------------------------------------------------
ggplot(top10_pos, aes(x = word,
                      y = log_odds_ratio,
                      col = candidate)) +
  
  geom_segment(aes(x = word,
                   xend = word,
                   y = 0,
                   yend = log_odds_ratio),
               size = 1.1) +
  
  geom_point(size = 3.5) +
  coord_flip() +
  scale_color_manual(values = col_candidate) +
  
  labs(title = "차기 대선주자 긍정 트윗 주요 단어",
       subtitle = "2020.8.13 ~ 2020.8.21",
       x = NULL, y = NULL, col = NULL) +
  
  theme_minimal(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12)) 


# -------------------------------------------------------------------------
# 이낙연 의원 긍정 트윗 추출
pos_nak <- tweet %>%
  filter(sentiment == "긍정" & candidate == "이낙연") %>%
  arrange(-score)

pos_nak %>% find_word(x = text, keyword = "음성")

pos_nak %>% find_word(x = text, keyword = "의원님")


# -------------------------------------------------------------------------
# 이재명 경기도지사 긍정 트윗 추출
pos_jae <- tweet %>%
  filter(sentiment == "긍정" & candidate == "이재명") %>%
  arrange(-score)

pos_jae %>% find_word(x = text, keyword = "경기도")

pos_jae %>% find_word(x = text, keyword = "이익을")


# -------------------------------------------------------------------------
pos_jae %>%
  filter(str_detect(text, "이익을")) %>%
  select(score)

pos_jae %>%
  filter(!str_detect(text, "이익을")) %>%
  select(score)


# -------------------------------------------------------------------------
# Wide form으로 변환
wide_neg <- frequency_sentiment %>%
  filter(sentiment == "부정" & !str_detect(word, "이낙연|이재명")) %>%
  pivot_wider(names_from = candidate,
              values_from = n,
              values_fill = list(n = 0))

# 로그 오즈비 구하기
log_odds_neg <- wide_neg %>%
  mutate(log_odds_ratio = log(((이낙연 + 1) / (sum(이낙연 + 1))) /
                              ((이재명 + 1) / (sum(이재명 + 1)))))


# -------------------------------------------------------------------------
# 불용어 확인
log_odds_neg %>%
  group_by(candidate = ifelse(log_odds_ratio > 0, "이낙연", "이재명")) %>%
  slice_max(abs(log_odds_ratio), n = 15, with_ties = F) %>%
  select(word) %>%
  print(n = Inf)

tweet %>%
  filter(candidate == "이재명") %>%
  find_word(x = text, keyword = "쓰나미급")

# 불용어 목록 생성
stopword_neg <- c("지금껏", "겪어보지", "대충격", "시작될", "그건", "주고")


# -------------------------------------------------------------------------
# 로그 오즈비 상하위 10개 단어 추출
top10_neg <- log_odds_neg %>%
  filter(!word %in% stopword_neg) %>%
  group_by(candidate = ifelse(log_odds_ratio > 0, "이낙연", "이재명")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

# 막대 그래프 생성
ggplot(top10_neg, aes(x = reorder(word, log_odds_ratio),
                      y = log_odds_ratio,
                      fill = candidate)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = col_candidate) +
  
  labs(title = "차기 대선주자 부정 트윗 주요 단어",
       subtitle = "2020.8.13 ~ 2020.8.21",
       x = NULL, y = NULL, fill = NULL) +
  
  theme_minimal(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))


# -------------------------------------------------------------------------
# 단어 순서 지정해 factor 타입으로 변환
top10_neg <- top10_neg %>%
  ungroup() %>%
  mutate(word = reorder(word, log_odds_ratio))

# 롤리팝 차트 생성
ggplot(top10_neg, aes(x = word,
                      y = log_odds_ratio,
                      col = candidate)) +
  
  geom_segment(aes(x = word,
                   xend = word,
                   y = 0,
                   yend = log_odds_ratio),
               size = 1.1) +
  
  geom_point(size = 3.5) +
  coord_flip() +
  scale_color_manual(values = col_candidate) +
  
  labs(title = "차기 대선주자 부정 트윗 주요 단어",
       subtitle = "2020.8.13 ~ 2020.8.21",
       x = NULL, y = NULL, col = NULL) +

  theme_minimal(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))


# -------------------------------------------------------------------------
# 이낙연 의원 부정 트윗 추출
neg_nak <- tweet %>%
  filter(sentiment == "부정" & candidate == "이낙연") %>%
  arrange(-score)

# 이재명 경기도지사 부정 트윗 추출
neg_jae <- tweet %>%
  filter(sentiment == "부정" & candidate == "이재명") %>%
  arrange(-score)


# -------------------------------------------------------------------------
neg_nak %>% find_word(x = text, keyword = "음성")

neg_nak %>% find_word(x = text, keyword = "의원님")

neg_jae %>% find_word(x = text, keyword = "경기도")

neg_jae %>% find_word(x = text, keyword = "쓰나미급")

