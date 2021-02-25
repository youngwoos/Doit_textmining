# 06-2 --------------------------------------------------------------------

# 기생충 기사 댓글 불러오기
library(readr)
library(dplyr)

raw_news_comment <- read_csv("news_comment_parasite.csv") %>%
  mutate(id = row_number())


# -------------------------------------------------------------------------
library(stringr)
library(textclean)

# 기본적인 전처리
news_comment <- raw_news_comment %>%
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "),
         reply = str_squish(reply)) %>%

  # 중복 댓글 제거
  distinct(reply, .keep_all = T) %>%

  # 짧은 문서 제거 - 3 단어 이상 추출
  filter(str_count(reply, boundary("word")) >= 3)


# -------------------------------------------------------------------------
library(tidytext)
library(KoNLP)

# 명사 추출
comment <- news_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = extractNoun,
                drop = F) %>%
  filter(str_count(word) > 1) %>%

  # 댓글 내 중복 단어 제거
  group_by(id) %>%
  distinct(word, .keep_all = T) %>%
  ungroup() %>%
  select(id, word)

comment


# -------------------------------------------------------------------------
count_word <- comment %>%
  add_count(word) %>%
  filter(n <= 200) %>%
  select(-n)


# -------------------------------------------------------------------------
# 불용어, 유의어 확인하기
count_word %>%
  count(word, sort = T) %>%
  print(n = 200)


# -------------------------------------------------------------------------
# 불용어 목록 만들기
stopword <- c("들이", "하다", "하게", "하면", "해서", "이번", "하네",
              "해요", "이것", "니들", "하기", "하지", "한거", "해주",
              "그것", "어디", "여기", "까지", "이거", "하신", "만큼")


# -------------------------------------------------------------------------
# 불용어, 유의어 처리하기
count_word <- count_word %>%
  filter(!word %in% stopword) %>%
  mutate(word = recode(word,
                       "자랑스럽습니" = "자랑",
                       "자랑스럽" = "자랑",
                       "자한" = "자유한국당",
                       "문재" = "문재인",
                       "한국의" = "한국",
                       "그네" = "박근혜",
                       "추카" = "축하",
                       "정경" = "정경심",
                       "방탄" = "방탄소년단"))


# -------------------------------------------------------------------------
# tibble 구조로 불용어 목록 만들기
stopword <- tibble(word = c("들이", "하다", "하게", "하면", "해서", "이번", "하네",
                            "해요", "이것", "니들", "하기", "하지", "한거", "해주",
                            "그것", "어디", "여기", "까지", "이거", "하신", "만큼"))

# 불용어 목록 저장하기
library(readr)
write_csv(stopword, "stopword.csv")

# 불용어 목록 불러오기
stopword <- read_csv("stopword.csv")

# 불용어 제거하기
count_word <- count_word %>%
  filter(!word %in% stopword$word)


# -------------------------------------------------------------------------
count_word <- count_word %>%
  anti_join(stopword, by = "word")


# -------------------------------------------------------------------------
# 문서별 단어 빈도 구하기
count_word_doc <- count_word %>%
  count(id, word, sort = T)

count_word_doc


# -------------------------------------------------------------------------
install.packages("tm")

# DTM 만들기
dtm_comment <- count_word_doc %>%
  cast_dtm(document = id, term = word, value = n)

dtm_comment


# -------------------------------------------------------------------------
as.matrix(dtm_comment)[1:8, 1:8]


# -------------------------------------------------------------------------
install.packages("topicmodels")
library(topicmodels)

# 토픽 모델 만들기
lda_model <- LDA(dtm_comment,
                 k = 8,
                 method = "Gibbs",
                 control = list(seed = 1234))
lda_model

# 모델 내용 확인
glimpse(lda_model)


# 06-3 --------------------------------------------------------------------

term_topic <- tidy(lda_model, matrix = "beta")
term_topic


# -------------------------------------------------------------------------
# 토픽별 단어 수
term_topic %>%
  count(topic)

# 토픽 1의 beta 합계
term_topic %>%
  filter(topic == 1) %>%
  summarise(sum_beta = sum(beta))

# -------------------------------------------------------------------------
term_topic %>%
  filter(term == "작품상")


# -------------------------------------------------------------------------
term_topic %>%
  filter(topic == 6) %>%
  arrange(-beta)


# -------------------------------------------------------------------------
terms(lda_model, 20) %>%
  data.frame()


# -------------------------------------------------------------------------
# 토픽별 beta 상위 10개 단어 추출
top_term_topic <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 10)


# -------------------------------------------------------------------------
install.packages("scales")
library(scales)
library(ggplot2)

ggplot(top_term_topic,
       aes(x = reorder_within(term, beta, topic),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free", ncol = 4) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(n.breaks = 4,
                     labels = number_format(accuracy = .01)) + 
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))


# 06-4 --------------------------------------------------------------------

doc_topic <- tidy(lda_model, matrix = "gamma")
doc_topic


# -------------------------------------------------------------------------
doc_topic %>%
  count(topic)

# 문서 1의 gamma 합계
doc_topic %>%
  filter(document == 1) %>%
  summarise(sum_gamma = sum(gamma))


# -------------------------------------------------------------------------
# 문서별로 확률이 가장 높은 토픽 추출
doc_class <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)

doc_class


# -------------------------------------------------------------------------
# integer로 변환
doc_class$document <- as.integer(doc_class$document)

# 원문에 토픽 번호 부여
news_comment_topic <- raw_news_comment %>%
  left_join(doc_class, by = c("id" = "document"))


# -------------------------------------------------------------------------
# 결합 확인
news_comment_topic %>%
  select(id, topic)


# -------------------------------------------------------------------------
news_comment_topic %>%
  count(topic)


# -------------------------------------------------------------------------
news_comment_topic <- news_comment_topic %>%
  na.omit()


# -------------------------------------------------------------------------
doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  count(document) %>%
  filter(n >= 2)


# -------------------------------------------------------------------------
set.seed(1234)
doc_class_unique <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  slice_sample(n = 1)

doc_class_unique


# -------------------------------------------------------------------------
# 문서 빈도 구하기
doc_class_unique %>%
  count(document, sort = T)


# -------------------------------------------------------------------------
top_terms <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 6, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))

top_terms


# -------------------------------------------------------------------------
count_topic <- news_comment_topic %>%
  count(topic)

count_topic


# -------------------------------------------------------------------------
count_topic_word <- count_topic %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic_name = paste("Topic", topic))

count_topic_word


# -------------------------------------------------------------------------
ggplot(count_topic_word,
       aes(x = reorder(topic_name, n),
           y = n,
           fill = topic_name)) +
  geom_col(show.legend = F) +
  coord_flip() +

  geom_text(aes(label = n) ,                # 문서 빈도 표시
            hjust = -0.2) +                 # 막대 밖에 표시

  geom_text(aes(label = term),              # 주요 단어 표시
            hjust = 1.03,                   # 막대 안에 표시
            col = "white",                  # 색깔
            fontface = "bold",              # 두껍게
            family = "nanumgothic") +       # 폰트

  scale_y_continuous(expand = c(0, 0),      # y축-막대 간격 줄이기
                     limits = c(0, 820)) +  # y축 범위
  labs(x = NULL)


# 06-5 --------------------------------------------------------------------

comment_topic <- news_comment_topic %>%
  mutate(reply = str_squish(replace_html(reply))) %>%
  arrange(-gamma)


# -------------------------------------------------------------------------
comment_topic %>%
  select(gamma, reply)


# -------------------------------------------------------------------------
# 토픽 1 내용 살펴보기
comment_topic %>%
  filter(topic == 1 & str_detect(reply, "작품")) %>%
  head(50) %>%
  pull(reply)

comment_topic %>%
  filter(topic == 1 & str_detect(reply, "진심")) %>%
  head(50) %>%
  pull(reply)

comment_topic %>%
  filter(topic == 1 & str_detect(reply, "정치")) %>%
  head(50) %>%
  pull(reply)


# -------------------------------------------------------------------------
# 토픽 이름 목록 만들기
name_topic <- tibble(topic = 1:8,
                     name = c("1. 작품상 수상 축하, 정치적 댓글 비판",
                              "2. 수상 축하, 시상식 감상",
                              "3. 조국 가족, 정치적 해석",
                              "4. 새 역사 쓴 세계적인 영화",
                              "5. 자랑스럽고 감사한 마음",
                              "6. 놀라운 4관왕 수상",
                              "7. 문화계 블랙리스트, 보수 정당 비판",
                              "8. 한국의 세계적 위상"))


# -------------------------------------------------------------------------
# 토픽 이름 결합하기
top_term_topic_name <- top_term_topic %>%
  left_join(name_topic, name_topic, by = "topic")

top_term_topic_name


# -------------------------------------------------------------------------
# 막대 그래프 만들기
ggplot(top_term_topic_name,
       aes(x = reorder_within(term, beta, name),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ name, scales = "free", ncol = 2) +
  coord_flip() +
  scale_x_reordered() +

  labs(title = "영화 기생충 아카데미상 수상 기사 댓글 토픽",
       subtitle = "토픽별 주요 단어 Top 10",
       x = NULL, y = NULL) +

  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        title = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


# 06-6 --------------------------------------------------------------------

install.packages("ldatuning")
library(ldatuning)

models <- FindTopicsNumber(dtm = dtm_comment,
                           topics = 2:20,
                           return_models = T,
                           control = list(seed = 1234))

models %>%
  select(topics, Griffiths2004)


# -------------------------------------------------------------------------
FindTopicsNumber_plot(models)


# -------------------------------------------------------------------------
# 토픽 수가 8개인 모델 추출하기
optimal_model <- models %>%
  filter(topics == 8) %>%
  pull(LDA_model) %>%              # 모델 추출
  .[[1]]                           # list 추출


# -------------------------------------------------------------------------
# optimal_model
tidy(optimal_model, matrix = "beta")


# -------------------------------------------------------------------------
# lda_model
tidy(lda_model, matrix = "beta")

