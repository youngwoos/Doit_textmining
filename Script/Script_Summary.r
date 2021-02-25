# Part 01 -----------------------------------------------------------------

# 전처리
moon <- raw_moon %>%
  str_replace_all("[^가-힣]", " ") %>%
  str_squish() %>%
  as_tibble()

# 토큰화
word_space <- moon %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")

# 단어 빈도 구하기
word_space <- word_space %>%
  count(word, sort = T) %>%
  filter(str_count(word) > 1)

# 자주 사용된 단어 추출
top20 <- word_space %>%
  head(20)


# Part 02 -----------------------------------------------------------------

## 1. 명사 추출하기

# 명사 기준 토큰화
word_noun <- moon %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)


# -------------------------------------------------------------------------

## 2. 특정 단어가 사용된 문장 살펴보기

# 문장 기준 토큰화
sentences_moon <- raw_moon %>%
  unnest_tokens(input = value,
                output = sentence,
                token = "sentences")

# 특정 단어가 사용된 문장 추출
sentences_moon %>%
  filter(str_detect(sentence, "국민"))



# Part 03 -----------------------------------------------------------------

## 1. 단어 빈도 비교하기

# 토큰화
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

# 하위 집단별 단어 빈도 구하기
frequency <- speeches %>%
  count(president, word) %>%
  filter(str_count(word) > 1)

# 가장 많이 사용된 단어 추출
top10 <- frequency %>%
  group_by(president) %>%
  slice_max(n, n = 10, with_ties = F)


# -------------------------------------------------------------------------

## 2. 로그 오즈비로 단어 비교하기

# long form을 wide form으로 변환
frequency_wide <- frequency %>%
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))

# 로그 오즈비 구하기
frequency_wide <- frequency_wide %>%
  mutate(log_odds_ratio = log(((moon + 1) / (sum(moon + 1))) /
                              ((park + 1) / (sum(park + 1)))))

# 상대적으로 중요한 단어 추출
top10 <- frequency_wide %>%
  group_by(president = ifelse(log_odds_ratio > 0, "moon", "park")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

# -------------------------------------------------------------------------

## 3. TF-IDF로 단어 비교하기

# TF-IDF 구하기
frequecy <- frequecy %>%
  bind_tf_idf(term = word,
              document = president,
              n = n) %>%
  arrange(-tf_idf)

# 상대적으로 중요한 단어 추출
top10 <- frequecy %>%
  arrange(tf_idf) %>%
  group_by(president) %>%
  slice_max(tf_idf, n = 10, with_ties = F)


# Part 04 -----------------------------------------------------------------

## 1. 자주 사용된 감정 단어 살펴보기

# 단어에 감정 점수 부여
word_comment <- word_comment %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

# 감정 분류
word_comment <- word_comment %>%
  mutate(sentiment = ifelse(polarity ==  2, "pos",
                     ifelse(polarity == -2, "neg", "neu")))

# 자주 사용된 감정 단어 추출
top10_sentiment <- word_comment %>%
  filter(sentiment != "neu") %>%
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10)


# -------------------------------------------------------------------------

## 2. 텍스트의 감정 점수 구하기

# 텍스트별로 단어의 감정 점수 합산
score_comment <- word_comment %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()


# -------------------------------------------------------------------------

## 3. 감정 범주별 주요 단어 살펴보기

# 감정 범주 변수 생성
score_comment <- score_comment %>%
  mutate(sentiment = ifelse(score >=  1, "pos",
                     ifelse(score <= -1, "neg", "neu")))

# 토큰화 및 전처리
comment <- score_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F) %>%
  filter(str_detect(word, "[가-힣]") &
         str_count(word) >= 2)

# 감정 범주별 단어 빈도 구하기
frequency_word <- comment %>%
  count(sentiment, word, sort = T)

# 로그 오즈비 구하기
comment_wide <- frequency_word %>%
  filter(sentiment != "neu") %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))

comment_wide <- comment_wide %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                              ((neg + 1) / (sum(neg + 1)))))

# 긍정, 부정 텍스트에 상대적으로 자주 사용된 단어 추출
top10 <- comment_wide %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10)


# Part 05 -----------------------------------------------------------------

## 1. 동시 출현 단어 분석 - Co-occurrence analysis

# 품사 기준 토큰화
comment_pos <- news_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = SimplePos22,
                drop = F)


# 명사, 동사, 형용사 추출
comment <- comment_pos %>%
  separate_rows(word, sep = "[+]") %>%
  filter(str_detect(word, "/n|/pv|/pa")) %>%
  mutate(word = ifelse(str_detect(word, "/pv|/pa"),
                       str_replace(word, "/.*$", "다"),
                       str_remove(word, "/.*$"))) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)


# 단어 동시 출현 빈도 구하기
pair <- comment %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = T)


# -------------------------------------------------------------------------

## 2. 단어 간 상관 분석 - Phi coefficient

# 파이 계수 구하기
word_cors <- comment %>%
  add_count(word) %>%
  filter(n >= 20) %>%
  pairwise_cor(item = word,
               feature = id,
               sort = T)


# -------------------------------------------------------------------------

## 3. 연이어 사용된 단어쌍 분석 - n-gram

# 텍스트를 한 행으로 구성
line_comment <- comment %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " "))

# 바이그램 토큰화
bigram_comment <- line_comment %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n = 2)

# 바이그램 분리
bigram_seprated <- bigram_comment %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# 단어쌍 빈도 구하기
pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit()


# -------------------------------------------------------------------------

## 4. 네트워크 그래프 만들기

# 네트워크 그래프 데이터 만들기
set.seed(1234)
graph_comment <- pair_bigram %>%
  filter(n >= 8) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

# 네트워크 그래프 만들기
set.seed(1234)
ggraph(graph_comment) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality,
                      color = group)) +
  geom_node_text(aes(label = name))


# Part 06 -----------------------------------------------------------------

## 1. LDA 모델 만들기

# 문서별 단어 빈도 구하기
count_word_doc <- count_word %>%
  count(id, word, sort = T)

# DTM 만들기
dtm_comment <- count_word_doc %>%
  cast_dtm(document = id, term = word, value = n)

# LDA 모델 만들기
lda_model <- LDA(dtm_comment,
                 k = 8,
                 method = "Gibbs",
                 control = list(seed = 1234))


# -------------------------------------------------------------------------

## 2. 토픽별 주요 단어 살펴보기

# beta 추출
term_topic <- tidy(lda_model, matrix = "beta")

# 토픽별 beta 상위 단어 추출
top_term_topic <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 10)


# -------------------------------------------------------------------------

## 3. 문서를 토픽별로 분류하기

# gamma 추출
doc_topic <- tidy(lda_model, matrix = "gamma")

# 문서별로 확률이 가장 높은 토픽 추출
doc_class <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)

# 변수 타입 통일
doc_class$document <- as.integer(doc_class$document)

# 문서에 확률이 가장 높은 토픽 번호 부여
news_comment_topic <- raw_news_comment %>%
  left_join(doc_class, by = c("id" = "document"))


# -------------------------------------------------------------------------

## 4. 토픽별 주요 문서 살펴보기

# 특정 토픽에서 gamma가 높은 문서 추출
news_comment_topic %>%
  filter(topic == 1) %>%
  arrange(-gamma) %>%
  select(reply)

