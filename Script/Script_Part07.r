# 07-1 --------------------------------------------------------------------

# 데이터 불러오기
library(readr)
library(dplyr)

raw_tada <- read_csv("news_comment_tada.csv") %>%
  mutate(id = row_number())

glimpse(raw_tada)


# -------------------------------------------------------------------------
library(stringr)
library(textclean)

tada <- raw_tada %>%
  filter(str_count(reply, " ") >= 1) %>%                   # 띄어쓰기 1개 이상 추출
  mutate(reply_raw = str_squish(replace_html(reply)),      # 원문 보유
         reply = str_replace_all(reply, "[^가-힣]", " "),  # 한글만 남기기
         reply = str_squish(reply))                        # 중복 공백 제거


# -------------------------------------------------------------------------
library(tidytext)
library(KoNLP)

word_noun <- tada %>%
  unnest_tokens(input = reply,
                output = word,
                token = extractNoun,
                drop = F)


# -------------------------------------------------------------------------
# 단어 빈도 구하기
frequency <- word_noun %>%
  count(word, sort = T) %>%    # 단어 빈도 구해 내림차순 정렬
  filter(str_count(word) > 1)  # 두 글자 이상만 남기기

# 상위 단어 추출
frequency %>%
  head(30) %>%
  print(n = Inf)


# -------------------------------------------------------------------------
# 불용어 목록 생성
stopword_noun <- c("들이", "하면", "하게", "해서")

# 주요 단어 목록 만들기
top20_noun <- frequency %>%
  filter(!word %in% stopword_noun) %>%
  head(20)


# -------------------------------------------------------------------------
top20_noun %>%
  print(n = Inf)


# -------------------------------------------------------------------------
install.packages("scales")
library(scales)
library(ggplot2)


# -------------------------------------------------------------------------
ggplot(top20_noun, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = comma(n, accuracy = 1)), hjust = -0.3) +  
  scale_y_continuous(limits = c(0, 3200)) +

  labs(title = "타다 금지법 기사 댓글 주요 단어",
       subtitle = "언급 빈도 Top 20",
       x = NULL) +

  theme_minimal() +
  theme(text = element_text(family = "nanumgothic", size = 12),
        plot.title = element_text(size = 14, face = "bold"),      # 제목 폰트
        plot.subtitle = element_text(size = 13))                  # 부제목 폰트


# 07-2 --------------------------------------------------------------------

word_sympathy <- word_noun %>%
  rename(like = sympathyCount,
         dislike = antipathyCount) %>%

  mutate(diff = like - dislike,
         sympathy = ifelse(diff >=  1, "like",
                    ifelse(diff <= -1, "dislike", "neutral")))


# -------------------------------------------------------------------------
# 공감 여부별 댓글 수
word_sympathy %>%
  distinct(id, .keep_all = T) %>%
  count(sympathy, sort = T)


# -------------------------------------------------------------------------
# 단어 빈도 구하기
frequency_sympathy <- word_sympathy %>%
  count(sympathy, word) %>%              # 공감 여부 및 단어별 빈도
  filter(str_count(word) > 1 &           # 두 글자 이상 추출
         sympathy != "centrism")         # centrism 제거

# Wide form으로 변환하기
library(tidyr)
frequency_wide <- frequency_sympathy %>%
  pivot_wider(names_from = sympathy,
              values_from = n,
              values_fill = list(n = 0))

# 로그 오즈비 구하기
frequency_wide <- frequency_wide %>%
  mutate(log_odds_ratio = log(((like    + 1) / (sum(like    + 1))) /
                              ((dislike + 1) / (sum(dislike + 1)))))

frequency_wide %>%
  arrange(-log_odds_ratio)


# -------------------------------------------------------------------------
top10_odds <- frequency_wide %>%
  filter(like >= 20 | dislike >= 20) %>%
  group_by(sympathy = ifelse(log_odds_ratio > 0, "like", "dislike")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

top10_odds %>%
  arrange(log_odds_ratio)


# -------------------------------------------------------------------------
# 막대 색깔 목록 생성
col_sentiment <- c("#619CFF", "#F8766D")

# 막대 순서 지정
top10_odds$sympathy <- factor(top10_odds$sympathy,
                              levels = c("like", "dislike"))

ggplot(top10_odds, aes(x = reorder(word, log_odds_ratio),
                       y = log_odds_ratio,
                       fill = sympathy)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = col_sentiment,          # 막대 색깔
                    labels = c("공감", "비공감")) +  # 범례 순서

  labs(title = "타다 금지법 기사 댓글 주요 단어",
       subtitle = "공감 vs 비공감 로그 오즈비 Top 10",
       x = NULL, fill = NULL) +

  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))


# -------------------------------------------------------------------------
tada %>%
  filter(str_detect(reply_raw, "조합")) %>%
  head(3) %>%
  pull(reply)


# -------------------------------------------------------------------------
# 스타일 함수 만들기
library(crayon)
font <- combine_styles(make_style("ivory"),
                       make_style("deeppink", bg = TRUE),
                       make_style("bold"))
font

font("폰트를 적용해 출력") %>% cat()


# -------------------------------------------------------------------------
# 관심 단어 설정
keyword <- "조합"

# 댓글 추출해 스타일 적용
tada %>%
  filter(str_detect(reply_raw, keyword)) %>%
  head(3) %>%
  mutate(reply = paste0(str_replace_all(reply,
                                        keyword,
                                        font(keyword)))) %>%  # 스타일 적용
  pull(reply) %>%                                             # reply 추출
  cat(sep = "\n\n")                                           # 줄바꿈 출력


# -------------------------------------------------------------------------
find_word <- function(df, x, keyword, n = 6) {

  # 스타일 함수 설정
  font <- combine_styles(make_style("ivory"),
                         make_style("deeppink", bg = TRUE),
                         make_style("bold"))

  # 키워드 추출해 스타일 적용
  df %>%
    filter(str_detect({{x}}, keyword)) %>%                  # 키워드 추출
    head(n) %>%                                             # n행 추출
    mutate(x = paste0("[", row_number(), "] ", {{x}}),      # 행번호 삽입
           x = paste0(str_replace_all(x,
                                      keyword,
                                      font(keyword)))) %>%  # 스타일 적용
    pull(x) %>%                                             # 텍스트 추출
    cat(sep = "\n\n")                                       # 줄바꿈 출력
}


# -------------------------------------------------------------------------
tada %>% find_word(x = reply_raw, keyword = "조합", n = 2)

tada %>% find_word(reply_raw, "조합", 2)


# -------------------------------------------------------------------------
tada <- tada %>%
  left_join(word_sympathy %>%
              distinct(id, .keep_all = T) %>%  # 중복 댓글 제거
              select(id, sympathy, diff),      # 주요 변수 추출
            by = "id")


# -------------------------------------------------------------------------
# 공감 댓글 추출
reply_like <- tada %>%
  filter(sympathy == "like") %>%     # like 추출
  arrange(-diff)                     # 공감 높은순 정렬

# 비공감 댓글 추출
reply_dislike <- tada %>%
  filter(sympathy == "dislike") %>%  # dislike 추출
  arrange(diff)                      # 비공감 높은순 정렬


# -------------------------------------------------------------------------
# 조합
reply_like %>% find_word(x = reply_raw, keyword = "조합", n = 10)

# 소비자
reply_like %>% find_word(x = reply_raw, keyword = "소비자", n = 10)

# 동남아
reply_like %>% find_word(x = reply_raw, keyword = "동남아", n = 10)

# 렌트카
reply_dislike %>% find_word(x = reply_raw, keyword = "렌트카", n = 10)


# -------------------------------------------------------------------------
# "한국당" 언급 댓글 제거 후 "한국" 언급한 댓글 추출
reply_dislike %>%
  filter(!str_detect(reply, "한국당")) %>%
  find_word(x = reply, keyword = "한국", n = 10)


# -------------------------------------------------------------------------
# 댓글
reply_dislike %>% find_word(x = reply, keyword = "댓글", n = 10)


# 07-3 --------------------------------------------------------------------

# 단어 목록 생성
category1 <- "택시 업계|택시업계|조합"
category2 <- "정부"
category3 <- "국회의원|한국당|자유한국당|자한당|자한|민주당|더불어민주당"

# 추출 및 결합
bind_category <- bind_rows(
  word_sympathy %>%
    filter(str_detect(reply, category1)) %>%
    mutate(category = "택시업계"),

  word_sympathy %>%
    filter(str_detect(reply, category2)) %>%
    mutate(category = "정부"),

  word_sympathy %>%
    filter(str_detect(reply, category3)) %>%
    mutate(category = "국회의원"))


# -------------------------------------------------------------------------
# 카테고리별 빈도
bind_category %>%
  group_by(id) %>%
  distinct(category, .keep_all = T) %>%
  ungroup() %>%
  count(category)


# -------------------------------------------------------------------------
# 불용어 목록 생성
stopword_category <- c("택시 업계", "택시업계", "업계", "조합",
                       "정부", "국회의원", "한국당", "자유한국당",
                       "자한당", "자한", "민주당", "더불어민주당")


# -------------------------------------------------------------------------
# 카테고리별 단어 빈도 구하기
frequency_category <- bind_category %>%
  filter(!word %in% stopword_category) %>%  # 불용어 제외

  group_by(id) %>%                          # 댓글별 분리
  distinct(word, .keep_all = T) %>%         # 댓글 내 중복 단어 제거
  ungroup() %>%                             # 그룹 해제

  count(category, word, sort = T) %>%       # 카테고리별 단어 빈도
  filter(str_count(word) >= 2)              # 2글자 이상 추출


# -------------------------------------------------------------------------
# tf-idf 구하기
tfidf_category <- frequency_category %>%
  bind_tf_idf(term = word,
              document = category,
              n = n) %>%
  arrange(-tf_idf)

tfidf_category


# -------------------------------------------------------------------------
# 주요 단어 추출, 불용어 확인
tfidf_category %>%
  group_by(category) %>%
  slice_max(tf_idf, n = 15, with_ties = F) %>%
  print(n = Inf)


# -------------------------------------------------------------------------
# 불용어 목록 생성
stopword_tfidf <- c("국회의윈님하고", "현정부", "에휴")


# -------------------------------------------------------------------------
# 주요 단어 추출
top10 <- tfidf_category %>%
  filter(!word %in% stopword_tfidf) %>%
  group_by(category) %>%
  slice_max(tf_idf, n = 10, with_ties = F)


# -------------------------------------------------------------------------
# 그래프 순서 정하기
top10$category <- factor(top10$category,
                         levels = c("택시업계", "정부", "국회의원"))

# 막대 그래프 만들기
ggplot(top10, aes(x = reorder_within(word, tf_idf, category),
                  y = tf_idf,
                  fill = category)) +
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~ category, scales = "free", ncol = 3) +
  scale_x_reordered() +
  scale_y_continuous(n.breaks = 5,
                     labels = number_format(accuracy = .001)) +

  labs(title = "타다 금지법 기사 댓글 주요 단어",
       subtitle = "카테고리별 TF-IDF Top 10",
       x = NULL) +

  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(size = 11))  # 카테고리명 폰트


# -------------------------------------------------------------------------
# 중복 댓글 제거
reply_category <- bind_category %>%
  group_by(category) %>%
  distinct(id, .keep_all = T)


# -------------------------------------------------------------------------
# 택시업계 카테고리
reply_category %>%
  filter(category == "택시업계") %>%
  find_word(x = reply_raw, keyword = "대기업")

# 정부 카테고리
reply_category %>%
  filter(category == "정부") %>%
  find_word(x = reply_raw, keyword = "지원")

# 국회의원 카테고리
reply_category %>%
  filter(category == "국회의원") %>%
  find_word(x = reply_raw, keyword = "박홍근")


# -------------------------------------------------------------------------
tada %>%
  find_word(x = reply_raw, keyword = "깨구락지")

tada %>%
  filter(str_detect(reply, "깨구락지")) %>%
  select(press, reply_raw)


# 07-4 --------------------------------------------------------------------

# 토큰화
pos_tada <- tada %>%
  unnest_tokens(input = reply,
                output = word_pos,
                token = SimplePos22,
                drop = F)

# 품사 태그 정리
separate_pos_tada <- pos_tada %>%
  separate_rows(word_pos, sep = "[+]") %>%                   # 품사 태그 분리
  filter(str_detect(word_pos, "/n|/pv|/pa")) %>%             # 품사 추출
  mutate(word = ifelse(str_detect(word_pos, "/pv|/pa"),      # "/pv", "/pa" 추출
                       str_replace(word_pos, "/.*$", "다"),  # "~다"로 바꾸기
                       str_remove(word_pos, "/.*$"))) %>%    # 태그 제거
  filter(str_count(word) >= 2) %>%                           # 2글자 이상 추출
  arrange(id)

separate_pos_tada %>%
  select(word)


# -------------------------------------------------------------------------
library(widyr)
word_cors <- separate_pos_tada %>%
  add_count(word) %>%
  filter(n >= 20) %>%
  pairwise_cor(item = word, feature = id, sort = T)

word_cors


# -------------------------------------------------------------------------
target <- c("타다", "정부", "택시")

# 상위 10개 추출
top_cors <- word_cors %>%
  filter(item1 %in% target) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 10)

top_cors


# -------------------------------------------------------------------------
# 그래프 순서 정하기
top_cors$item1 <- factor(top_cors$item1, levels = target)

# 막대 그래프 만들기
ggplot(top_cors, aes(x = reorder_within(item2, correlation, item1),
                     y = correlation,
                     fill = item1)) +
  geom_col(show.legend = F) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  scale_x_reordered() +

  labs(title = "타다 금지법 기사 댓글 주요 단어",
       subtitle = "파이 계수 Top 10",
       x = NULL) +

  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(size = 11))


# -------------------------------------------------------------------------
# 네트워크 그래프 데이터 만들기
library(tidygraph)
set.seed(1234)
graph_cors <- word_cors %>%
  filter(correlation >= 0.15) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),       # 중심성
         group = as.factor(group_infomap()))     # 커뮤니티

# 네트워크 그래프 만들기
library(ggraph)
set.seed(1234)
ggraph(graph_cors, layout = "fr") +              # 레이아웃

  geom_edge_link(color = "gray50",               # 엣지 색깔
                 aes(edge_alpha = correlation,   # 엣지 명암
                     edge_width = correlation),  # 엣지 두께
                 show.legend = F) +              # 범례 삭제
  scale_edge_width(range = c(1, 4)) +            # 엣지 두께 범위

  geom_node_point(aes(size = centrality,         # 노드 크기
                      color = group),            # 노드 색깔
                  show.legend = F) +             # 범례 삭제
  scale_size(range = c(5, 10)) +                 # 노드 크기 범위

  geom_node_text(aes(label = name),              # 텍스트 표시
                 repel = T,                      # 노드밖 표시
                 size = 5,                       # 텍스트 크기
                 family = "nanumgothic") +       # 폰트

  theme_graph()                                  # 배경 삭제


# -------------------------------------------------------------------------
tada %>%
  filter(str_detect(reply_raw, "선거")) %>%
  find_word(x = reply_raw, keyword = "내년", n = 10)

tada %>%
  filter(str_detect(reply_raw, "내년")) %>%
  find_word(x = reply_raw, keyword = "총선", n = 10)

tada %>%
  filter(str_detect(reply_raw, "목적지")) %>%
  find_word(x = reply_raw, keyword = "손님", n = 10)

tada %>%
  filter(str_detect(reply_raw, "손님")) %>%
  find_word(x = reply_raw, keyword = "골라", n = 10)


# -------------------------------------------------------------------------

separate_pos_tada %>%
  filter(word == "고르다") %>%
  pull(reply_raw)


# -------------------------------------------------------------------------
# 한 댓글이 하나의 행을 구성하도록 결합
line_comment <- separate_pos_tada %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " "))

# 바이그램으로 토큰화
bigram_comment <- line_comment %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n = 2)

bigram_comment


# -------------------------------------------------------------------------
# 바이그램 분리하기
bigram_seprated <- bigram_comment %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# 단어쌍 빈도 구하기
pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit()

pair_bigram


# -------------------------------------------------------------------------
# 네트워크 그래프 데이터 만들기
set.seed(1234)
graph_bigram <- pair_bigram %>%
  filter(n >= 8) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),       # 중심성
         group = as.factor(group_infomap()))     # 커뮤니티

# 네트워크 그래프 만들기
set.seed(1234)
ggraph(graph_bigram, layout = "fr") +            # 레이아웃

  geom_edge_link(color = "gray50",               # 엣지 색깔
                 alpha = 0.5) +                  # 엣지 명암

  geom_node_point(aes(size = centrality,         # 노드 크기
                      color = group),            # 노드 색깔
                  show.legend = F) +             # 범례 삭제
  scale_size(range = c(5, 15)) +                 # 노드 크기 범위

  geom_node_text(aes(label = name),              # 텍스트 표시
                 repel = T,                      # 노드밖 표시
                 size = 5,                       # 텍스트 크기
                 family = "nanumgothic") +       # 폰트

  theme_graph()                                  # 배경 삭제


# -------------------------------------------------------------------------
line_tada <- line_comment %>%
  left_join(tada, by = "id")

line_tada %>%
  select(sentence)


# -------------------------------------------------------------------------
line_tada %>%
  filter(str_detect(sentence, "시대 역행")) %>%
  find_word(x = reply_raw, keyword = "역행", n = 10)

line_tada %>%
  filter(str_detect(sentence, "시대 뒤떨어지다")) %>%
  find_word(x = reply_raw, keyword = "뒤떨어", n = 10)

line_tada %>%
  filter(str_detect(sentence, "택시 면허")) %>%
  find_word(x = reply_raw, keyword = "면허", n = 10)

line_tada %>%
  filter(str_detect(sentence, "면허 사다")) %>%
  find_word(x = reply_raw, keyword = "사서", n = 10)


# 07-5 --------------------------------------------------------------------

# 명사 추출
noun_tada <- tada %>%
  distinct(reply, .keep_all = T) %>%                   # 중복 댓글 제거
  filter(str_count(reply, boundary("word")) >= 3) %>%  # 짧은 댓글 제거
  unnest_tokens(input = reply,                         # 명사 추출
                output = word,
                token = extractNoun,
                drop = F) %>%
  filter(str_count(word) > 1)

# 중복, 고빈도 단어 제거
unique_noun_tada <- noun_tada %>%
  group_by(id) %>%                                     # 중복 단어 제거
  distinct(word, .keep_all = T) %>%
  ungroup() %>%
  add_count(word) %>%                                  # 고빈도 단어 제거
  filter(n <= 200) %>%
  select(id, word)

unique_noun_tada


# -------------------------------------------------------------------------
# 문서별 단어 빈도 구하기
count_word <- unique_noun_tada %>%
  count(id, word, sort = T)

# DTM 만들기
dtm_tada <- count_word %>%
  cast_dtm(document = id, term = word, value = n)

dtm_tada


# -------------------------------------------------------------------------
library(ldatuning)
models_tada <- FindTopicsNumber(dtm = dtm_tada,
                                topics = 2:20,
                                return_models = T,
                                control = list(seed = 1234))

# 성능 지표 그래프
FindTopicsNumber_plot(models_tada)


# -------------------------------------------------------------------------
# 토픽 수가 9개인 모델 추출
lda_model <- models_tada %>%
  filter(topics == 9) %>%
  pull(LDA_model) %>%              # 모델 추출
  .[[1]]                           # list 추출

lda_model


# -------------------------------------------------------------------------
# 토픽별 단어 확률 beta 추출
term_topic <- tidy(lda_model, matrix = "beta")


# -------------------------------------------------------------------------
# 토픽별 beta 상위 단어 추출
term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  print(n = Inf)


# -------------------------------------------------------------------------
# 불용어 목록 생성
stopword_lda <- c("하게", "하다", "하려", "해라", "그것", "하면", "하네",
                  "하기", "하나", "해서", "하면", "하지", "한거", "니들")

# 불용어 제외 후 상위 10개 단어 추출
top_term_topic <- term_topic %>%
  filter(!term %in% stopword_lda) %>%
  group_by(topic) %>%
  slice_max(beta, n = 10)

top_term_topic


# -------------------------------------------------------------------------
ggplot(top_term_topic, aes(x = reorder_within(term, beta, topic),
                           y = beta,
                           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))


# -------------------------------------------------------------------------
# 문서별 토픽 확률 gamma 추출하기
doc_topic <- tidy(lda_model, matrix = "gamma")

# 문서별로 확률이 가장 높은 토픽 추출
doc_class <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)

doc_class


# -------------------------------------------------------------------------
# integer로 변환
doc_class$document <- as.integer(doc_class$document)

# 원문에 토픽 번호 부여
tada_topic <- tada %>%
  left_join(doc_class, by = c("id" = "document"))


# -------------------------------------------------------------------------
top_terms <- term_topic %>%
  filter(!term %in% stopword_lda) %>%
  group_by(topic) %>%
  slice_max(beta, n = 6, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))

top_terms


# -------------------------------------------------------------------------
count_topic <- tada_topic %>%
  count(topic) %>%
  na.omit()

count_topic


# -------------------------------------------------------------------------
count_topic_word <- count_topic %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic_name = paste("Topic", topic))

count_topic_word


# -------------------------------------------------------------------------
library(scales)
ggplot(count_topic_word,
       aes(x = reorder(topic_name, n),
           y = n,
           fill = topic_name)) +
  geom_col(show.legend = F) +
  coord_flip() +

  geom_text(aes(label = comma(n, accuracy = 1)),  # 문서 빈도 표시
            hjust = -0.2) +

  geom_text(aes(label = term),                    # 주요 단어 표시
            hjust = 1.03,
            col = "white",
            fontface = "bold",
            family = "nanumgothic") +

  scale_y_continuous(expand = c(0, 0),            # y축-막대 간격 줄이기
                     limits = c(0, 1100)) +       # y축 범위

  labs(title = "타다 금지법 기사 댓글 토픽",
       subtitle = "토픽별 주요 단어 및 댓글 빈도",
       x = NULL, y = NULL) +

  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))


# -------------------------------------------------------------------------
# 토픽별 주요 문서 추출
reply_topic <- tada_topic %>%
  group_by(topic) %>%
  slice_max(gamma, n = 100)


# -------------------------------------------------------------------------
# 토픽 1 내용 살펴보기
reply_topic %>%
  filter(topic == 1) %>%
  pull(reply_raw)

# 토픽 2 내용 살펴보기
reply_topic %>%
  filter(topic == 2) %>%
  pull(reply_raw)


# -------------------------------------------------------------------------
# 토픽 이름 목록 만들기
name_topic <- tibble(topic = 1:9,
                     name = c("1. 신사업 가로막는 국회",
                              "2. 시대 흐름 역행하는 법안",
                              "3. 택시 업계 보호, 국민 무시",
                              "4. 자유 시장경제 반하는 결정",
                              "5. 불만족스러운 택시 서비스",
                              "6. 국가 발전 가로막는 정부",
                              "7. 기존 업계 밥그릇 지키는 정치인",
                              "8. 총선만 신경 쓰는 국회의원",
                              "9. 타다는 렌트카, 무면허 택시 안된다"))


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
  facet_wrap(~ name, scales = "free", ncol = 3) +
  coord_flip() +
  scale_x_reordered() +

  labs(title = "타다 금지법 기사 댓글 토픽",
       subtitle = "토픽별 주요 단어 Top 10",
       x = NULL, y = NULL) +

  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_blank(),   # x축 이름 삭제
        axis.ticks.x = element_blank())  # x축 눈금 삭제

