# 01-1 -------------------------------------------------------------------

raw_moon <- readLines("speech_moon.txt", encoding = "UTF-8")
head(raw_moon)

txt <- "치킨은!! 맛있다. xyz 정말 맛있다!@#"
txt

install.packages("stringr")
library(stringr)

str_replace_all(string = txt, pattern = "[^가-힣]", replacement = " ")


# ------------------------------------------------------------------------
moon <- raw_moon %>%
 str_replace_all("[^가-힣]", " ")

head(moon)


# ------------------------------------------------------------------------
# 파라미터명 입력
str_replace_all(string = txt, pattern = "[^가-힣]", replacement = " ")

# 파라미터명 생략
str_replace_all(txt, "[^가-힣]", " ")


# ------------------------------------------------------------------------
txt <- "치킨은  맛있다   정말 맛있다  "
txt
str_squish(txt)


# ------------------------------------------------------------------------
moon <- moon %>%
 str_squish()

head(moon)


# ------------------------------------------------------------------------
library(dplyr)
moon <- as_tibble(moon)
moon


# ------------------------------------------------------------------------
moon <- raw_moon %>%
 str_replace_all("[^가-힣]", " ") %>%  # 한글만 남기기
 str_squish() %>%                      # 연속된 공백 제거
 as_tibble()                           # tibble로 변환


# ------------------------------------------------------------------------
iris             # data frame 출력
as_tibble(iris)  # tibble 구조로 변환


# 01-2 --------------------------------------------------------------------

text <- tibble(value = "대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.")
text


# ------------------------------------------------------------------------
install.packages("tidytext")
library(tidytext)

# 문장 기준 토큰화
text %>%
  unnest_tokens(input = value,        # 토큰화할 텍스트
                output = word,        # 출력 변수명
                token = "sentences")  # 문장 기준


# ------------------------------------------------------------------------
# 띄어쓰기 기준 토큰화
text %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")      # 띄어쓰기 기준


# ------------------------------------------------------------------------
# 문자 기준 토큰화
text %>%
  unnest_tokens(input = value,
                output = word,
                token = "characters")  # 문자 기준


# ------------------------------------------------------------------------
word_space <- moon %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")
word_space



# 01-3 --------------------------------------------------------------------

word_space <- word_space %>%
 count(word, sort = T)

word_space


# ------------------------------------------------------------------------
str_count("배")
str_count("사과")


# ------------------------------------------------------------------------
# 두 글자 이상만 남기기
word_space <- word_space %>%
 filter(str_count(word) > 1)

word_space


# ------------------------------------------------------------------------
word_space <- word_space %>%
 count(word, sort = T) %>%
 filter(str_count(word) > 1)


# ------------------------------------------------------------------------
top20 <- word_space %>%
 head(20)

top20

# ------------------------------------------------------------------------
install.packages("ggplot2")
library(ggplot2)

ggplot(top20, aes(x = reorder(word, n), y = n)) +  # 단어 빈도순 정렬
 geom_col() +
 coord_flip()                                      # 회전


# ------------------------------------------------------------------------
ggplot(top20, aes(x = reorder(word, n), y = n)) +
 geom_col() +
 coord_flip() +
 geom_text(aes(label = n), hjust = -0.3) +            # 막대 밖 빈도 표시
  
 labs(title = "문재인 대통령 출마 연설문 단어 빈도",  # 그래프 제목
      x = NULL, y = NULL) +                           # 축 이름 삭제
  
 theme(title = element_text(size = 12))               # 제목 크기


# ------------------------------------------------------------------------
install.packages("ggwordcloud")
library(ggwordcloud)

ggplot(word_space, aes(label = word, size = n)) +
 geom_text_wordcloud(seed = 1234) +     
 scale_radius(limits = c(3, NA),     # 최소, 최대 단어 빈도
              range = c(3, 30))      # 최소, 최대 글자 크기


# ------------------------------------------------------------------------
ggplot(word_space, 
       aes(label = word, 
           size = n, 
           col = n)) +                     # 빈도에 따라 색깔 표현
 geom_text_wordcloud(seed = 1234) +  
 scale_radius(limits = c(3, NA),
              range = c(3, 30)) +
 scale_color_gradient(low = "#66aaf2",     # 최소 빈도 색깔
                      high = "#004EA1") +  # 최고 빈도 색깔
 theme_minimal()                           # 배경 없는 테마 적용


# ------------------------------------------------------------------------
install.packages("showtext")
library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()


# ------------------------------------------------------------------------
ggplot(word_space,
       aes(label = word,
           size = n,
           col = n)) +
  geom_text_wordcloud(seed = 1234,
                      family = "nanumgothic") +  # 폰트 적용
  scale_radius(limits = c(3, NA),
               range = c(3, 30)) +
  scale_color_gradient(low = "#66aaf2",
                       high = "#004EA1") +
  theme_minimal()


# ------------------------------------------------------------------------
font_add_google(name = "Black Han Sans", family = "blackhansans")
showtext_auto()

ggplot(word_space,
       aes(label = word,
           size = n,
           col = n)) +
  geom_text_wordcloud(seed = 1234,
                      family = "blackhansans") +  # 폰트 적용
  scale_radius(limits = c(3, NA),
               range = c(3, 30)) +
  scale_color_gradient(low = "#66aaf2",
                       high = "#004EA1") +
  theme_minimal()


# ------------------------------------------------------------------------
font_add_google(name = "Gamja Flower", family = "gamjaflower")
showtext_auto()

ggplot(top20, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  
  labs(title = "문재인 대통령 출마 연설문 단어 빈도",
       x = NULL, y = NULL) +
  
  theme(title = element_text(size = 12),
        text = element_text(family = "gamjaflower"))  # 폰트 적용

