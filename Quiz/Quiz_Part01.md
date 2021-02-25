Quiz Part 1
===========

#### `speech_park.txt`에는 박근혜 전 대통령의 대선 출마 선언문이 들어있습니다. `speech_park.txt`를 이용해 문제를 해결해 보세요.

[Q1. `speech_park.txt`를 불러와 분석에 적합하게 전처리한 다음 띄어쓰기
기준으로 토큰화하세요.](#Q1)

[Q2. 가장 자주 사용된 단어 20개를 추출하세요.](#Q2)

[Q3. 가장 자주 사용된 단어 20개의 빈도를 나타낸 막대 그래프를 만드세요.
그래프의 폰트는 나눔고딕으로 설정하세요.](#Q3)

------------------------------------------------------------------------

#### Q1. `speech_park.txt`를 불러와 분석에 적합하게 전처리한 다음 띄어쓰기 기준으로 토큰화하세요.<a name="Q1"></a>

##### 전처리

``` r
raw_park <- readLines("speech_park.txt", encoding = "UTF-8")
```

``` r
library(dplyr)
library(stringr)
park <- raw_park %>%
  str_replace_all("[^가-힣]", " ") %>%  # 한글만 남기기
  str_squish() %>%                      # 연속된 공백 제거
  as_tibble()                           # tibble로 변환

park
```

    ## # A tibble: 96 x 1
    ##    value                                                                        
    ##    <chr>                                                                        
    ##  1 "존경하는 국민 여러분 저는 오늘 국민 한 분 한 분의 꿈이 이루어지는 행복한 대한민국을 만들기 위해 저의 모든 것을 바치겠다는 각오로 ~
    ##  2 ""                                                                           
    ##  3 "국민 여러분 저의 삶은 대한민국과 함께 해온 시간이었습니다 우리나라가 가난을 이기고 꿈을 이뤄가는 위대한 과정을 어린 시절부터 가슴깊이~
    ##  4 ""                                                                           
    ##  5 "어머니가 흉탄에 돌아가신 후 견딜 수 없는 고통과 어려움 속에서도 그 힘든 시간을 이겨낼 수 있었던 것은 어머니의 빈자리에 대한 책임감~
    ##  6 ""                                                                           
    ##  7 "그때부터 제 삶은 완전히 다른 길을 가야했습니다 개인의 삶 대신 국민과 함께 하는 공적인 삶이 시작되었습니다 각계각층의 국민들을 만나고~
    ##  8 ""                                                                           
    ##  9 "아버지를 잃는 또 다른 고통과 아픔을 겪고 저는 평범한 삶을 살고자 했습니다 하지만 국민들의 땀과 눈물로 이룩해 온 나라가 외환위기를 ~
    ## 10 ""                                                                           
    ## # ... with 86 more rows

##### 토큰화

``` r
library(tidytext)
word_space <- park %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")        # 띄어쓰기 기준

word_space
```

    ## # A tibble: 1,414 x 1
    ##    word    
    ##    <chr>   
    ##  1 존경하는
    ##  2 국민    
    ##  3 여러분  
    ##  4 저는    
    ##  5 오늘    
    ##  6 국민    
    ##  7 한      
    ##  8 분      
    ##  9 한      
    ## 10 분의    
    ## # ... with 1,404 more rows

------------------------------------------------------------------------

#### Q2. 가장 자주 사용된 단어 20개를 추출하세요.<a name="Q2"></a>

``` r
top20 <- word_space %>%
  count(word, sort = T) %>%
  filter(str_count(word) > 1) %>%
  head(20)

top20
```

    ## # A tibble: 20 x 2
    ##    word             n
    ##    <chr>        <int>
    ##  1 국민            29
    ##  2 저는            14
    ##  3 있습니다        12
    ##  4 함께            12
    ##  5 꿈을            10
    ##  6 것입니다         8
    ##  7 새로운           8
    ##  8 있는             8
    ##  9 국민행복의       7
    ## 10 길을             7
    ## 11 것이             6
    ## 12 국민들의         6
    ## 13 만들겠습니다     6
    ## 14 박근혜           6
    ## 15 아니라           6
    ## 16 여러분의         6
    ## 17 우리             6
    ## 18 있도록           6
    ## 19 통해             6
    ## 20 대한             5

------------------------------------------------------------------------

#### Q3. 가장 자주 사용된 단어 20개의 빈도를 나타낸 막대 그래프를 만드세요. 그래프의 폰트는 나눔고딕으로 설정하세요.<a name="Q3"></a>

##### 폰트 설정

``` r
library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()
```

##### 막대 그래프 만들기

``` r
library(ggplot2)
ggplot(top20, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip () +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))
```

![](Quiz_Part01_files/figure-markdown_github/unnamed-chunk-8-1.png)
