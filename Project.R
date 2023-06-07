
#install.packages('glue')
library('jsonlite')
library(XML)
library(xml2)
library(ggplot2)
library(dplyr)
library(tidyr)

url = ""

valueJson = fromJSON(url)
print(head(valueJson))
dfJson = data.frame(valueJson$data)

# 연령을 10살 단위로 묶어서 합치는 함수
combine_age <- function(age) {
  if (is.character(age) && !is.na(age)) {
    age_range <- strsplit(age, " - ")[[1]]
    age <- as.numeric(age_range[1])
    if (!is.na(age)) {
      group <- floor(age / 10) * 10
      return(group)
    }
  }
  return(NA)
}
# 행정기관별 연령을 10살 기준으로 묶어서 합치기
dfJson$연령대 <- sapply(dfJson$연령.만., combine_age)

# 결과 출력
dfJson

#결과 출력
filteredData <- filter(dfJson, 연령대 %in% c("0", "10"))
filteredData

# 행정기관 열의 고유한 값 추출
unique_values <- unique(filteredData$행정기관)
unique_values

# '행정기관' 별로 '남'과 '여'의 합 계산
summary_df <- filteredData %>%
  group_by(행정기관) %>%
  summarize(남 = sum(남), 여 = sum(여))
summary_df

# 남성과 여성 합산 후 내림차순 정렬
summary_df1 <- summary_df %>%
  mutate(합계 = 남 + 여) %>%
  arrange(desc(합계)) %>%
  head(20)
summary_df1

# 데이터프레임 재구성
df <- summary_df1 %>%
  select(행정기관, 남, 여) %>%
  pivot_longer(cols = c(남, 여), names_to = "성별", values_to = "인구 수")

# 행정기관을 요인(factor) 형식으로 변환
df$행정기관 <- factor(df$행정기관, levels = summary_df1$행정기관)

# 막대 그래프 생성
ggplot(df, aes(x = 행정기관, y = `인구 수`, fill = 성별)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "행정기관", y = "인구 수", title = "행정기관별 인구 수 (남성 vs 여성)") +
  scale_fill_manual(values = c("#3366CC", "#FF6666"), labels = c("남성", "여성")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

#아동센터
senter_url = ""

senter_json = fromJSON(senter_url)
print(head(senter_json))
senter_df = data.frame(senter_json$data)
senter_df

library(stringr)

#동만 추출
filteredData <- senter_df %>%
  mutate(행정주소 = str_extract(주소, "\\(.*[동면]\\)"))

#괄호제거
filteredData <- filteredData %>%
  mutate(행정주소 = str_replace_all(행정주소, "[[:punct:]]", ""))

filteredData_missing <- filteredData %>%
  filter(is.na(행정주소))

filteredData_missing


