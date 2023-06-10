
#install.packages('glue')
#install.packages("readxl")
#install.packages("corrplot")
# install.packages("ggpubr")
# 라이브러리 로드

library(ggpubr)
library(readxl)
library(jsonlite)
library(XML)
library(xml2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(corrplot)

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

# 1동과 2동을 하나의 동으로 묶기 위한 함수
combine_dongs <- function(dong) {
  # 1동과 2동을 하나의 동으로 변경
  if (dong %in% c("웅동1동", "웅동2동")) {
    return("웅동")}
  if (dong %in% c("합성1동", "합성2동")) {
    return("합성동")
  }
  if (dong %in% c("회원1동", "회원2동")) {
    return("회원동")
  }
  if (dong %in% c("구암1동", "구암2동")) {
    return("구암동")
  }
  if (dong %in% c("양덕1동", "양덕2동")) {
    return("양덕동")
  }
   else {
    return(dong)
  }
}

# 행정기관 열 값 변경
filteredData$행정기관 <- sapply(filteredData$행정기관, combine_dongs)
unique(filteredData$행정기관)


# '행정기관' 별로 '남'과 '여'의 합 계산
summary_df <- filteredData %>%
  group_by(행정기관) %>%
  summarize(남 = sum(남), 여 = sum(여))
summary_df

# 남성과 여성 합산 후 내림차순 정렬
summary_df1 <- summary_df %>%
  mutate(합계 = 남 + 여) %>%
  arrange(desc(합계)) 
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
  labs(x = "행정기관", y = "인구 수", title = "행정기관별 인구 수 (0~19세)") +
  scale_fill_manual(values = c("#3399CC", "#FF7777"), labels = c("남성", "여성")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

#전체 인구수 구하기
everyData <- filter(dfJson, 연령대 %in% c("0", "10","30","40","50","60","70","80","90"))
everyData$행정기관 <- sapply(everyData$행정기관, combine_dongs)
summary_every_df <- everyData %>%
  group_by(행정기관) %>%
  summarize(남 = sum(남), 여 = sum(여))
summary_every_df
summary_every_df <- summary_every_df %>%
  mutate(합계 = 남 + 여) %>%
  arrange(desc(합계)) 
summary_every_df <- summary_every_df %>%
  select(-남, -여)
summary_every_df

#아동센터
senter_url = ""
senter_json = fromJSON(senter_url)
print(head(senter_json))
senter_df = data.frame(senter_json$data)
senter_df

#동만 추출
filteredData <- senter_df %>%
  mutate(행정주소 = str_extract(주소, "\\(.*[동면]\\)"))

#괄호제거
filteredData <- filteredData %>%
  mutate(행정주소 = str_replace_all(행정주소, "[[:punct:]]", ""))

filteredData
nrow(filteredData)

#결측치행 처리
filteredData_missing <- filteredData %>%
  filter(is.na(행정주소))

#읍면리 가져오기
filteredData_missing <- filteredData_missing %>%
  mutate(행정주소 = str_extract(주소, "[가-힣0-9]+(동|면|읍)"))

#결측치 하나는 경화동
filteredData_missing <- filteredData_missing %>%
  mutate(행정주소 = ifelse(is.na(행정주소), "경화동", 행정주소))

#다시 합치기
filteredData <- filteredData %>%
  left_join(filteredData_missing, by = c("시설명", "전화번호", "주소")) %>%
  mutate(행정주소 = coalesce(행정주소.x, 행정주소.y)) %>%
  select(-행정주소.x, -행정주소.y)

#동이름이 좀 이상해서 수정
filteredData$행정주소 <- gsub("가음동", "가음정동", filteredData$행정주소)
filteredData$행정주소 <- gsub("봉곡동 코오롱아파트 상가동", "봉곡동", filteredData$행정주소)

#안합쳐진 22개 전처리
#안합쳐진 행들 출력
# not_merged_rows <- address_counts[!(address_counts$행정주소 %in% merged_df$행정주소), ]
# not_merged_rows
# 행정주소 아동센터 수
# 6    남문동           1
# 8    대방동           1
# 10   대창동           1
# 12   도계동           2
# 14   마천동           1
# 15     명동           1
# 16   반지동           1
# 17   봉곡동           2
# 18   부림동           1
# 23   서상동           1
# 26   소계동           1
# 27   소답동           2
# 28   송학동           1
# 29   신월동           1
# 30   안골동           1
# 31   안민동           1
# 41   태평동           1
# 42   팔용동           2
#
######
#나중에 merge 했을 때 부속되어있는 동 이거나 없는 동은 인접동으로 변경
filteredData$행정주소 <- gsub("남문동", "웅천동", filteredData$행정주소)
filteredData$행정주소 <- gsub("대방동", "가음정동", filteredData$행정주소)
filteredData$행정주소 <- gsub("대창동", "문화동", filteredData$행정주소)
filteredData$행정주소 <- gsub("도계동", "명곡동", filteredData$행정주소)
filteredData$행정주소 <- gsub("마천동", "웅동", filteredData$행정주소)
filteredData$행정주소 <- gsub("명동", "웅천동", filteredData$행정주소)
filteredData$행정주소 <- gsub("반지동", "반송동", filteredData$행정주소)
filteredData$행정주소 <- gsub("봉곡동", "명곡동", filteredData$행정주소)
filteredData$행정주소 <- gsub("부림동", "오동동", filteredData$행정주소)
filteredData$행정주소 <- gsub("서상동", "의창동", filteredData$행정주소)
filteredData$행정주소 <- gsub("소계동", "의창동", filteredData$행정주소)
filteredData$행정주소 <- gsub("소답동", "의창동", filteredData$행정주소)
filteredData$행정주소 <- gsub("송학동", "여좌동", filteredData$행정주소)
filteredData$행정주소 <- gsub("신월동", "반월중앙동", filteredData$행정주소)
filteredData$행정주소 <- gsub("안골동", "웅동", filteredData$행정주소)
filteredData$행정주소 <- gsub("안민동", "성주동", filteredData$행정주소)
filteredData$행정주소 <- gsub("태평동", "충무동", filteredData$행정주소)
filteredData$행정주소 <- gsub("팔용동", "팔룡동", filteredData$행정주소)

# 행정주소별 등장 횟수 카운트
address_counts <- filteredData %>%
  count(행정주소, name = "아동센터 수")
address_counts
nrow(address_counts)

#결과 합치기
merged_df <- merge(address_counts, summary_df1, by.x = "행정주소", by.y = "행정기관", all.y = TRUE)
merged_df[is.na(merged_df)] <- 0

# 행정기관 열의 고유한 값 추출
unique_values_merged_df <- unique(merged_df$행정주소)
unique_values_merged_df

#아동센터는 82개인데 60개만 merge됨
sum(address_counts$'아동센터 수')
sum(merged_df$'아동센터 수')

#엑셀 파일 읽기
area_df_first <- read_excel("C:/Users/mmnnb/Documents/창원시면적.xlsx")
area_df_first

#행정주소 별로 면적을 구하는 전처리하기
colnames(area_df_first) <-area_df_first[2, ]
area_df <- area_df_first[-c(1:2), ]
area_df_filtered <- area_df[is.na(area_df$"읍면동별(1)"), ]
area_df_end=area_df_filtered[, 2:3]
area_df_end$소계 <- as.numeric(area_df_end$소계)
area_df_end$"읍면동별(2)" <- sapply(area_df_end$"읍면동별(2)", combine_dongs)
area_df_end <- aggregate(. ~ `읍면동별(2)`, data = area_df_end, FUN = sum)

#면적 추가하기
merged_area_df <- merge(merged_df, area_df_end, by.x = "행정주소", by.y = "읍면동별(2)", all.x = TRUE)
colnames(merged_area_df)[6] <- "면적"

#인구밀도 추가하기
merged_area_df <- merged_area_df[, -c(3, 4)]  # "남", "여" 컬럼 제거
merged_area_df$인구밀도 <- merged_area_df$합계 / merged_area_df$면적

#초등학교 개수 추출
elementary_school_data <-read_excel("C:/Users/mmnnb/Documents/전국초중등학교위치표준데이터.xls")
elementary_school_changwon <- elementary_school_data[grep("창원시", elementary_school_data$소재지도로명주소), ]
elementary_school_changwon <- elementary_school_changwon[grep("초등학교", elementary_school_changwon$학교급구분), ] 

#초등학교 주소 들고오기
elementary_school_changwon <- elementary_school_changwon %>%
  mutate(행정주소 = str_extract(소재지도로명주소, "[가-힣]+(동|면|읍)(?![가-힣])"))

#결측치 2개 있음
#이유는 주소가 기입되어 있지 않음
elementary_school_changwon$행정주소
elementary_school_changwon$행정주소[[12]]="의창동"
elementary_school_changwon$행정주소[[81]]="용원동"

elementary_school_changwon$행정주소 <- gsub("가음동", "가음정동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("남문동", "웅천동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("대방동", "가음정동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("대창동", "문화동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("도계동", "명곡동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("마천동", "웅동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("명동", "웅천동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("반지동", "반송동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("봉곡동", "명곡동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("부림동", "오동동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("서상동", "의창동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("소계동", "의창동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("소답동", "의창동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("송학동", "여좌동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("신월동", "반월중앙동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("안골동", "웅동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("안민동", "성주동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("태평동", "충무동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("팔용동", "팔룡동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("남양동", "가음정동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("대내동", "문화동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("대원동", "팔룡동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("도천동", "여좌동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("두척동", "회성동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("명서동", "명곡동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("반림동", "반송동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("사림동", "용지동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("삼정자동", "가음정동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("성호동", "교방동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("신촌동", "웅남동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("용원동", "웅동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("용호동", "용지동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("장군동", "완월동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("장천동", "풍호동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("제황산동", "충무동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("중동", "의창동", elementary_school_changwon$행정주소)
elementary_school_changwon$행정주소 <- gsub("청안동", "웅동", elementary_school_changwon$행정주소)

# 행정주소별 등장 횟수 카운트
elementary_count <- elementary_school_changwon %>%
  count(행정주소, name = "초등학교 수")

#면적 추가하기
merged_elementary_df <- merge(merged_area_df, elementary_count , by.x = "행정주소", by.y = "행정주소", all.x = TRUE)
merged_elementary_df
not_merged_rows <- elementary_count[!(elementary_count$행정주소 %in% merged_elementary_df$행정주소), ]

#덕산초인데 자은동에 있어서 따로 수정...
rows_with_덕산 <- elementary_school_changwon[grep("덕산", elementary_school_changwon$소재지도로명주소), ]
merged_elementary_df[9,"초등학교 수"]=1
merged_elementary_df[37,"초등학교 수"]=2
merged_elementary_df[is.na(merged_elementary_df)] <- 0

#기초생활보장 수급권자
basic_df <- read_excel("C:/Users/mmnnb/Desktop/BIK/새 폴더/기초생활보장 수급권자수 현황(2022년1월~12월)/기준연월_2022-12_기초생활보장 수급권자구분별 총괄 현황.xlsx")
basic_df <- basic_df[-c(1:10), ]
basic_df=basic_df[, 3:4]
colnames(basic_df) <- c("행정주소", "수급권자")


print(basic_df, n=57)
# 행정기관 열 값 변경
basic_df$행정주소 <- sapply(basic_df$행정주소, combine_dongs)
basic_df <- basic_df %>%
  group_by(행정주소) %>%
  summarise(수급권자 = sum(as.numeric(수급권자)))

#수급권자 추가하기
merged_basic_df <- merge(merged_elementary_df, basic_df, by.x = "행정주소", by.y = "행정주소", all.x = TRUE)
merged_basic_df$수급권자 <- as.numeric(merged_basic_df$수급권자)

#전체인구 추가하기
merged_basic_df <- merge(merged_basic_df, summary_every_df, by.x = "행정주소", by.y = "행정기관", all.x = TRUE)
merged_basic_df
colnames(merged_basic_df)
merged_basic_df$수급권자 <- merged_basic_df$수급권자 / merged_basic_df$합계.y
merged_basic_df <- merged_basic_df[, -which(colnames(merged_basic_df) == "합계.y")]

# 상관계수 행렬 생성
cor_matrix <- cor(merged_basic_df[, c("아동센터 수", "합계.x", "면적", "인구밀도","초등학교 수","수급권자")])
cor_matrix 
corrplot(cor_matrix, method = 'color', order = 'alphabet')

# 클러스터링에 사용할 변수 선택
cluster_vars <- c("아동센터 수","합계.x", "인구밀도", "초등학교 수", "수급권자")

# 클러스터링에 사용할 데이터프레임 생성
cluster_data <- merged_basic_df[, cluster_vars]

# 클러스터링을 위한 표준화
cluster_data_scaled <- scale(cluster_data)

# PCA 수행
pca_result <- prcomp(cluster_data_scaled, scale = TRUE)

# 주성분 요약 정보 출력
summary(pca_result)

# 주성분 개수 선택 (예: 3개)
num_components <- 2

# 선택한 주성분 개수로 PCA 변환 수행
pca_data <- predict(pca_result, newdata = cluster_data_scaled)[, 1:num_components]

# 엘보우 방법을 위한 응집도 계산
wss_pca <- numeric(length = 10)
for (i in 1:10) {
  kmeans_result <- kmeans(pca_data, centers = i)
  wss_pca[i] <- kmeans_result$tot.withinss
}
wss_pca

# 응집도 그래프 그리기
plot(1:10, wss_pca, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares")

# K-means 클러스터링 적용
k_pca <- 5
set.seed(123)
kmeans_result_pca <- kmeans(pca_data, centers = k_pca)

# 클러스터링 결과 확인
cluster_labels_pca <- kmeans_result_pca$cluster
cluster_centers_pca <- kmeans_result_pca$centers

# 클러스터링 결과를 데이터프레임에 추가
merged_basic_df$Cluster_PCA <- cluster_labels_pca

# 클러스터링 결과 출력
print(merged_basic_df)

# PCA 데이터 시각화
pca_data_df <- as.data.frame(pca_data)
pca_data_df$Cluster <- factor(cluster_labels_pca)
ggplot(pca_data_df, aes(x = PC1, y = PC2, color = Cluster, label = merged_basic_df$행정주소)) +
  geom_point(size = 3) +
  geom_text(vjust = -1.5, color = "black") +
  labs(x = "PC1", y = "PC2", color = "Cluster") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 10)
  )
merged_basic_df
#컬럼명 변경
colnames(merged_basic_df) <- c("행정주소","아동센터_수","합계","면적","인구밀도","초등학교_수","수급권자","Cluster_PCA")

# 클러스터별 데이터 분포 확인
summary_df <- merged_basic_df %>%
  group_by(Cluster_PCA) %>%
  summarise(
    Avg_아동센터_수 = mean(아동센터_수),
    Avg_합계 = mean(합계),
    Avg_면적 = mean(면적),
    Avg_인구밀도 = mean(인구밀도),
    Avg_초등학교_수 = mean(초등학교_수),
    Avg_수급권자 = mean(수급권자)
  )

# 아동센터 수 시각화
plot1 <- ggplot(summary_df, aes(x = Cluster_PCA, y = Avg_아동센터_수, fill = Cluster_PCA)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "클러스터", y = "평균 아동센터 수", fill = "클러스터") +
  theme_minimal()

# 합계 시각화
plot2 <- ggplot(summary_df, aes(x = Cluster_PCA, y = Avg_합계, fill = Cluster_PCA)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "클러스터", y = "평균 합계", fill = "클러스터") +
  theme_minimal()

# 면적 시각화
plot3 <- ggplot(summary_df, aes(x = Cluster_PCA, y = Avg_면적, fill = Cluster_PCA)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "클러스터", y = "평균 면적", fill = "클러스터") +
  theme_minimal()

# 인구밀도 시각화
plot4 <- ggplot(summary_df, aes(x = Cluster_PCA, y = Avg_인구밀도, fill = Cluster_PCA)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "클러스터", y = "평균 인구밀도", fill = "클러스터") +
  theme_minimal()

# 초등학교 수 시각화
plot5 <- ggplot(summary_df, aes(x = Cluster_PCA, y = Avg_초등학교_수, fill = Cluster_PCA)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "클러스터", y = "평균 초등학교 수", fill = "클러스터") +
  theme_minimal()

# 수급권자 시각화
plot6 <- ggplot(summary_df, aes(x = Cluster_PCA, y = Avg_수급권자, fill = Cluster_PCA)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "클러스터", y = "평균 수급권자", fill = "클러스터") +
  theme_minimal()

# subplot 그리기
ggarrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 2, nrow = 3)

#군집 분석
merged_basic_df
merged_basic_df[merged_basic_df$아동센터_수 == 0, ]
merged_basic_df[merged_basic_df$Cluster_PCA == 1, ]
merged_basic_df[merged_basic_df$Cluster_PCA == 2, ]
merged_basic_df[merged_basic_df$Cluster_PCA == 3, ]
merged_basic_df[merged_basic_df$Cluster_PCA == 4, ]
merged_basic_df[merged_basic_df$Cluster_PCA == 5, ]
