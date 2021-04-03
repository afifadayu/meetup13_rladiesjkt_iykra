install.packages("tidyverse")
install.packages("ggrepel")
install.packages("sqldf")
library(tidyverse)
library(ggrepel)
library(sqldf)

raw_data=read.csv("Dataset.csv")
cols <- names(raw_data)
#regression: lm(dependen~independen)

#iterasi automate sendiri
#missing value kalo datanya sedikit: mean/median

predictor <- grep("^(X|Y)", cols, value = TRUE)
rsquare <- sapply(predictor, function(pred) {
  a <- lm(raw_data$MM~raw_data[[pred]], na.action="na.omit") 
  summary(a)$r.squared
})

#rsq
Result5 <- data.frame(cols = predictor, rsquare = rsquare)

atributes <- grep("^(X|Y)", cols, value = TRUE)
Dataset<-raw_data[,atributes]
combine<-as.data.frame(gather(Dataset,attrb,score))
combine$itung<-ifelse(combine$score>=4,1,0)
combine$pembagi<-ifelse(combine$score>=1,1,0)

#top 2 box
T2B<-sqldf("select attrb,sum(itung)/sum(pembagi) T2B from combine group by 1")

Result5 %>% 
  arrange(cols) -> Result5
T2B %>% 
  arrange(attrb) -> T2B
cbind(Result5, T2B = T2B[,2]) -> hasil

#COLS + RSQ + Top two Box
hasil

#plot
hasil %>% 
  filter(cols != "MM") %>% 
  ggplot(aes(x = T2B, y = rsquare)) +
  geom_point() + 
  geom_vline(xintercept = quantile(hasil$T2B, probs = 0.33), color ="red") + 
  geom_vline(xintercept = quantile(hasil$T2B, probs = 0.67), color = "red") + 
  geom_hline(yintercept = quantile(hasil$rsquare, probs = 0.33), color = "blue") + 
  geom_hline(yintercept = quantile(hasil$rsquare, probs = 0.67), color = "blue") +
  geom_text_repel(aes(label = cols), check_overlap = T)


#contoh, satu persatu
model <- lm(raw_data$MM ~ raw_data$Y1)
summary(model)
