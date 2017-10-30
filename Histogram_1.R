library(readr)
library(ggplot2)
library(dplyr)
library(wordcloud2)
Consumer_Complaints <- read_csv("C:/Users/Dominique Njinkeu/OneDrive - afrtsd.com/Project/Consumer_Complaints.csv")

cc<-Consumer_Complaints %>% filter(`Timely response?`=="No")%>% group_by(Product) %>% summarise(total_count=n()) %>%arrange(desc(total_count)) %>%top_n(4)
theme_set(theme_bw())
prod<-ggplot(cc,aes(y=total_count,x=Product))
prod+geom_bar(stat = "identity")

