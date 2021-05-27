library(ggplot2)
library(tidyselect)
library(dplyr)
library(tidyverse)
library(Stat2Data)
data(Backpack)
library(car)

df <- read.csv('C:/Users/ekate/Desktop/RRRR/b_depressed.csv')
df1 <- subset(df, select = c(Number_children, total_members, living_expenses, depressed))
df2 <- df1 %>% drop_na
#print (as_tibble(df2)) # красиво представить таблицу если нужно
df_without <- df2 %>% filter(depressed == 0)
df_dep <- df2 %>% filter(depressed == 1)

# количество детей у тех, кто НЕ подвержен депрессии

print (var(df_without$Number_children))  # дисперсия
print (sd(df_without$Number_children))  # стандартное отклонение
print (mad(df_without$Number_children)) #медианное абсолютное отклонение
print (fivenum(df_without$Number_children)) #минимум, нижняя граница типичных значений, медиана, верхняя граница типичных значений, максимум
print (IQR(df_without$Number_children)) #межквартильный размах

# количество детей у тех, кто подвержен депрессии

print (var(df_dep$Number_children))  # дисперсия
print (sd(df_dep$Number_children))  # стандартное отклонение
print (mad(df_dep$Number_children)) #медианное абсолютное отклонение
print (fivenum(df_dep$Number_children)) #минимум, нижняя граница типичных значений, медиана, верхняя граница типичных значений, максимум
print (IQR(df_dep$Number_children)) #межквартильный размах

print(boxplot (Number_children ~ depressed, df2))
ch_with <- (subset(df_without, select = c(Number_children)))
ch_without <- (subset(df_dep, select = c(Number_children)))

t_test <- t.test(ch_with$Number_children, ch_without$Number_children, 
alternative="two.sided", 
var.equal=FALSE)
print (t_test)


print (hist(df2$total_members))
#гистограмма

print (boxplot(living_expenses ~ depressed, df2))
print (boxplot(total_members ~ depressed, df2))
#ящик с усами


child_0 <- subset(df, select = c(Number_children, depressed))
child_0 <- child_0 %>% filter(Number_children == 0)
child_0_depress <- sum(subset(child_0, select = c(depressed)))
child_0_without <- nrow(subset(child_0, select = c(depressed))) - child_0_depress
                        #38 человек

child_1 <- subset(df, select = c(Number_children, depressed))
child_1 <- child_1 %>% filter(Number_children == 1)
child_1_depress <- sum(subset(child_1, select = c(depressed)))
child_1_without <- nrow(subset(child_1, select = c(depressed))) - child_1_depress
                        #15 человек

child_2 <- subset(df, select = c(Number_children, depressed))
child_2 <- child_2 %>% filter(Number_children == 2)
child_2_depress <- sum(subset(child_2, select = c(depressed)))
child_2_without <- nrow(subset(child_2, select = c(depressed))) - child_2_depress
                        #51 человек

child_3 <- subset(df, select = c(Number_children, depressed))
child_3 <- child_3 %>% filter(Number_children == 3)
child_3_depress <- sum(subset(child_3, select = c(depressed)))
child_3_without <- nrow(subset(child_3, select = c(depressed))) - child_3_depress
                        #51 человек

child_4 <- subset(df, select = c(Number_children, depressed))
child_4 <- child_4 %>% filter(Number_children == 4)
child_4_depress <- sum(subset(child_4, select = c(depressed)))
child_4_without <- nrow(subset(child_4, select = c(depressed))) - child_4_depress
                        #40 человек

child_5 <- subset(df, select = c(Number_children, depressed))
child_5 <- child_5 %>% filter(Number_children == 5)
child_5_depress <- sum(subset(child_5, select = c(depressed)))
child_5_without <- nrow(subset(child_5, select = c(depressed))) - child_5_depress
                        #20 человек

child_6 <- subset(df, select = c(Number_children, depressed))
child_6 <- child_6 %>% filter(Number_children == 6)
child_6_depress <- sum(subset(child_6, select = c(depressed)))
child_6_without <- nrow(subset(child_6, select = c(depressed))) - child_6_depress
                        #9 человек

child_7 <- subset(df, select = c(Number_children, depressed))
child_7 <- child_7 %>% filter(Number_children == 7)
child_7_depress <- sum(subset(child_7, select = c(depressed)))
child_7_without <- nrow(subset(child_7, select = c(depressed))) - child_7_depress
                        #10 человек

child_8 <- subset(df, select = c(Number_children, depressed))
child_8 <- child_8 %>% filter(Number_children == 8)
child_8_depress <- sum(subset(child_8, select = c(depressed)))
child_8_without <- nrow(subset(child_8, select = c(depressed))) - child_8_depress
                        #2 человекd

child_9 <- subset(df, select = c(Number_children, depressed))
child_9 <- child_9 %>% filter(Number_children == 9)
child_9_depress <- sum(subset(child_9, select = c(depressed)))
child_9_without <- nrow(subset(child_9, select = c(depressed))) - child_9_depress
                        #1 человек

child_10 <- subset(df, select = c(Number_children, depressed))
child_10 <- child_10 %>% filter(Number_children == 10)
child_10_depress <- sum(subset(child_10, select = c(depressed)))
child_10_without <- nrow(subset(child_10, select = c(depressed))) - child_10_depress
                        #0 человек

child_11 <- subset(df, select = c(Number_children, depressed))
child_11 <- child_11 %>% filter(Number_children == 11)
child_11_depress <- sum(subset(child_11, select = c(depressed)))
child_11_without <- nrow(subset(child_11, select = c(depressed))) - child_11_depress
                        #1 человек

children <- c(0:11)
people_with <- c(child_0_depress, child_1_depress, child_2_depress, child_3_depress, child_4_depress, child_5_depress, child_6_depress, child_7_depress, child_8_depress, child_9_depress, child_10_depress, child_11_depress)
people_without <- c(child_0_without, child_1_without, child_2_without, child_3_without, child_4_without, child_5_without, child_6_without, child_7_without, child_8_without, child_9_without, child_10_without, child_11_without)
CH_PE <- data.frame(children=children, people_with=people_with, people_without=people_without)


t_test <- t.test(CH_PE$people_with, CH_PE$people_without, 
alternative="two.sided", 
var.equal=TRUE)
print (t_test)