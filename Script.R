#### Bank Churners ####
BC <- read.csv("BankChurners.csv")

# Mengetahui range umur customer dan frekuensinya menggunakan histogram dan summary.
summary(BC$Customer_Age)
hist(BC$Customer_Age)
# Diketahui bahwa umur customer pada range 26 hingga 73, dan  frekuensi tertinggi di range umur 45 s/d 50.

# Mengetahui gender customer menggunakan summary factor, dan library dplyr.
summary.factor(BC$Gender)
install.packages('dplyr')
library(dplyr)
sum(BC$Gender == 'M')
sum(BC$Gender == 'F')
# Dengan lib dplyr
BC %>%
  group_by(BC$Gender) %>%
  summarise(gender_count=n())
# Diketahui bahwa customer female (5,358) > customer male (4,769).

# Mengetahui banyaknya customer yang attrited dan existing.
sum(BC$Attrition_Flag == 'Attrited Customer')
sum(BC$Attrition_Flag == 'Existing Customer')
# Diketahui bahwa customer yang sudah ada sebanyak 8500,
# lebih banyak dibanding dengan customer yang tertarik sebanyak 1626

# Mengetahui banyaknya data dari status pernikahan customer menggunakan group_by dan sumarise dari lib dplyr.
BC %>%
  group_by(BC$Marital_Status) %>%
  summarise(marital_count=n())
# Dapat diketahui bahwa data status pernikahan paling banyak adalah menikah yakni sebanyak 4,687.
# Sedangkan yang lainnya dapat dilihat sbb:
# 1 Divorced           748
# 2 Married            4687
# 3 Single             3943
# 4 Unknown            749

# Mengetahui education level pada customer.
BC %>%
  group_by(BC$Education_Level) %>%
  summarise(education_count=n())
# Diketahui bahwa level edukasi paling tinggi pada customer adalah Graduate.
# Sedangkan data lainnya dapat dilihat sbb:
# 1 College            1013
# 2 Doctorate          451
# 3 Graduate           3128
# 4 High School        2013
# 5 Post-Graduate      516
# 6 Uneducated         1487
# 7 Unknown            1519

# Mengetahui income category pada customer.
BC %>%
  group_by(BC$Income_Category) %>%
  summarise(income_cat_count=n())
# Pendapatan customer paling banyak di angka kurang dari $40k yakni sebanyak 3561.
# Sedangkan data lainnya dapat dilihat sbb:
# 1 $120K +            727
# 2 $40K - $60K        1790
# 3 $60K - $80K        1402
# 4 $80K - $120K       1535
# 5 Less than $40K     3561
# 6 Unknown            1112

# Mengetahui kategori kartu yang paling banyak dimiliki customer.
BC %>%
  group_by(BC$Card_Category) %>%
  summarise(cards_cat_count=n())
# Kartu dengan kategori Blue adalah kartu yang paling banyak dimiliki customer dengan jumlah 9436.
# Sedangkan data lainnya dapat dilihat sbb:
# 1 Blue               9436
# 2 Gold               116
# 3 Platinum           20
# 4 Silver             555

# Setelah diketahui data-data tersebut kita mencari korelasi pada data tersebut.

#### Korelasi Antar Data ####
# Mengetahui korelasi pada data Bank Churners
cor(BC$Customer_Age, BC$Dependent_count, method = 'pearson')
cor(BC$Customer_Age, BC$Months_on_book, method = 'pearson')
cor(BC$Customer_Age, BC$Months_Inactive_12_mon, method = 'pearson')
cor(BC$Customer_Age, BC$Contacts_Count_12_mon, method = 'pearson')

# Uji Korelasi
cor.test(BC$Customer_Age, BC$Dependent_count, method = 'pearson', conf.level = 0.9)
cor.test(BC$Customer_Age, BC$Months_on_book, method = 'pearson', conf.level = 0.8)

#### Boxplot ####
# Boxplot untuk kolom Customer Age
BC %>%
  group_by(BC$Customer_Age) %>%
  summarise(Customer_Age_count=n())
boxplot(BC$Customer_Age)
# Pada customer age memiliki pencilan dengan umur > 70. Dengan nilai tengah 46.

# Boxplot lainnya
boxplot(BC$Months_on_book)
boxplot(BC$Total_Relationship_Count)

#### Scatter Plot ####
scatter.smooth(BC$Customer_Age, BC$Months_on_book)
scatter.smooth(BC$Customer_Age, BC$Dependent_count)
scatter.smooth(BC$Customer_Age, BC$Total_Relationship_Count)

#### ggplot2  ####
# The easiest way to get ggplot2 is to install the whole tidyverse:
install.packages("tidyverse")
library(ggplot2)
library(tidyverse)
ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))

#### Check the correlation between categorical and numeric independent variable ####
install.packages("ltm")
library(ltm)

# Korelasi antara Customer Age (numeric) with Attrition Flag (categorical)
biserial.cor(BC$Customer_Age, BC$Attrition_Flag)
# Out:    [1] 0.01820314

# Korelasi antara Customer Age (numeric) with Gender (categorical)
biserial.cor(BC$Customer_Age, BC$Gender)
# Out:    [1] 0.01731152

biserial.cor(BC$Attrition_Flag, BC$Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1)

#### Barplot Diagram Batang ####
# Status pernikahan customer.
barplot (table(BC$Marital_Status))

# Attrition Customer.
barplot (table(BC$Attrition_Flag))

# Income Customer (Categorical)
barplot (table(BC$Income_Category))

# Stacked Bar Plot with Colors and Legend
cag <- table(BC$Attrition_Flag, BC$Customer_Age)
barplot(cag, main="Attrition Flag dan Umur Customer", horiz = TRUE,
        xlab="Umur Customer", col=c("darkblue","red"),
        legend = rownames(cag), beside = TRUE)

#### HeatMap  ####
BCclear <- subset(BC, select=c(1, 3, 5, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21))
BCclear <- BCclear[order(BC$Customer_Age),]
head(BCclear)
row.names(BCclear) <- BCclear$CLIENTNUM
BCclear_matrix <- data.matrix(BCclear)
BCclear_heatmap <- heatmap(BCclear_matrix, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))
