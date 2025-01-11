#kullanilan paketler library(dplyr)  # Veri manipulasyonu
#library(tidyverse)  # Veri analizi ve gorsellestirme
#library(ggplot2)  # Gelismis gorsellestirme
#library(caret)  # Makine ogrenimi araclar??
#library(Metrics)#MAE,RMSE benzeri olcutler
#install.packages("randomForest")
#library(randomForest)

df=read.table(file.choose(),header=T,sep=",")
str(df)
summary(df)
dim(df)
head(df)
tail(df)
anyNA(df)
unique(df)
names(df)
t(summary(df))

head(df$Date)  # ??lk 6 sat??r
str(df$Date)   # Veri yap??s?? inceleme

#tarih formati donusturme
df$Date <- as.Date(df$Date, format="%d-%m-%Y")

# Kontrol etmek i??in ilk birka?? sat??r?? g??r??nt??le
head(df$Date)



summary(df$Date)  # Tarih s??tununun ??zetini g??r??nt??le



head(df$Date)     # ??lk 6 sat??r?? g??r??nt??le

sum(is.na(df$Date))  # Eksik de??er say??s??n?? g??sterir

# Date s??tununun i??eri??ini kontrol et
head(df$Date)

str(df$Date)


max_date <- max(df$Date, na.rm = TRUE)


min_date <- min(df$Date, na.rm = TRUE)

print(paste("Maksimum Tarih:", max_date))

print(paste("Minimum Tarih:", min_date))

# 1 Eylul'den sonrasi haric tutarak veriyi filtreleme
df <- df[!(format(df$Date, "%m") == "09" & format(df$Date, "%d") > 1), ]

# Benzersiz magazalar kontrol edilir
df <- as.data.frame(df)
class(df)
unique(df$Store)

# Magaza bazinda veri grupla ve sayisal sutunlarin ortalamalarini al
storemean <- aggregate(. ~ Store, data=df, FUN=mean, na.rm=TRUE)


head(storemean, 5)

# Her magazanin en yuksek satisi
storemax <- tapply(df$Weekly_Sales, df$Store, max, na.rm = TRUE)

# Her magazanin en dusuk satisi
storemin <- tapply(df$Weekly_Sales, df$Store, min, na.rm = TRUE)

# En yuksek ve en dusuk satislar
head(storemax, 5)
head(storemin, 5)

install.packages("viridis")  
library(viridis)            

library(ggplot2)

# Magaza bazinda haftalik satislar icin barplot 
ggplot(storemean, aes(x=factor(Store), y=Weekly_Sales)) +
  geom_bar(stat="identity", fill="steelblue") +  # Tek renk
  theme_minimal() +
  labs(title="Magaza Baz??nda Haftalik Satislar", 
       x="Magaza", y="Haftalik Satislar") +
  theme(axis.text.x = element_text(angle=90, hjust=1))

install.packages("ggplot")
library(ggplot2)

# ggplot2 ile Histogram
ggplot(df, aes(x=Weekly_Sales)) +
  geom_histogram(bins=10, fill="skyblue", color="black") +
  labs(title="Satis Dagilimi", 
       x="Haftalik Satis Miktarlari", 
       y="Frekans") +
  theme_minimal() +
  theme(text = element_text(family = "sans", size=12))  # T??rk??e karakter deste??i

# Veriyi uzun formata donusturme ile daha esnek ve etkili gorselelstirmek icin
df_long <- reshape(df, 
                   varying = c("Weekly_Sales", "Temperature", "Fuel_Price", "CPI", "Unemployment"), 
                   v.names = "value", 
                   timevar = "variable", 
                   times = c("Weekly_Sales", "Temperature", "Fuel_Price", "CPI", "Unemployment"), 
                   direction = "long")



set.seed(123) # Rastgele say?? ??reticisi i??in ba??lang???? noktas?? belirle (ayn?? veriyi elde etmek i??in)
veri <- data.frame(
  variable = rep(c("CPI", "Fuel_Price", "Temperature", "Unemployment", "Weekly_Sales"), each = 100),
  value = c(rlnorm(100, meanlog = 2), rlnorm(100, meanlog = 3), rlnorm(100, meanlog = 4), rlnorm(100, meanlog = 5), rlnorm(100, meanlog = 8))
)
df <- veri
install.packages('ggplot2')
library(ggplot2)
library(reshape2)
library(tidyr)

#  veri ??er??evesi
df <- data.frame(
  CPI = c(1.1, 1.3, 1.2, 1.5, 1.7),
  Fuel_Price = c(3.2, 3.0, 3.1, 2.9, 3.3),
  Temperature = c(45, 50, 55, 53, 49),
  Unemployment = c(7.1, 6.9, 7.0, 7.2, 6.8),
  Weekly_Sales = c(15000, 16000, 15500, 14500, 17000)
)

# Uzun formata d??n????t??rme
df_long <- pivot_longer(df, cols = everything(), names_to = "Degiskenler", values_to = "Degerler")

ggplot(df_long, aes(x = Degiskenler, y = Degerler)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  scale_y_log10() +  # Y ekseninde logaritmik ??l??ek uygular
  labs(
    title = "Ayk??r?? Degerler",
    x = "Deg??kenler",
    y = "Degerler"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans"),          # T??rk??e karakter deste??i
    axis.text.x = element_text(angle = 45, hjust = 1)  # X ekseninde yaz??lar?? e??ik yapar
  )




# Boxplot
ggplot2(df, aes(x=varia, y=value)) +
  geom_boxplot(fill='skyblue', color='black') +
  scale_y_log10() +  # Logaritmik ??l??ek uygula (10 taban??)
  labs(
    title="Aykiri Degerler", 
    x="Degiskenler", 
    y="Degerler"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family="sans"),       # T??rk??e karakter deste??i
    axis.text.x = element_text(angle=45, hjust=1)  # X ekseninde yaz??lar?? e??ik yap
  )


install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)

# ??rnek veri 
set.seed(123)
df <- data.frame(
  Holiday_Flag = sample(c(0, 1), size = 100, replace = TRUE),
  Weekly_Sales = rlnorm(100, meanlog = 8)
)

# Barplot 
ggplot(df, aes(x = factor(Holiday_Flag), y = Weekly_Sales, fill = factor(Holiday_Flag))) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  scale_fill_manual(values = c("skyblue", "lightpink"), name = "Tatil Durumu") +
  labs(
    title = "Tatillerin Sat??s Uzerindeki Etkisi",
    x = "Tatil Durumu",
    y = "Ortalama Haftal??k Sat??s"
  ) +
  theme_minimal()

library(ggplot2)
library(dplyr)
library(lubridate)

# (2010-2012 y??llar??n?? kapsayan haftal??k tarihler)
set.seed(123)
df <- data.frame(
  Date = seq(as.Date("2010-01-01"), as.Date("2012-12-31"), by = "week"),
  Weekly_Sales = rlnorm(length(seq(as.Date("2010-01-01"), as.Date("2012-12-31"), by = "week")), meanlog = 8)
)

# 'Date' s??tunundan y??l bilgisini al??n??r
df <- df %>%
  mutate(Year = year(Date))

# Veriyi y??la g??re grupland??r??p toplam sat????lar?? hesaplama
yearly_sales <- df %>%
  group_by(Year) %>%
  summarise(Total_Sales = sum(Weekly_Sales))

# Veriyi g??rselle??tirme
ggplot(yearly_sales, aes(x = Year, y = Total_Sales)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 3) +
  scale_x_continuous(breaks = yearly_sales$Year) +  # Yaln??zca tam y??l de??erlerini g??ster
  labs(
    title = "Y??ll??k Toplam Sat??s Miktarlar??" ,
        x = "Y??l",
    y = "Toplam Sat???? Miktar??"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans"),
    panel.grid.minor = element_line(color = "gray", linetype = "dashed")
  )

install.packages("ggplot2")
library(ggplot2)


df=read.table(file.choose(),header=T,sep=",")

df$Temperature <- as.numeric(as.character(df$Temperature))
df$Weekly_Sales <- as.numeric(as.character(df$Weekly_Sales))

library(ggplot2)
library(scales) 

ggplot(df, aes(x = Temperature, y = Weekly_Sales)) +
  geom_point(color = "skyblue", alpha = 0.8) +
  labs(
    title = "Sicakl??k ve Sat???? ??li??kisi",
    x = "Sicakl??k",
    y = "Haftal??k Sat???? Miktar??"
  ) +
  scale_y_continuous(labels = comma) + # Virg??ll?? say??lar
  theme_minimal()

#yakit fiyat?? satis
library(ggplot2)
library(ggplot2)
library(scales) 

ggplot(df, aes(x=Fuel_Price, y=Weekly_Sales)) +
  geom_jitter(alpha=0.6, color="skyblue", width = 0.1) +
  geom_smooth(method="lm", color="red") +
  scale_y_log10(labels = comma) + # virg??ll?? say??lar
  labs(title="Yakit Fiyati ve Haftalik Satis Iliskisi (Logaritmik Olcek ve Jitter)",
       x="Yakit Fiyati", y="Haftalik Satis (Logaritmik Olcek)") +
  theme_minimal()


library(ggplot2)
library(gridExtra)

# CPI ve Unemployment icin histogram ve KDE 
p1 <- ggplot(df, aes(x=CPI)) +
  geom_histogram(aes(y=..density..), bins=30, fill="purple", color="black", alpha=0.6) +
  geom_density(color="black", fill="purple", alpha=0.3) +
  labs(title="Tuketici Fiyat Endeksi Dagilimi", x="CPI", y="Yogunluk") +
  theme_minimal()

p2 <- ggplot(df, aes(x=Unemployment)) +
  geom_histogram(aes(y=..density..), bins=30, fill="green", color="black", alpha=0.6) +
  geom_density(color="black", fill="green", alpha=0.3) +
  labs(title="Issizlik Orani Dagilimi", x="Issizlik Orani", y="Yogunluk") +
  theme_minimal()

# ??k?? graf??k yan yana
grid.arrange(p1, p2, ncol=2)


# Korelasyon hesaplama
correlation <- cor(df[, sapply(df, is.numeric)])

print(correlation)


library(ggplot2)
library(reshape2)

# Korelasyon matrisi hesaplama
correlation_matrix <- cor(df[, sapply(df, is.numeric)])

# Korelasyon matrisini uzun formata donustur
melted_correlation_matrix <- melt(correlation_matrix)

# Isi haritasi olusturmak icin
ggplot(melted_correlation_matrix, aes(Var1, Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label=sprintf("%.2f", value)), color="white", size=3) +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0) +
  labs(title="Korelasyon Matrisi", x="Degiskenler", y="Degiskenler") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#modelleme
library(caret)
library(dplyr)

# Hedef de??i??ken ve ??zellikler
y <- df$Weekly_Sales
x <- df %>% select(-Date, -Weekly_Sales)

# E??itim ve test setleri ay??r
set.seed(42)
train_index <- createDataPartition(y, p = 0.7, list = FALSE)

x_train <- x[train_index, ]
x_test <- x[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Sadece say??sal s??tunlar?? se??
x_train_numeric <- x_train %>% select_if(is.numeric)
x_test_numeric <- x_test %>% select_if(is.numeric)

# Standardizasyon
x_train_scaled <- scale(x_train_numeric)
x_test_scaled <- scale(x_test_numeric, 
                       center = attr(x_train_scaled, "scaled:center"), 
                       scale = attr(x_train_scaled, "scaled:scale"))

# randomForest 
install.packages("randomForest")
library(randomForest)
library(Metrics)#hata metrikleri

# Random Forest modelini olu??tur ve e??it
rf_model <- randomForest(x = x_train_scaled, y = y_train)

# Test seti ??zerinde tahmin yap
y_pred_rf <- predict(rf_model, x_test_scaled)

# R^2 Skoru (Determinasyon Katsay??s??)
r2 <- cor(y_test, y_pred_rf)^2

# Mean Squared Error (MSE)
mse_rf <- mse(y_test, y_pred_rf)


# Mean Absolute Error (MAE)
mae_rf <- mae(y_test, y_pred_rf)

# Sonu??lar?? yazd??r
cat(sprintf("Random Forest R^2: %.3f\n", r2))
cat(sprintf("Random Forest MSE: %.3f\n", mse_rf))
cat(sprintf("Random Forest MAE: %.3f\n", mae_rf))


library(xgboost)  # XGBoost modeli i??in
library(Metrics)  # Hata metrikleri i??in

# E??itim ve test setlerini matrise d??n????t??r
x_train_matrix <- as.matrix(x_train_scaled)
x_test_matrix <- as.matrix(x_test_scaled)
y_train_vector <- as.numeric(y_train)
y_test_vector <- as.numeric(y_test)

# XGBoost modeli
xgb_model <- xgboost(data = x_train_matrix, 
                     label = y_train_vector, 
                     nrounds = 100,       # ??terasyon say??s??
                     objective = "reg:squarederror", # Regresyon i??in hata fonksiyonu
                     verbose = 0)         # E??itim ????kt??s??n?? kapat??r

# Test seti ??zerinde tahmin yap
y_pred_xgb <- predict(xgb_model, x_test_matrix)

# R^2 Skoru Hesaplama
r2_xgb <- cor(y_test_vector, y_pred_xgb)^2


# Mean Squared Error (MSE)
mse_xgb <- mse(y_test_vector, y_pred_xgb)

# Mean Absolute Error (MAE)
mae_xgb <- mae(y_test_vector, y_pred_xgb)

# Sonu??lar?? yazd??r
cat(sprintf("XGBoost R^2: %.3f\n", r2_xgb))
cat(sprintf("XGBoost MSE: %.3f\n", mse_xgb))
cat(sprintf("XGBoost MAE: %.3f\n", mae_xgb))

install.packages("lightgbm")
library(lightgbm)
install.packages("Metrics")
library(Metrics)
install.packages("caret")
library(caret)


# Ba????ms??z ve ba????ml?? de??i??kenlerin ayr??lmas??
x <- as.matrix(df[, -ncol(df)])  # Ba????ms??z de??i??kenler
y <- df[, ncol(df)]             # Ba????ml?? de??i??ken

# E??itim ve test setlerine b??lme (%80 e??itim, %20 test)
set.seed(123)  # Rastgelelik i??in sabit
train_index <- sample(1:nrow(df), size = 0.8 * nrow(df))
x_train <- x[train_index, ]
y_train <- y[train_index]
x_test <- x[-train_index, ]
y_test <- y[-train_index]

# Hiperparametre aral??????n?? tan??mlama
grid <- expand.grid(
  num_leaves = seq(20, 50, by = 10),
  learning_rate = seq(0.01, 0.1, by = 0.01),
  feature_fraction = seq(0.7, 1, by = 0.1),
  nrounds = seq(50, 150, by = 25)
)

# E??itim kontrol?? ve rastgele arama
control <- trainControl(
  method = "cv",         # ??apraz do??rulama
  number = 5,            # 5 katl?? ??apraz do??rulama
  search = "random",     # Rastgele arama
  verboseIter = TRUE     # E??itim ilerlemesini yazd??r
)

# Modelin e??itim s??reci
set.seed(123)  # Rastgelelik i??in sabit
lgb_model <- train(
  x = x_train, y = y_train,
  method = "lightgbm",
  tuneGrid = grid,
  trControl = control,
  metric = "RMSE" ,"MSE","MAE"       # Performans ??l????t??
)

# Optimum hiperparametreleri yazd??r
print(lgb_model$bestTune)

y_pred <- predict(lgb_model, newdata = x_test)


r2 <- cor(y_test, y_pred)^2
mse <- mean((y_test - y_pred)^2)
mae <- mean(abs(y_test - y_pred))

cat(sprintf("LightGBM R^2: %.3f\n", r2))
cat(sprintf("LightGBM MSE: %.3f\n", mse))
cat(sprintf("LightGBM MAE: %.3f\n", mae))



install.packages("dplyr")
library(dplyr)

# En fazla sat??s yapan magaza bulma
en_fazla_satis <- df %>%
  group_by(Store) %>%
  summarise(total_sales = sum(Weekly_Sales)) %>%
  filter(total_sales == max(total_sales)) %>%
  pull(Store)


cat(sprintf("En fazla sat??s yapan magaza: %s\n", en_fazla_satis))

# En az sat??s yapan magaza bulma
en_az_satis <- df %>%
  group_by(Store) %>%
  summarise(total_sales = sum(Weekly_Sales)) %>%
  filter(total_sales == min(total_sales)) %>%
  pull(Store)

cat(sprintf("En az sat??s yapan magaza: %s\n", en_az_satis))


install.packages("ggplot2")
library(ggplot2)

# En fazla ve en az sat??s yapan magazalar icin veriler
stores <- c('Magaza 20', 'Magaza 33')
sales <- c(sum(en_fazla_satis), sum(en_az_satis))

# Veriyi bir veri cercevesine donusturme
sales_data <- data.frame(Store = stores, Sales = sales)

# Bar grafik
ggplot(sales_data, aes(x = Store, y = Sales, fill = Store)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = 'En Fazla ve En Az Satis Yapan Magazalarin Haftalik Satis Karsilastirilmasi',
       x = 'Magazalar', y = 'Toplam Haftalik Satis ($)') +
  theme_minimal() +
  scale_fill_manual(values = c('blue', 'red')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid.major.y = element_line(linetype = "dashed", color = "gray"))



install.packages("ggplot2")
install.packages("dplyr")
install.packages("magrittr")  

library(magrittr)
library(ggplot2)
library(dplyr)

# Magaza 33 verisi filtrelenir
`%>%` <- magrittr::`%>%`
store33 <- df %>% filter(Store == 33)

store33 <- df %>% filter(Store == 33)

# 4 haftalik hareketli ortalama hesapla
store33 <- store33 %>%
  arrange(Date) %>%
  mutate(Rolling_Avg = zoo::rollapply(Weekly_Sales, 4, mean, fill = NA, align = 'right'))


# Eksik veri temizli??i
store33 <- na.omit(store33)

# Date format?? kontrol??
#store33$Date <- as.Date(store33$Date, format = "%Y-%m-%d")

ggplot(store33, aes(x = Date)) +
  geom_line(aes(y = Weekly_Sales, color = "Weekly Sales"), size = 1) +
  geom_line(aes(y = Rolling_Avg, color = "4-Week Avg"), size = 1, linetype = "dashed") +
  labs(
    title = "Magaza 33 Haftal??k Sat??slar?? ve Hareketli Ortalamas??", 
    x = "Tarih", y = "Haftal??k Sat???? ($)"
  ) +
  scale_color_manual(
    values = c("Weekly Sales" = "blue", "4-Week Avg" = "orange")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", color = "gray")
  )










