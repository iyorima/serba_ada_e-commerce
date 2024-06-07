# Install package hanya digunakan ketika library belum terinstall
install.packages("readr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("rfm")
install.packages("ggplot2")
install.packages("plotly")

# Load library
library(readr)
library(dplyr)
library(lubridate)
library(rfm)
library(plotly)
library(ggplot2)

# Baca file CSV
df <- read_csv("dataset UTS.csv")

# Mengubah data yang bernilai ? menjadi NA
df[df == "?"] <- NA

# Cek keberadaan missing value pada setiap variabel
# Terdapat 39 missing value pada PurchaseDate dan 
# 48 missing value pada TransactionAmount
print("Data yang hilang sebelum Pembersihan Data:")
print(colSums(is.na(df)))

# Mengubah nama kolom CustomerID menjadi customer_id,
# karena library rfm hanya menerima nama variabel tersebut
colnames(df)[1] <- "customer_id"

# Imputasi missing value pada variabel TransactionAmount dengan median
df$TransactionAmount <- as.numeric(df$TransactionAmount)

median_transaction_amount <- median(df$TransactionAmount, na.rm = TRUE)

df$TransactionAmount[is.na(df$TransactionAmount)] <- median_transaction_amount

print(median_transaction_amount)

# Membuang baris yang memiliki missing value pada variabel lainnya
df <- na.omit(df)

# Mengubah tipe data PurchaseDate menjadi Date dengan format tanggal/bulan/tahun
df$PurchaseDate <- as.Date(df$PurchaseDate, "%d/%m/%Y")

# Cek kembali keberadaan missing value setelah pembersihan
print("\nData yang hilang setelah Pembersihan Data:")
print(colSums(is.na(df)))

# Transformasi dataset menjadi data frame
df <- data.frame(df)

# ==================== RFM ======================
# date analysis ditentukan H+1 setelah tanggal terakhir transaksi (10 Juni 2023)
analysis_date <- as.Date("11/06/2023", "%d/%m/%Y")

# Menghitung Recency, Frequency, and Monetary values
df_rfm <- df %>% 
  group_by(customer_id) %>% 
  summarise(
    recency = as.numeric(analysis_date - max(PurchaseDate)),
    frequency = n_distinct(OrderID), 
    monetary= sum(TransactionAmount, na.rm = TRUE))

# Menghitung skor Recency, Frequency, Monetery dengan nilai 1 s.d. 5.
df_rfm_final <- rfm_table_customer(df_rfm, customer_id, 
                                   frequency, recency, 
                                   monetary, analysis_date)

# Segment categories
rfm_segment_names <- c("CHAMPIONS", "LOYAL CUSTOMERS", 
                          "PROMISING", "NEW CUSTOMERS", 
                          "CANT LOSE THEM", "AT RISK", 
                          "NEED ATTENTION", "LOST")

# Segment intervals
recency_lower <-   c(4, 4, 4, 4, 1, 1, 1, 1)
recency_upper <-   c(5, 5, 5, 5, 3, 3, 3, 3)
frequency_lower <- c(4, 4, 1, 1, 4, 4, 1, 1)
frequency_upper <- c(5, 5, 3, 3, 5, 5, 3, 3)
monetary_lower <-  c(4, 1, 4, 1, 4, 1, 4, 1)
monetary_upper <-  c(5, 3, 5, 3, 5, 3, 5, 3)

# generate segments
rfm_segments <- rfm_segment(df_rfm_final, rfm_segment_names, recency_lower,
                        recency_upper, frequency_lower, frequency_upper, monetary_lower,
                        monetary_upper)

# RFM Segment Overview
rfm_segments

rfm_segment_overview <- rfm_segment_summary(rfm_segments)

rfm_plot_segment(rfm_segment_overview)

rfm_plot_median_monetary(rfm_segments)
rfm_plot_median_recency(rfm_segments)

rfm_plot_bar_chart(df_rfm_final)

# Peta sebaran segmentasi customers
rfm_plot_segment_scatter(rfm_segments)

# ================== K Means dan Elbow ================== 
# Install package hanya digunakan ketika library belum terinstall
# install.packages('tidyverse')
# install.packages('cluster')
# install.packages('factoextra')

# Load Library
library(tidyverse)  
library(cluster)
library(factoextra)

# Transform data RFM menjadi data frame dan mengubah customer_id menjadi nama baris
df_frame <- data.frame(df_rfm_final$rfm[, c('customer_id',
                                            'recency_score', 
                                            'frequency_score', 
                                            'monetary_score')])

df_transform <- df_frame
df_transform <- df_frame[,-1]
rownames(df_transform) <- df_frame[,1]

# Scaling data
df_kmeans <- scale(df_transform)
                                           
# Metode Elbow - Menghasilkan k-optimal = 6?
# Untuk memastikan nilai k-optimal, perlu dilakukan uji ulang menggunakan algoritma lain
fviz_nbclust(df_kmeans, kmeans, method = "wss")

# Metode Silhouette
# Hasil yang diperoleh dengan Silhouette k-optimal = 4?
fviz_nbclust(df_kmeans, kmeans, method = "silhouette")

kmeans_overview <- kmeans(df_kmeans, 6, nstart = 25)
print(kmeans_overview)
kmeans_overview$centers

# Visualisasi KMeans
fviz_cluster(kmeans_overview, data = df_kmeans)

# Mendapatkan data asli dengan tambahan kolom klasterisasinya
kmeans_clusters <- cbind(df_rfm_final$rfm, cluster = kmeans_overview$cluster)
head(kmeans_clusters)

# Mendapatkan jumlah masing-masing cluster
kmeans_overview$size

# visualisasi
plot(c(1,2,3,4,5,6), kmeans_overview$size)
barplot(kmeans_overview$size,names.arg=c(1,2,3,4,5,6),xlab="Value",ylab="Count",col="blue",
        main="Revenue chart",border="red")

# ================== Hierarchical Clustering ================== 
# Load Library
library(cluster)

# Transform data RFM menjadi data frame dan mengubah customer_id menjadi nama baris
df_frame <- data.frame(df_rfm_final$rfm[, c('customer_id',
                                            'recency_score', 
                                            'frequency_score', 
                                            'monetary_score')])

df_transform <- df_frame
df_transform <- df_frame[,-1]
rownames(df_transform) <- df_frame[,1]

# Scaling data
df_hierarchical = scale(df_transform)

hierarchical_overview <- agnes(x = df_hierarchical,
                   stand = TRUE,
                   metric = "euclidean", 
                   method = "ward")

hierarchical_clusters <- cutree(hierarchical_overview, k = 6)

table(hierarchical_clusters)

# Visualisasi Hierarchical
fviz_dend(hierarchical_overview, k = 6, cex = 0.5)

# Detail setiap cluster
rownames(df_transform)[hierarchical_clusters == 1]
rownames(df_transform)[hierarchical_clusters == 2]
rownames(df_transform)[hierarchical_clusters == 3]
rownames(df_transform)[hierarchical_clusters == 4]
rownames(df_transform)[hierarchical_clusters == 5]
rownames(df_transform)[hierarchical_clusters == 6]

# Melihat rerata nilai RFM tiap cluster
rfm_with_clusters <- cbind(df_rfm_final$rfm, cluster = hierarchical_clusters)

cluster_summary <- rfm_with_clusters %>%
  group_by(cluster) %>%
  summarise(
    avg_recency = mean(recency_score),
    avg_frequency = mean(frequency_score),
    avg_monetary = mean(monetary_score),
    )
print(cluster_summary)
