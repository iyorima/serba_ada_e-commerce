# Install package
install.packages("readr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("rfm")
# Load library
library(readr)
library(dplyr)
library(lubridate)
library(rfm)

# Baca file CSV
dataset <- read_csv("dataset UTS.csv")

# Mengubah data yang bernilai ? menjadi NA
dataset[dataset == "?"] <- NA

# Cek keberadaan missing value pada setiap variabel
missing_values <- colSums(is.na(dataset))
print("Data yang hilang sebelum Pembersihan Data:")
print(missing_values)

# Imputasi missing value pada variabel TransactionAmount dengan median
dataset$TransactionAmount <- as.numeric(dataset$TransactionAmount)

median_transaction_amount <- median(dataset$TransactionAmount, na.rm = TRUE)
dataset$TransactionAmount[is.na(dataset$TransactionAmount)] <- median_transaction_amount
print(median_transaction_amount)

# Buang record yang memiliki missing value pada variabel lainnya
dataset <- na.omit(dataset)

dataset$PurchaseDate <- as.Date(dataset$PurchaseDate, "%d/%m/%Y")

# Cek kembali keberadaan missing value setelah pembersihan
missing_values_after_cleaning <- colSums(is.na(dataset))
print("\nData yang hilang setelah Pembersihan Data:")
print(missing_values_after_cleaning)

#==================== RFM ======================
# date analysis H+1 setelah tanggal terakhir transaksi (10 Juni 2023)
analysis_date <- as.Date("11/06/2023", "%d/%m/%Y")

dataset_rfm <- dataset %>% 
  group_by(CustomerID) %>% 
  summarise(
    Recency_days = analysis_date - max(PurchaseDate),
    Frequency = n(),
    Monetary = sum(TransactionAmount)
  ) %>%
  ungroup()

rfm_score <- rfm_table_customer(data = dataset_rfm, 
                                customer_id = CustomerID,
                                n_transactions = Frequency,                                recency_days = Recency_days,
                                total_revenue = Monetary, 
                                analysis_date = analysis_date,
                                recency_bins = 5,frequency_bins = 5, monetary_bins = 5)