
library(terra)
library(ggplot2)
library(dplyr)

#-----
# 1. 讀取分別代表「範圍 (Range)」與「數量 (Count)」的年平均光柵
# 假設這兩個光柵都包含 2021 至 2025 年對應的多個圖層
r_range <- rast("Report of Foresty_20261008/heatMap/aggregated/yearly_average_range.tif")
r_count <- rast("Report of Foresty_20261008/heatMap/aggregated/yearly_average_count.tif")
r_abundance <- rast("Report of Foresty_20261008/heatMap/aggregated/yearly_average_abundance.tif")


# 2. 逐格、逐年相乘：每格的 range * 每格的 count
# terra 會自動對應相同年份（圖層）進行相乘
r_range_times_count <- r_range * r_count

# 3. 使用 global() 批次計算每年相乘後的總和
yearly_sums <- global(r_range_times_count, fun = "sum", na.rm = TRUE)
yearly_count_sums <- global(r_count, fun = "sum", na.rm = TRUE)
yearly_abundance_sums <- global(r_abundance, fun = "sum", na.rm = TRUE)


# 4. 將結果整理成乾淨的資料框 (data.frame)
yearly_summary <- data.frame(
  Year            = names(r_range_times_count), # 抓取年份圖層名稱
  Total_count = yearly_count_sums$sum ,            # 取出加總數值
  Total_Abundance = yearly_abundance_sums$sum,             # 取出加總數值
  Total_range_count = yearly_sums$sum             # 取出加總數值
)

# 確保年份欄位是字串，以便後續繪圖
yearly_summary$Year <- as.character(yearly_summary$Year) %>%
  str_remove_all(., "X")

# 5. 印出結果並存成 CSV 檔

openxlsx::write.xlsx(
  yearly_summary, 
  "Report of Foresty_20261008/heatMap/aggregated/yearly_summary.xlsx"
)



