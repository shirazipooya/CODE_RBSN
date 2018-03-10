# Extract Data from RBSN Stations:
# Address: http://reports.irimo.ir/jasperserver/login.html
# User Name: user
# Password: user

rm(list = ls())

# Load library:
library(dplyr)
library(ggplot2)

# Set Current Directori to RBSN_Data Folder:
setwd(dir = choose.dir(default = getwd(), caption = "Select RBSN Data Folder (*.csv): "))

# Read Data:
Data <- read.csv(file = "Mashhad_40745.csv", header = TRUE, skip = 2)

# Modified Data:
  # remove X cloumn:
Data$X <- NULL
  # remove empty rows:
Data <- Data[-which(x = Data$station_i == "" | Data$station_i == "station_i"), ]
  # reset row names:
row.names(Data) <- NULL

# Remove "null", "nul" and "n" from Data:
Data[Data == "null"] <- NA
Data[Data == "n"] <- NA
Data[Data == "nul"] <- NA

# Reset Factor Level:
Data <- droplevels.data.frame(Data)

# Rename of date column:
colnames(Data)[5] <- "date"

# Change Class of date Cloumn:
Data$date <- as.POSIXct(x = as.character(x = Data$date), format = "%m/%d/%Y %H:%M")

# Change Class of Clumns:
Column.Names <- colnames(Data)[-c(1,5)]
for (ColName in Column.Names) {
  eval(expr = parse(text = paste("Data$", ColName, " <- as.numeric(as.character(x = Data$",
                                 ColName, "))", sep = "")))
}

# Group Date Variable into Day/Month/Year:
A <- Data %>%
  mutate(month = format(x = date, "%m"), year = format(x = date, "%Y")) %>%
  group_by(month, year) %>%
  summarise(total = sum(rrr, na.rm = TRUE))

B <- Data %>%
  mutate(year = format(x = date, "%Y")) %>%
  group_by(year) %>%
  summarise(total = sum(rrr, na.rm = TRUE))

# Order Data by the Year:
A <- A[order(A$year),]

A$date <- seq(from = as.Date("1951/01/01"), to = as.Date("2017/12/01"), by = "month")

B <- B[order(A$year),]

B$date <- seq(from = as.Date("1951/01/01"), to = as.Date("2017/12/01"), by = "year")

ggplot(data = B, mapping = aes(x = year, y = total, group = 1)) +
  geom_point() +
  geom_line(linetype = "dashed", color="red")







