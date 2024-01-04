csv_file <- "metabolicSyndrome.csv"

df <- read.csv(csv_file, sep=",", header = TRUE, stringsAsFactors = TRUE)

summary(df)
str(df)