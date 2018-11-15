### MySQL to R -- ZOO LAB

library(RMySQL)
library(RPostgreSQL)
library(RSQLite)
library(sqldf)
library(ggplot2)

conn <- dbConnect(MySQL(), user = 'lerbay', password = 'tJU7dnLQ',
                  host = 'da2017septft.cks5ya9qdcwp.us-east-2.rds.amazonaws.com',
                  port = 3306, dbname='lerbay_zoo_lab');

dbtables <- dbListTables(conn)
dbtables

weeks_per_anm <- dbReadTable(conn, 'weeks_per_anm')

weeks_per_anm$cal_date <- as.Date(weeks_per_anm$cal_date)


p <- ggplot(data = weeks_per_anm) + 
    geom_line(aes(x = cal_date, y = weight, group= id))
print(p)
