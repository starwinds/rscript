# make days and choose only working days
days = seq(as.Date("2016-01-01"),as.Date("2016-12-31"),by="1 day")
library(chron)
weekDays=days[!is.weekend(days)]
