# Using web scrapting technology to get data from google directly
# avoid manually clicking.

# library(RGoogleTrends)
# 3-31. cannot make it work yet.

# using R package gtrendsR
library(gtrendsR)
data = gtrends("League of Legends", time='all')
hits = data$interest_over_time$hits
date = data$interest_over_time$date
date = sapply(date, function(x) gsub('[-0-9]{3}$', '', x))

result_data = cbind(hits, date)
saveAs = sprintf("league%s.rds", date[length(date)])
savePath = sprintf("../data/%s", saveAs)
if(is.na(match(saveAs, list.files('../data')))){
  saveRDS(result_data, file=savePath) 
}else{
  cat(saveAs, " exits", "\n")
}
