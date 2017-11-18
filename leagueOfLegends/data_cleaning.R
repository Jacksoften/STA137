# cleaning the data set
setwd("/home/yunzheli/class/sta137/leagueOfLegends/")

lol = read.table("leagueoflegends.csv", stringsAsFactors = FALSE, skip = 3)
# dummy variable for championship month

# clean data
# seperate string with ','
sep_lol = lapply(lol, function(x) strsplit(x, ","))

# unlist
sep_lol = unlist(sep_lol)

# choose numbers
lol_num = as.numeric(sep_lol[2*(1:(length(sep_lol)/2))])
lol_date = sep_lol[2*(1:(length(sep_lol)/2)) - 1]
lol_t = ts(lol_num, start = 2004, end = 2017, freq = 12)
plot(lol_t, main = "League of Legends", sub = "made by Yunzhe Li", xlab = "time", ylab = "frequnce")


# From wiki, League of Legends was released at October 27, 2009
# by looking at the plot, I am only interested in the data start from 2009-11
# or in another words, the data which is larger than 10

index = grep("2009-10", unlist(lol))
lol_num_filtered = lol_num[-(1:index)]
lol_date_filtered = lol_date[-(1:index)]
names(lol_date_filtered) = NULL
lol_data = cbind(lol_num_filtered, lol_date_filtered)

saveRDS(lol_data, "league")
