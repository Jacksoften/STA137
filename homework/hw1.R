# linear regression assumptions
# 1. equal variance
# 2. normality of errors
# 3. uncorrelated errors
# 4. independent responses
path = "/home/yunzheli/class/"
setwd(path)
bodyFat = read.table("sta137/bodyfat.R", stringsAsFactors = FALSE)
electrical_bill = read.table("sta137/electrical-bills.R", stringsAsFactors = FALSE)

colnames(bodyFat) = c("DietaryFat", "BodyFat")
bodyFat
bfModel = lm(BodyFat ~ DietaryFat, data = bodyFat)
bfModel$coefficients
plot(BodyFat ~ DietaryFat, data = bodyFat)
res = resid(bfModel)
qqnorm(res)
qqline(res)
hist(res)
# No auto-correlation
acf(res)
summary(bfModel)


colnames(electrical_bill) = c("monthlyBill", "monthlyIncome", "people", "size")
electrical_bill 
ebModel = lm(monthlyBill ~ monthlyIncome + people + size, data = electrical_bill)
anova(ebModel)
summary(ebModel)
ebModel
