data <- "heartatk.csv"
Heart_Table <- read.csv(data)

Sex <- Heart_Table$SEX
Died <- Heart_Table$DIED
MyTable <- xtabs(~Sex+Died)

install.packages("epitools")

#計算oddsratio
epitab(MyTable)

#計算relative risk (risk ratio)
riskratio(MyTable)

#卡方檢定
RESULTS <- chisq.test(MyTable)

#檢視列聯表期望值
RESULTS$expected
