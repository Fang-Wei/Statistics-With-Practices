data <- "heartatk.csv"
Heart_Table <- read.csv(data)

Sex <- Heart_Table$SEX
Died <- Heart_Table$DIED
MyTable <- xtabs(~Sex+Died)

install.packages("epitools")

#�p��oddsratio
epitab(MyTable)

#�p��relative risk (risk ratio)
riskratio(MyTable)

#�d���˩w
RESULTS <- chisq.test(MyTable)

#�˵��C�p�������
RESULTS$expected