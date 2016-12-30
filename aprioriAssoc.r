#reading data and preprocessing
library(readr)
fertility_Diagnosis <- read_csv("~/Downloads/fertility_Diagnosis.txt",col_names = FALSE)


library(arules)

#classifying age into three groups
fertility_Diagnosis$V2 <- cut(fertility_Diagnosis$V2, breaks= c(0,0.33,0.66,1), labels = c("18-24","24-30","30-36") )
#classifying hours spent sitting into two groups
fertility_Diagnosis$V9 <- cut(fertility_Diagnosis$V9, breaks= c(0,0.50,1), labels = c("less than 8 hours","8-16 hours") )
#converting the data into factors so that it's ready for apriori algorithm
d <- transform(fertility_Diagnosis, V1 = as.factor(V1), V2 = as.factor(V2), V3 = as.factor(V3), V4 = as.factor(V4), V5 = as.factor(V5), V6 = as.factor(V6), V7 = as.factor(V7), V8 = as.factor(V8), V9 = as.factor(V9), V10 = as.factor(V10))

colnames(d) <- c("Seasons", "Age","Childish disease", "Accident/trauma","Surgeries","fevers in past year","alcohol consumption","smoking habbit","hours spent sitting","diagnosis")
head(d)

#generate association rules using apriori
# min supp = 0.01 , conf = 0.8
# keeping right hand side of the rules fixed to focus on patients with altered fertility diagnosis
rules = apriori(d, parameter = list(supp=0.01, conf=0.8), appearance = list(rhs=c("diagnosis=O"), default="lhs"))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

#saving rules found with apriori to CSV file 
write.csv(as(rules.sorted, "data.frame"), file="allrules.csv")

#finding the redundant rules.
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

#saving redundant rules to CSV
write.csv(as(rules.sorted[redundant], "data.frame"), file="redunsantrules.csv")

#removing redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)
#saving final rule-set to CSV
write.csv(as(rules.pruned, "data.frame"), file="rules.csv")

install.packages("arulesViz", type="source")

library(arulesViz)

#visualising the rules
plot(rules.pruned, method="graph")
plot(rules.pruned, method="paracoord")




