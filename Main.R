rm(list=ls())

# Work Directory
datDir <- c('E:/work/git/base')
setwd(datDir)

# Trying to find Java

find.java <- function() {
  for (root in c("HLM", "HCU")) for (key in c("Software\\JavaSoft\\Java Runtime Environment",
                                              "Software\\JavaSoft\\Java Development Kit")) {
    hive <- try(utils::readRegistry(key, root, 2),
                silent = TRUE)
    if (!inherits(hive, "try-error"))
      return(hive)
  }
  hive
}

Path.Java <- find.java()
cat(Path.Java[3]$`1.8`$JavaHome)

Sys.setenv(JAVA_HOME=Path.Java[3]$`1.8`$JavaHome)

library(rJava)
library(xlsx)

# Read data
setwd(paste(datDir,"/news",sep=""))

base <- read.xlsx("top-300-connect.xlsx",1,as.data.frame=TRUE, 
                  header=T, stringsAsFactors=FALSE, encoding="UTF-8")

q1 <- read.xlsx("2016-Q1.xlsx",1,as.data.frame=TRUE, 
                  header=T, stringsAsFactors=FALSE, encoding="UTF-8")

q2 <- read.xlsx("2016-Q2.xlsx",1,as.data.frame=TRUE, 
                     header=T, stringsAsFactors=FALSE, encoding="UTF-8")

q3 <- read.xlsx("2016-Q3.xlsx",1,as.data.frame=TRUE, 
                header=T, stringsAsFactors=FALSE, encoding="UTF-8")

q4 <- read.xlsx("2016-Q4.xlsx",1,as.data.frame=TRUE, 
                header=T, stringsAsFactors=FALSE, encoding="UTF-8")

library('sqldf')

# Data integration
base <-
  sqldf("SELECT t.*,t1.ktseli as q1, t2.ktseli as q2, t3.ktseli as q3, t4.ktseli as q4, t4.razryad as grade
        FROM base t
        LEFT OUTER JOIN q1 t1
        ON t.tabelnyi = t1.tabelnyi
        LEFT OUTER JOIN q2 t2
        ON t.tabelnyi = t2.tabelnyi
        LEFT OUTER JOIN q3 t3
        ON t.tabelnyi = t3.tabelnyi
        LEFT OUTER JOIN q4 t4
        ON t.tabelnyi = t4.tabelnyi
        ")

# summary(base)
base$q2 <- as.numeric(base$q2)

# Mean of all Assessments
base$ktseli <- rowMeans(base[c('q1', 'q2','q3','q4')], na.rm=TRUE)

summary(base$ktseli)

# ?????? ?????? ?? ??????
rdata <- base[which(base$ktseli != 'NaN'),]

summary(rdata$ktseli)

med <- median(rdata$ktseli)

rdata$success <- 0

for(i in 1:nrow(rdata))
  if(rdata$ktseli[i] > med) rdata$success[i] <- 1

# Split randomly
data.target.split <- rdata[sample(1:nrow(rdata), nrow(rdata), replace = F),]
data.target.split.train <- data.target.split[1:floor(nrow(data.target.split)*.50), ]
data.target.split.evaluate <- data.target.split[(floor(nrow(data.target.split)*.50)+1):nrow(data.target.split), ]


library(rpart)

# Rpart
data.target.split.rpart <- rpart(data.target.split.train$success ~ flag + soobshenii + loyalnost + vovlechennost, data = data.target.split.train, method = "class", cp = 0.011, na.action = na.rpart)

summary(data.target.split.rpart)


# Размер дерева
plotcp(data.target.split.rpart)

# Графика
plot(data.target.split.rpart,uniform = TRUE)
text(data.target.split.rpart,use.n = TRUE, cex = 0.75)
# printcp(data.target.split.rpart)

# Use the model R-part to predict the evaluation.
data.target.split.evaluate$prediction <- predict(data.target.split.rpart, newdata=data.target.split.evaluate, type="class")
summary(data.target.split.evaluate$prediction)

# Calculate the overall accuracy.
data.target.split.evaluate$correct <- data.target.split.evaluate$prediction == data.target.split.evaluate$success
print(paste("% of predicted classifications correct", 100*mean(data.target.split.evaluate$correct)))
table(data.target.split.evaluate$prediction, data.target.split.evaluate$success)

# Проверка кластеров по дереву решений

che <- data.target.split.evaluate
sqldf("SELECT count(tabelnyi), success,prediction from che t group by success,prediction")

# Если в модели оставить только тех, кого предсказывает, как успешные "1", тогда точность модели составит ~80%
accurasy <- sqldf("SELECT count(tabelnyi), success,prediction from che t group by success,prediction")
print(paste("% of predicted classifications correct", 100*sum(accurasy[which(accurasy$prediction == 1 & accurasy$success == 1),]$`count(tabelnyi)`)/sum(accurasy[which(accurasy$prediction == 1),]$`count(tabelnyi)`)))


# # Save results
setwd('E:/work/git/base/news')

# save(data.target.split.rpart, file = "rpart_model_72.Rdata")
# load("rpart_model_87.Rdata", envir = e <- new.env())
# identical(data.target.split.rpart, e$data.target.split.rpart, ignore.environment = TRUE)
# data.target.split.rpart <- e$data.target.split.rpart
# 
# save(data.target.split, file = "data_target_split_72.Rdata")
# load("data_target_split_87.Rdata", envir = e <- new.env())
# identical(data.target.split, e$data.target.split, ignore.environment = TRUE)
# data.target.split <- e$data.target.split.rpart
# 
# save(data.target.split.evaluate, file = "data_target_split_evaluate_72.Rdata")
# load("data_target_split_evaluate_87.Rdata", envir = e <- new.env())
# identical(data.target.split.evaluate, e$data.target.split.evaluate, ignore.environment = TRUE)
# data.target.split.evaluate <- e$data.target.split.evaluate
# 
# save(data.target.split.train, file = "data_target_split_train_72.Rdata")
# load("data_target_split_train_87.Rdata", envir = e <- new.env())
# identical(data.target.split.train, e$data.target.split.train, ignore.environment = TRUE)
# data.target.split.train <- e$data.target.split.train
