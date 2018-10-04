train <- read.csv("Training_dataset_Original.csv")
str(train)

summary(train)


#Removing variables

train <- train[,c(-1)]

train <- train[,c(-11,-15,-17,-18,-21,-22,-23,-30,-31,-35,-40,-41,-45,-46)]

summary(train)

#Dividing the data into charging and lending

charge <- subset(train,mvar47=="C")

summary(charge)
summary(Lending)

#Firstly predicting for Charge card

charge <- charge[,c(-14,-17)]


summary(charge)

#Imputing Missing values
library("mice")
set.seed(144)

imputed = complete(mice(charge[,c(1,6,7,8,9,10)]))

charge$mvar1 <- imputed$mvar1
charge$mvar6 <- imputed$mvar6
charge$mvar7 <- imputed$mvar7
charge$mvar8 <- imputed$mvar8
charge$mvar9 <- imputed$mvar9
charge$mvar10 <- imputed$mvar10

set.seed(144)
imputed = complete(mice(charge[,c(2,19,23,29)]))

charge$mvar2 <- imputed$mvar2
charge$mvar28 <- imputed$mvar28
charge$mvar34 <- imputed$mvar34
charge$mvar43 <- imputed$mvar43

set.seed(144)
imputed = complete(mice(charge[,c(11,12,17,21,25)]))

charge$mvar12 <- imputed$mvar12
charge$mvar13 <- imputed$mvar13
charge$mvar32 <- imputed$mvar32
charge$mvar37 <- imputed$mvar37
charge$mvar26 <- imputed$mvar26

set.seed(144)
imputed = complete(mice(charge[,c(15,20,23,24,26,27,29)]))

charge$mvar20 <- imputed$mvar20
charge$mvar29 <- imputed$mvar29
charge$mvar36 <- imputed$mvar36
charge$mvar38 <- imputed$mvar38
charge$mvar39 <- imputed$mvar39

set.seed(144)
imputed = complete(mice(charge[,c(3,4,5,28)]))

charge$mvar3 <- imputed$mvar3
charge$mvar4 <- imputed$mvar4
charge$mvar5 <- imputed$mvar5
charge$mvar42 <- imputed$mvar42

set.seed(144)
imputed = complete(mice(charge[,c(16,18)]))
charge$mvar25 <- imputed$mvar25
charge$mvar27 <- imputed$mvar27

set.seed(144)
imputed = complete(mice(charge[,c(24,30)]))

charge$mvar44 <- imputed$mvar44

set.seed(144)
imputed = complete(mice(charge[,c(14,15)]))
charge$mvar19 <- imputed$mvar19

set.seed(144)
imputed = complete(mice(charge[,c(2,22)]))
charge$mvar33 <- imputed$mvar33


charge <- charge[,-31]

#Applying Random Forest

library("randomForest")

charge$default_ind <- as.factor(charge$default_ind)


model9 <- randomForest( default_ind ~ .,data = charge,ntree=500,mtry=6)

test <- read.csv("Evaluation_dataset.csv")

test <- test[,c(-1)]

test <- test[,c(-11,-15,-17,-18,-21,-22,-23,-30,-31,-35,-40,-41,-45,-46)]

chargeTest <- subset(test,mvar47=="C")
LendingTest <- subset(test,mvar47=="L")

chargeTest <- chargeTest[,c(-14,-17)]


summary(chargeTest)

#Imputing Missing values
library("mice")
set.seed(144)

imputed = complete(mice(chargeTest[,c(1,6,7,8,9,10)]))

chargeTest$mvar1 <- imputed$mvar1
chargeTest$mvar6 <- imputed$mvar6
chargeTest$mvar7 <- imputed$mvar7
chargeTest$mvar8 <- imputed$mvar8
chargeTest$mvar9 <- imputed$mvar9
chargeTest$mvar10 <- imputed$mvar10

set.seed(144)
imputed = complete(mice(chargeTest[,c(2,19,23,29)]))

chargeTest$mvar2 <- imputed$mvar2
chargeTest$mvar28 <- imputed$mvar28
chargeTest$mvar34 <- imputed$mvar34
chargeTest$mvar43 <- imputed$mvar43

set.seed(144)
imputed = complete(mice(chargeTest[,c(11,12,17,21,25)]))

chargeTest$mvar12 <- imputed$mvar12
chargeTest$mvar13 <- imputed$mvar13
chargeTest$mvar32 <- imputed$mvar32
chargeTest$mvar37 <- imputed$mvar37
chargeTest$mvar26 <- imputed$mvar26

set.seed(144)
imputed = complete(mice(chargeTest[,c(15,20,23,24,26,27,29)]))

chargeTest$mvar20 <- imputed$mvar20
chargeTest$mvar29 <- imputed$mvar29
chargeTest$mvar36 <- imputed$mvar36
chargeTest$mvar38 <- imputed$mvar38
chargeTest$mvar39 <- imputed$mvar39

set.seed(144)
imputed = complete(mice(chargeTest[,c(3,4,5,28)]))

chargeTest$mvar3 <- imputed$mvar3
chargeTest$mvar4 <- imputed$mvar4
chargeTest$mvar5 <- imputed$mvar5
chargeTest$mvar42 <- imputed$mvar42

set.seed(144)
imputed = complete(mice(chargeTest[,c(16,18)]))
chargeTest$mvar25 <- imputed$mvar25
chargeTest$mvar27 <- imputed$mvar27

set.seed(144)
imputed = complete(mice(chargeTest[,c(24,30)]))

chargeTest$mvar44 <- imputed$mvar44

set.seed(144)
imputed = complete(mice(chargeTest[,c(14,15)]))
chargeTest$mvar19 <- imputed$mvar19

set.seed(144)
imputed = complete(mice(chargeTest[,c(2,22)]))
chargeTest$mvar33 <- imputed$mvar33


chargeTest <- chargeTest[,-31]


pred2 <- predict(model9,newdata=chargeTest,type = "prob")

library("xlsx")

write.xlsx(pred2,"C:/Users/lenovo/Documents/American Express 2018/Solutions/133.xlsx")

#Now predicting for Lending Card
train <- read.csv("Training_dataset_Original.csv")
str(train)

summary(train)


#Removing variables

train <- train[,c(-1)]

train <- train[,c(-6,-11,-15,-17,-18,-21,-22,-23,-24,-30,-31,-35,-40,-41,-45,-46)]

Lending <- subset(train,mvar47=="L")

Lending <- Lending[,c(-8,-11,-13,-30)]

summary(Lending)

library("mice")

#Imputing Missing values


set.seed(144)

imputed = complete(mice(Lending[,c(1,6,7,8)]))

Lending$mvar1 <- imputed$mvar1
Lending$mvar7 <- imputed$mvar7
Lending$mvar8 <- imputed$mvar8
Lending$mvar10 <- imputed$mvar10

imputed = complete(mice(Lending[,c(2,16,20)]))

Lending$mvar2 <- imputed$mvar2
Lending$mvar28 <- imputed$mvar28
Lending$mvar34 <- imputed$mvar34

imputed = complete(mice(Lending[,c(14,15,18,22)]))

Lending$mvar32 <- imputed$mvar32
Lending$mvar37 <- imputed$mvar37
Lending$mvar26 <- imputed$mvar26
Lending$mvar27 <- imputed$mvar27

imputed = complete(mice(Lending[,c(21,23,24,26)]))

Lending$mvar36 <- imputed$mvar36
Lending$mvar38 <- imputed$mvar38
Lending$mvar39 <- imputed$mvar39
Lending$mvar43 <- imputed$mvar43

imputed = complete(mice(Lending[,c(3,4,5,19,25)]))

Lending$mvar3 <- imputed$mvar3
Lending$mvar4 <- imputed$mvar4
Lending$mvar5 <- imputed$mvar5
Lending$mvar42 <- imputed$mvar42
Lending$mvar33 <- imputed$mvar33

imputed = complete(mice(Lending[,c(9,12,13,17)]))

Lending$mvar12 <- imputed$mvar12
Lending$mvar20 <- imputed$mvar20
Lending$mvar29 <- imputed$mvar29
Lending$mvar25 <- imputed$mvar25

Lending <- Lending[,-27]

#Applying Random Forest

library("randomForest")
Lending$default_ind <- as.factor(Lending$default_ind)
model5 <- randomForest(default_ind ~ .,data = Lending,ntree = 600,mtry=6)

test <- read.csv("Evaluation_dataset.csv")
summary(test)

test <- test[,c(-1)]

test <- test[,c(-6,-11,-15,-17,-18,-21,-22,-23,-24,-30,-31,-35,-40,-41,-45,-46)]

chargeTest <- subset(test,mvar47=="C")
LendingTest <- subset(test,mvar47=="L")
LendingTest <- LendingTest[,c(-8,-11,-13,-30)]

set.seed(144)

imputed = complete(mice(LendingTest[,c(1,6,7,8)]))

LendingTest$mvar1 <- imputed$mvar1
LendingTest$mvar7 <- imputed$mvar7
LendingTest$mvar8 <- imputed$mvar8
LendingTest$mvar10 <- imputed$mvar10

imputed = complete(mice(LendingTest[,c(2,16,20)]))

LendingTest$mvar2 <- imputed$mvar2
LendingTest$mvar28 <- imputed$mvar28
LendingTest$mvar34 <- imputed$mvar34

imputed = complete(mice(LendingTest[,c(14,15,18,22)]))

LendingTest$mvar32 <- imputed$mvar32
LendingTest$mvar37 <- imputed$mvar37
LendingTest$mvar26 <- imputed$mvar26
LendingTest$mvar27 <- imputed$mvar27

imputed = complete(mice(LendingTest[,c(21,23,24,26)]))

LendingTest$mvar36 <- imputed$mvar36
LendingTest$mvar38 <- imputed$mvar38
LendingTest$mvar39 <- imputed$mvar39
LendingTest$mvar43 <- imputed$mvar43

imputed = complete(mice(LendingTest[,c(3,4,5,19,25)]))

LendingTest$mvar3 <- imputed$mvar3
LendingTest$mvar4 <- imputed$mvar4
LendingTest$mvar5 <- imputed$mvar5
LendingTest$mvar42 <- imputed$mvar42
LendingTest$mvar33 <- imputed$mvar33

imputed = complete(mice(LendingTest[,c(9,12,13,17)]))

LendingTest$mvar12 <- imputed$mvar12
LendingTest$mvar20 <- imputed$mvar20
LendingTest$mvar29 <- imputed$mvar29
LendingTest$mvar25 <- imputed$mvar25

LendingTest <- LendingTest[,-27]

pred1 = predict(model5,newdata=LendingTest,type = "prob")
write.xlsx(pred1,"C:/Users/lenovo/Documents/American Express 2018/Solutions/144.xlsx")






