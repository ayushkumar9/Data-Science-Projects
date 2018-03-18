
train <- read.csv("DPSD.csv")
str(train)
train$dob <- as.numeric(train$dob)
train$gn <- as.numeric(train$gn)
train$ln <- as.character(train$ln)
train$fn <- as.character(train$fn)
a <- as.data.frame(strsplit(train$ln," "))
for(i in 1:103)
{
  train$ln[i] <- as.character(a[1,i])
}
b <- as.data.frame(strsplit(train$fn," "))
for(i in 1:103)
{
  train$fn[i] <- as.character(b[1,i])
}
for(i in 1:102)
{
  
  for(j in (i+1):103)
  {
  if((train$dob[i] == train$dob[j]) && (train$gn[i] == train$gn[j]) && (train$ln[i] == train$ln[j]) && (train$fn[i] == train$fn[j]))
      {
        train[i,] <- 0
        
      }
  }
  
}

p=numeric(0)
for(i in 1:103)
{
  
  if(train[i,1] == 0){
  p<-c(p,i)
  
  }
   
  
}
train <- train[-p,]

write.csv(train,"C:/Users/ayush.kumar/Documents/Innovaccer/output.csv")