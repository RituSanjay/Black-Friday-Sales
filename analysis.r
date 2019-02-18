library("bindr")
library("caret")
library("crayon")
library("curl")
library("DescTools")
library("dplyr")
library("e1071")
library("GGally")
library("ggplot2")
library("mongolite")
library("plotly")
library("plyr")
library("reshape")
library("reshape2")
library("rlang")
library("scatterplot3d")
library("utils")
library("tidyverse")

#import the mongoDB dataset
m <- mongo(collection = "blackfriday",db="assignment_3",url="mongodb://localhost")
blackfriday <- m$find()

#****************************************************#

#charts and plots to gain basic understanding of data

# 1) gender vs purchase

# first we query from the db a subset containing the total purchase per gender 
# and average purchase (by gender)
# then convert this to a r-dataset
# finally plot this on a barchart

m$mapreduce(
            mapfunction = function(){
              emit(this.Gender,this.Purchase)
            },
            reducefunction = function(varG,varP){
              return Array.avg(varP)
            },
            {out:}
)

# 2) age vs purchase

# 3) occupation vs purchase

# 4) city_category vs purchase


#****************************************************#

#queries to extract relevant tables (subsets) for further analysis

# 1) find number of products

length(m$distinct("Product_ID"))

# 2) find list of distinct age groups

m$distinct("Age")

# 3) find list of distinct ocupations

sort(m$distinct("Occupation"))

# 4) find list of unique city_category

m$distinct("City_Category")

# 5) find unique 3-level product category combination

sort(m$distinct("Product_Category_1"))
sort(as.numeric(m$distinct("Product_Category_2")))
sort(as.numeric(m$distinct("Product_Category_3")))

#> var distinctProductTags = {$group: { _id: {tag1:"$Product_Category_1",
#tag2:"$Product_Category_2",tag3:"$Product_Category_3"}}}
#> db.blackfriday.aggregate([distinctProductTags])

m$aggregate('[
             {"$group":
                 { "_id":
                     {"tag1":"$Product_Category_1",
                      "tag2":"$Product_Category_2",
                      "tag3":"$Product_Category_3"}
                 }
               }
]')

# 6) find no of customers in each age group by gender

#db.blackfriday.aggregate([
#  {$group: {_id: {gender:"$Gender",age:"$Age"},
#    num:{$sum: 1}}},
#  {$sort: {Gender:1}}
#  ])

m$aggregate('[
    {"$group": {"_id": {"gender":"$Gender","age":"$Age"},
      "numOfCust":{"$sum": 1}}},
    {"$sort": {"Gender":1}}
    ]')

# 7) find average and total spending by each age group

#> db.blackfriday.aggregate([
#  { $group: {_id :"$Age",
#    tot_sales: {$sum: "$Purchase"},
#    avg_sales: {$avg: "$Purchase"}
#  }} ])

ageGrpSpending <- m$aggregate('[
            {"$group": 
                 { "_id": "$Age",
                  "tot_sales": {"$sum": "$Purchase"},
                  "avg_sales": {"$avg": "$Purchase"}
                 }
}]')


# 8) find average and total spending by each gender

genderGrpSpending <- m$aggregate('[
            {"$group": 
                              { "_id": "$Gender",
                              "tot_sales": {"$sum": "$Purchase"},
                              "avg_sales": {"$avg": "$Purchase"}
                              }
                              }]')

# 9) find average and total spending by each occupation

occnGrpSpending <- m$aggregate('[
            {"$group": 
                              { "_id": "$Occupation",
                              "tot_sales": {"$sum": "$Purchase"},
                              "avg_sales": {"$avg": "$Purchase"}
                              }
                              }]')



# 10) find average and total spending by each city category

m$aggregate('[
            {"$group": 
                              { "_id": {"cityCategory":"$City_Category",
                                        "noOfYrsInCity":"$Stay_In_Current_City_Years"},
                              "tot_sales": {"$sum": "$Purchase"},
                              "avg_sales": {"$avg": "$Purchase"}
                              }
                              }]')



#****************************************************#

#find correlations between the data
corr <- ggpairs(blackfriday[,c(7,12)],aes(color=status,alpha=0.4))
print(corr)

bf <-blackfriday
dmy <- dummyVars(" ~ .", data = bf)
trsf <- data.frame(predict(dmy, newdata = bf))
cormat <- round(cor(bf[,c(3:12)]),2)
head(cormat)

#****************************************************#

#checking hypothesis

# first change data types
bf$Product_ID = as.factor(bf$Product_ID)
bf$Gender= as.factor(bf$Gender)
bf$Age = as.factor(bf$Age)
bf$City_Category =as.factor(bf$City_Category)
bf$Stay_In_Current_City_Years= as.factor(bf$Stay_In_Current_City_Years)
bf$Product_Category_2 =as.integer(bf$Product_Category_2)
bf$Product_Category_3 = as.integer(bf$Product_Category_3)
str(bf)

#find missing values
t(colSums(is.na(bf)))

# 1) women are more likely to spend
femalePurchase <- bf %>% filter(Gender =="F") %>% select(Purchase)
malePurchase <- bf %>% filter(Gender =="M") %>% select(Purchase)
t.test(malePurchase,femalePurchase, alternative = "less")
  ###implies that men are more likely to spend more
ggplot(aes(x=Gender,y=Purchase,fill=Gender),data=bf)+
  geom_boxplot()+ggtitle("Black Friday Purchases:Men vs Women")


# 2) married couples spend more on black friday sales
single <-bf %>% filter(Marital_Status == 0) %>% select(Purchase)
married <-bf %>% filter(Marital_Status == 1) %>% select(Purchase)
t.test(single,married)
  ###implies taht there is no significant difference between 
    #married or single customers
ggplot(aes(x=factor(Marital_Status),y=Purchase,fill=Gender),data=bf)+
  geom_boxplot()+ggtitle("Black Friday Purchases:Married vs Single")

# 3) how does age impact spending
diffSpend <- aov(Purchase~Age,data=bf)
summary(diffSpend)
  ### implies that the Pr(>F) betwen the different ranges is too small
    # and hence there is an impact on purchase 
#pairwise.t.test(bf$Purchase,as.factor(bf$Age))
ggplot(aes(x=Age,y=Purchase,fill=Age),data=bf)+
  geom_boxplot()+ggtitle("Black Friday Purchases:Who Shall We Target")

# 4) does it(spending) matter which part of the city they live in
citySpend <- aov(Purchase~City_Category,data=bf)
summary(citySpend)
  ###implies that the area of residence in the city affects amt of spending
ggplot(aes(x=City_Category,y=Purchase,fill=City_Category),data=bf)+
  geom_boxplot()+ggtitle("Black Friday Purchases:City Divisions")

# 5) does how long they have been at their current residences impact spending
resiSpend <- aov(Purchase~Stay_In_Current_City_Years,data=bf)
summary(resiSpend)
   ### implies no significant difference
ggplot(aes(x=Stay_In_Current_City_Years,y=Purchase,fill=City_Category),data=bf)+
  geom_boxplot()+ggtitle("Black Friday Purchases:Years of Residence")

# 6) do people in certain occupations tend to spend more
occuSpend<-aov(Purchase~Occupation,data=bf)
summary(occuSpend)
ggplot(aes(x=as.factor(Occupation),y=Purchase,fill=Gender),data=bf)+
  geom_boxplot()+ggtitle("Black Friday Purchases:Occupation")


# 7)Men spend more than women,btw ages 51-55,living in area C,engaged in occupation 12
bf %>% filter(Age=="51-55",City_Category=="C",Occupation=="17") %>%
  ggplot(aes(x=Gender,y=Purchase,fill =as.factor(Marital_Status)))+geom_boxplot()+
  ggtitle("Black Friday Purchases:Occupation-17")
   ### guess we could say that our hypothesis is true visually ie.
f<-bf %>% filter(Age=="51-55",City_Category=="C",Gender=="F") %>% select(Purchase)
m<-bf %>% filter(Age=="51-55",City_Category=="C",Gender=="M") %>% select(Purchase)
t.test(f,m,alternative = "less")
#****************************************************#

#linear regression to predict purchase price

summary(blackfriday)
str(bf)
#find number of missing values
sapply(bf,function(x) sum(is.na(x)))
#dependent variable : blackfriday$purchase
#independent categorical variable : blackfriday$Gender,blackfriday$Age,
#blackfriday$Occupation, blackfriday$City_Category, blackfriday$Marital_Status,
#independent variable:blackfriday$Stay_In_Current_City_Years

sample <- sample(1:nrow(bf),size=floor(nrow(bf)*0.7))
train<-bf[sample,]
test<-bf[-sample,]

lr_mod1 <- lm(Purchase~
                Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+
                Marital_Status+Product_Category_1,data = train)
summary(lr_mod1)
step<-step(lr_mod1)

train_pred_pur <- predict(lr_mod1,newdata = train)
tr <- cbind(train,train_pred_pur)
test_pred_pur <- predict(lr_mod1,newdata = test)
te <- cbind(test,test_pred_pur)

#checking model accuracy by MAPE
(mean(abs((tr$Purchase-tr$train_pred_pur)/tr$train_pred_pur)))
(mean(abs((te$Purchase-te$test_pred_pur)/te$test_pred_pur)))
#checing model accuracy by RMSE
(sqrt(mean(tr$Purchase-tr$train_pred_pur)**2))
(sqrt(mean(te$Purchase-te$test_pred_pur)**2))
  ############################################


#****************************************************#
