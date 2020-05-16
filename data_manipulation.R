#working directory
path <- "/home/ahmed/Projects/R-from-scratch"

#set working directory
setwd(path)

library(dplyr)
library(dummies)

a <- combi %>% 
  group_by(Outlet_Identifier)%>%
  tally()

head(a)

names(a)[2] <- "Outlet_Count"
combi <- full_join(a, combi, by = "Outlet_Identifier")

b <- combi%>%
  group_by(Item_Identifier)%>%
  tally()

names(b)[2] <- "Item_Count"
head (b)
combi <- merge(b, combi, by = "Item_Identifier")
combi <- dummy.data.frame(combi, names = c('Outlet_Size','Outlet_Location_Type','Outlet_Type', 'Item_Type_New'),  sep='_')
str (combi)



c <- combi%>%
  select(Outlet_Establishment_Year)%>% 
  mutate(Outlet_Year = 2013 - combi$Outlet_Establishment_Year)

head(c)
combi <- full_join(c, combi)

q <- substr(combi$Item_Identifier,1,2)
q <- gsub("FD","Food",q)
q <- gsub("DR","Drinks",q)
q <- gsub("NC","Non-Consumable",q)
table(q)
combi$Item_Type_New <- q

combi$Item_Fat_Content <- ifelse(combi$Item_Fat_Content == "Regular",1,0)
sample <- select(combi, Outlet_Location_Type)
demo_sample <- data.frame(model.matrix(~.-1,sample))
head(demo_sample)
