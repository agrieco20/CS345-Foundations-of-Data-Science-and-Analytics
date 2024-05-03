#Author: Anthony Grieco
#Date: 3/21/2024
#Class: Foundations of Data Science

library(tidyverse)
library(RMariaDB)

#Connection Information to Access the Database
getConn <- function(){
  con <- dbConnect(
    drv = RMariaDB::MariaDB(),
    host = 'Apollo',
    user = 'agrieco896',
    password = .rs.askForPassword("Database Password:"),
    port = 3306,
    dbname = 'CS345')
  cat(paste("\tHost: ", slot(con,'host'), "\n\tDatabase: ", slot(con,'db')))
  return(con)
}

con <- getConn() #Connects to the database

#------------------------------------------------------------------------
#1. Total Revenue Per Month Across All Products

#Calculates the Total Number of Sales for each individual month and then pulls that information off the data base
query <- "SELECT MONTH(o.order_date) AS Month, SUM(o.order_total) AS 'Total_Sales'
          FROM order_tbl o
          GROUP BY MONTH(o.order_date)"
df <- dbGetQuery(con, query)
df

#Bar Chart Generation (Orders Months with the Most Sales towards the top and the ones with the fewest at the bottom)
df %>% arrange(desc(Month)) %>%
  ggplot(aes(x=Total_Sales, y=reorder(Month, Total_Sales))) +
  geom_bar(stat = "identity", fill="blue", alpha=.6) +
  labs(x="Sales ($)", y = "Month")

#------------------------------------------------------------------------
#2. Total Sales By Age Group (12-17, 18-24, 25-34, 35-44, 45-54, 55-64, 65-74, and 75-84)

#Finds total sales  of all products sold to people between the Ages of 12-17
query <- "SELECT '12-17' AS Age, COUNT(o.order_total) AS ItemsSold
          FROM order_tbl o, customer_tbl c
          WHERE c.age >= 12 AND c.age <= 17
            AND o.customer_id = c.customer_id;"
age12_17 <- dbGetQuery(con, query)
age12_17

#Finds total sales  of all products sold to people between the Ages of 18-24
query <- "SELECT '18-24' AS Age, COUNT(o.order_total) AS ItemsSold
          FROM order_tbl o, customer_tbl c
          WHERE c.age >= 18 AND c.age <= 24
            AND o.customer_id = c.customer_id;"
age18_24 <- dbGetQuery(con, query)
age18_24

#Finds total sales  of all products sold to people between the Ages of 25-34
query <- "SELECT '25-34' AS Age, COUNT(o.order_total) AS ItemsSold
          FROM order_tbl o, customer_tbl c
          WHERE c.age >= 25 AND c.age <= 34
            AND o.customer_id = c.customer_id;"
age25_34 <- dbGetQuery(con, query)
age25_34

#Finds total sales  of all products sold to people between the Ages of 35-44
query <- "SELECT '35-44' AS Age, COUNT(o.order_total) AS ItemsSold
          FROM order_tbl o, customer_tbl c
          WHERE c.age >= 35 AND c.age <= 44
            AND o.customer_id = c.customer_id;"
age35_44 <- dbGetQuery(con, query)
age35_44

#Finds total sales  of all products sold to people between the Ages of 45-54
query <- "SELECT '45-54' AS Age, COUNT(o.order_total) AS ItemsSold
          FROM order_tbl o, customer_tbl c
          WHERE c.age >= 45 AND c.age <= 54
            AND o.customer_id = c.customer_id;"
age45_54 <- dbGetQuery(con, query)
age45_54

#Finds total sales  of all products sold to people between the Ages of 55-64
query <- "SELECT '55-64' AS Age, COUNT(o.order_total) AS ItemsSold
          FROM order_tbl o, customer_tbl c
          WHERE c.age >= 55 AND c.age <= 64
            AND o.customer_id = c.customer_id;"
age55_64 <- dbGetQuery(con, query)
age55_64

#Finds total sales  of all products sold to people between the Ages of 65-74
query <- "SELECT '65-74' AS Age, COUNT(o.order_total) AS ItemsSold
          FROM order_tbl o, customer_tbl c
          WHERE c.age >= 65 AND c.age <= 74
            AND o.customer_id = c.customer_id;"
age65_74 <- dbGetQuery(con, query)
age65_74

#Finds total sales  of all products sold to people between the Ages of 75-84
query <- "SELECT '75-84' AS Age, COUNT(o.order_total) AS ItemsSold
          FROM order_tbl o, customer_tbl c
          WHERE c.age >= 75 AND c.age <= 84
            AND o.customer_id = c.customer_id;"
age75_84 <- dbGetQuery(con, query)
age75_84

#Combines the individual dataframes above into a single master dataframe so that the Age Bracket with the Most Sales can be Determined
ageRange <- rbind(age12_17, age18_24, age25_34, age35_44, age45_54, age55_64, age65_74, age75_84)
ageRange

#Bar Chart Generation (Orders Which of the Above Age Brackets had the Greatest Number of Sales)
ageRange %>% arrange(desc(Age)) %>%
  ggplot(aes(x=ItemsSold, y=reorder(Age, ItemsSold))) +
  geom_bar(stat = "identity", fill="blue", alpha=.6) +
  labs(x="Units Sold", y = "Age Bracket (Years)")

#------------------------------------------------------------------------
#3. Total Sales of "USB-C Charging Cable" and the State that Sells the Most of Them

#Determines how many USB-C Charging Cables have been Sold to each Individual State
query <- "SELECT c.st, COUNT(o.order_total) AS USBC_CableSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'USB-c Charging Cable'
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id
          GROUP BY c.st;"
usbc <- dbGetQuery(con, query)
usbc

#Calculates the Total Number of Units Sold Across Every State
totalSoldUnits<- sum(usbc$USBC_CableSold[1:8])
totalSoldUnits

#Bar Chart Generation (Orders Which of the Above States have had the Greatest Number of Sales)
usbc %>% arrange(desc(st)) %>%
  ggplot(aes(x=USBC_CableSold, y=reorder(st, USBC_CableSold))) +
  geom_bar(stat = "identity", fill="blue", alpha=.6) +
  labs(x="USB-C Charging Cables Sold (Units)", y = "State")

#------------------------------------------------------------------------
#4. Age Groups in California that Purchase the Most USB-C Charging Cables

#Finds total number of USB-C Charging Cables Sold in California to people between the Ages of 12-17
query <- "SELECT '12-17' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'USB-c Charging Cable'
            AND c.age >= 12 AND c.age <= 17
            AND c.st = 'CA'
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id
          GROUP BY c.st;"
usbc_age12_17 <- dbGetQuery(con, query)
usbc_age12_17

#Finds total number of USB-C Charging Cables Sold in California to people between the Ages of 18-24
query <- "SELECT '18-24' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'USB-c Charging Cable'
            AND c.age >= 18 AND c.age <= 24
            AND c.st = 'CA'
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id
          GROUP BY c.st;"
usbc_age18_24 <- dbGetQuery(con, query)
usbc_age18_24

#Finds total number of USB-C Charging Cables Sold in California to people between the Ages of 25-34
query <- "SELECT '25-34' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'USB-c Charging Cable'
            AND c.age >= 25 AND c.age <= 34
            AND c.st = 'CA'
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id
          GROUP BY c.st;"
usbc_age25_34 <- dbGetQuery(con, query)
usbc_age25_34

#Finds total number of USB-C Charging Cables Sold in California to people between the Ages of 35-44
query <- "SELECT '35-44' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'USB-c Charging Cable'
            AND c.age >= 35 AND c.age <= 44
            AND c.st = 'CA'
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id
          GROUP BY c.st;"
usbc_age35_44 <- dbGetQuery(con, query)
usbc_age35_44

#Finds total number of USB-C Charging Cables Sold in California to people between the Ages of 45-54
query <- "SELECT '45-54' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'USB-c Charging Cable'
            AND c.age >= 45 AND c.age <= 54
            AND c.st = 'CA'
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id
          GROUP BY c.st;"
usbc_age45_54 <- dbGetQuery(con, query)
usbc_age45_54

#Finds total number of USB-C Charging Cables Sold in California to people between the Ages of 55-64
query <- "SELECT '55-64' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'USB-c Charging Cable'
            AND c.age >= 55 AND c.age <= 64
            AND c.st = 'CA'
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id
          GROUP BY c.st;"
usbc_age55_64 <- dbGetQuery(con, query)
usbc_age55_64

#Finds total number of USB-C Charging Cables Sold in California to people between the Ages of 65-74
query <- "SELECT '65-74' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'USB-c Charging Cable'
            AND c.age >= 65 AND c.age <= 74
            AND c.st = 'CA'
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id
          GROUP BY c.st;"
usbc_age65_74 <- dbGetQuery(con, query)
usbc_age65_74

#Finds total number of USB-C Charging Cables Sold in California to people between the Ages of 75-84
query <- "SELECT '75-84' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'USB-c Charging Cable'
            AND c.age >= 75 AND c.age <= 84
            AND c.st = 'CA'
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id
          GROUP BY c.st;"
usbc_age75_84 <- dbGetQuery(con, query)
usbc_age75_84

#Combines the individual dataframes above into a single master dataframe so that the Age Brackets that Purchased the Greatest Number of USB-C Charging Cables can be Determined
usbc_ageRange <- rbind(usbc_age12_17, usbc_age18_24, usbc_age25_34, usbc_age35_44, usbc_age45_54, usbc_age55_64, usbc_age65_74, usbc_age75_84)
usbc_ageRange

#Bar Chart Generation (Orders Which of the Above Age Brackets Purchased the Greatest Number of USB-C Charging Cables)
usbc_ageRange %>% arrange(desc(Age)) %>%
  ggplot(aes(x=NumSold, y=reorder(Age, NumSold))) +
  geom_bar(stat = "identity", fill="blue", alpha=.6) +
  labs(x="Units Sold", y = "Age Group")

#------------------------------------------------------------------------
#5. Age Groups that Purchase the Greatest Number of Products 

#Finds total number of products purchased by people between the Ages of 12-17
query <- "SELECT '12-17' AS Age, COUNT(o.order_total) AS NumSold
          FROM order_tbl o, customer_tbl c
          WHERE c.age >= 12 AND c.age <= 17
            AND o.customer_id = c.customer_id;"
prod_age12_17 <- dbGetQuery(con, query)
prod_age12_17


#Finds total number of products purchased by people between the Ages of 18-24
query <- "SELECT '18-24' AS Age, COUNT(o.order_total) AS NumSold
          FROM order_tbl o, customer_tbl c
          WHERE c.age >= 18 AND c.age <= 24
            AND o.customer_id = c.customer_id;"
prod_age18_24 <- dbGetQuery(con, query)
prod_age18_24

#Finds total number of products purchased by people between the Ages of 25-34
query <- "SELECT '25-34' AS Age, COUNT(o.order_total) AS NumSold
          FROM order_tbl o, customer_tbl c
          WHERE c.age >= 25 AND c.age <= 34
            AND o.customer_id = c.customer_id;"
prod_age25_34 <- dbGetQuery(con, query)
prod_age25_34

#Finds total number of products purchased by people between the Ages of 35-44
query <- "SELECT '35-44' AS Age, COUNT(o.order_total) AS NumSold
          FROM order_tbl o, customer_tbl c
          WHERE c.age >= 35 AND c.age <= 44
            AND o.customer_id = c.customer_id;"
prod_age35_44 <- dbGetQuery(con, query)
prod_age35_44

#Finds total number of products purchased by people between the Ages of 45-54
query <- "SELECT '45-54' AS Age, COUNT(o.order_total) AS NumSold
          FROM order_tbl o, customer_tbl c
          WHERE c.age >= 45 AND c.age <= 54
            AND o.customer_id = c.customer_id;"
prod_age45_54 <- dbGetQuery(con, query)
prod_age45_54

#Finds total number of products purchased by people between the Ages of 55-64
query <- "SELECT '55-64' AS Age, COUNT(o.order_total) AS NumSold
          FROM order_tbl o, customer_tbl c
          WHERE c.age >= 55 AND c.age <= 64
            AND o.customer_id = c.customer_id;"
prod_age55_64 <- dbGetQuery(con, query)
prod_age55_64

#Finds total number of products purchased by people between the Ages of 65-74
query <- "SELECT '65-74' AS Age, COUNT(o.order_total) AS NumSold
          FROM order_tbl o, customer_tbl c
          WHERE c.age >= 65 AND c.age <= 74
            AND o.customer_id = c.customer_id;"
prod_age65_74 <- dbGetQuery(con, query)
prod_age65_74

#Finds total number of products purchased by people between the Ages of 75-84
query <- "SELECT '75-84' AS Age, COUNT(o.order_total) AS NumSold
          FROM order_tbl o, customer_tbl c
          WHERE c.age >= 75 AND c.age <= 84
            AND o.customer_id = c.customer_id;"
prod_age75_84 <- dbGetQuery(con, query)
prod_age75_84

#Combines the individual dataframes above into a single master dataframe so that the Age Brackets that Purchased the Greatest Number of Products can be Determined
prod_ageRange <- rbind(prod_age12_17, prod_age18_24, prod_age25_34, prod_age35_44, prod_age45_54, prod_age55_64, prod_age65_74, prod_age75_84)
prod_ageRange

#Bar Chart Generation (Orders Which of the Above Age Brackets Purchased the Greatest Number of Products)
prod_ageRange %>% arrange(desc(Age)) %>%
  ggplot(aes(x=NumSold, y=reorder(Age, NumSold))) +
  geom_bar(stat = "identity", fill="blue", alpha=.6) +
  labs(x="Units Sold", y = "Age Group")

#------------------------------------------------------------------------
#6. "Apple Airpods Headphones" and "Wired Headphones": Does Age or Gender play a role in whether these products are sold?

#Determines total number of "Apple Airpods Headphones" and "Wired Headphones" Sold to people between the Ages of 12-17
query <- "SELECT '12-17' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Apple Airpods Headphones'
            AND c.age >= 12 AND c.age <= 17
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id;"
headphones_age12_17 <- dbGetQuery(con, query)

query <- "SELECT '12-17' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Wired Headphones'
            AND c.age >= 12 AND c.age <= 17
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id;"
wired <- dbGetQuery(con, query)

headphones_age12_17$NumSold <- headphones_age12_17$NumSold + wired$NumSold
headphones_age12_17

#Determines total number of "Apple Airpods Headphones" and "Wired Headphones" Sold to people between the Ages of 18-24
query <- "SELECT '18-24' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Apple Airpods Headphones'
            AND c.age >= 18 AND c.age <= 24
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id;"
headphones_age18_24 <- dbGetQuery(con, query)

query <- "SELECT '18-24' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Wired Headphones'
            AND c.age >= 18 AND c.age <= 24
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id;"
wired <- dbGetQuery(con, query)

headphones_age18_24$NumSold <- headphones_age18_24$NumSold + wired$NumSold
headphones_age18_24

#Determines total number of "Apple Airpods Headphones" and "Wired Headphones" Sold to people between the Ages of 25-34
query <- "SELECT '25-34' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Apple Airpods Headphones'
            AND c.age >= 25 AND c.age <= 34
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id;"
headphones_age25_34 <- dbGetQuery(con, query)

query <- "SELECT '25-34' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Wired Headphones'
            AND c.age >= 25 AND c.age <= 34
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id;"
wired <- dbGetQuery(con, query)

headphones_age25_34$NumSold <- headphones_age25_34$NumSold + wired$NumSold
headphones_age25_34

#Determines total number of "Apple Airpods Headphones" and "Wired Headphones" Sold to people between the Ages of 35-44
query <- "SELECT '35-44' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Apple Airpods Headphones'
            AND c.age >= 35 AND c.age <= 44
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id;"
headphones_age35_44 <- dbGetQuery(con, query)

query <- "SELECT '35-44' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Wired Headphones'
            AND c.age >= 35 AND c.age <= 44
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id;"
wired <- dbGetQuery(con, query)

headphones_age35_44$NumSold <- headphones_age35_44$NumSold + wired$NumSold
headphones_age35_44

#Determines total number of "Apple Airpods Headphones" and "Wired Headphones" Sold to people between the Ages of 45-54
query <- "SELECT '45-54' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Apple Airpods Headphones'
            AND c.age >= 45 AND c.age <= 54
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id;"
headphones_age45_54 <- dbGetQuery(con, query)

query <- "SELECT '45-54' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Wired Headphones'
            AND c.age >= 45 AND c.age <= 54
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id;"
wired <- dbGetQuery(con, query)

headphones_age45_54$NumSold <- headphones_age45_54$NumSold + wired$NumSold
headphones_age45_54

#Determines total number of "Apple Airpods Headphones" and "Wired Headphones" Sold to people between the Ages of 55-64
query <- "SELECT '55-64' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Apple Airpods Headphones'
            AND c.age >= 55 AND c.age <= 64
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id;"
headphones_age55_64 <- dbGetQuery(con, query)

query <- "SELECT '55-64' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Wired Headphones'
            AND c.age >= 55 AND c.age <= 64
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id;"
wired <- dbGetQuery(con, query)

headphones_age55_64$NumSold <- headphones_age55_64$NumSold + wired$NumSold
headphones_age55_64

#Determines total number of "Apple Airpods Headphones" and "Wired Headphones" Sold to people between the Ages of 65-74
query <- "SELECT '65-74' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Apple Airpods Headphones'
            AND c.age >= 65 AND c.age <= 74
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id;"
headphones_age65_74 <- dbGetQuery(con, query)

query <- "SELECT '65-74' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Wired Headphones'
            AND c.age >= 65 AND c.age <= 74
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id;"
wired <- dbGetQuery(con, query)

headphones_age65_74$NumSold <- headphones_age65_74$NumSold + wired$NumSold
headphones_age65_74

#Determines total number of "Apple Airpods Headphones" and "Wired Headphones" Sold to people between the Ages of 75-84
query <- "SELECT '75-84' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Apple Airpods Headphones'
            AND c.age >= 75 AND c.age <= 84
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id;"
headphones_age75_84 <- dbGetQuery(con, query)

query <- "SELECT '75-84' AS Age, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Wired Headphones'
            AND c.age >= 75 AND c.age <= 84
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id;"
wired <- dbGetQuery(con, query)

headphones_age75_84$NumSold <- headphones_age75_84$NumSold + wired$NumSold
headphones_age75_84

#Combines the individual dataframes above into a single master dataframe so that the Age Brackets that Purchased the Greatest Number of USB-C Charging Cables can be Determined
headphones_ageRange <- rbind(headphones_age12_17, headphones_age18_24, headphones_age25_34, headphones_age35_44, headphones_age45_54, headphones_age55_64, headphones_age65_74, headphones_age75_84)
headphones_ageRange

#Bar Chart Generation (Orders Which of the Above Age Brackets Purchased the most "Apple Airpods Headphones" and "Wired Headphones")
headphones_ageRange %>% arrange(desc(Age)) %>%
  ggplot(aes(x=NumSold, y=reorder(Age, NumSold))) +
  geom_bar(stat = "identity", fill="blue", alpha=.6) +
  labs(x="Units Sold", y = "Age Group")

#Determines Total Number of "Apple Airpods Headphones" and "Wired Headphones" Sold to a Particular Gender
query <- "SELECT gender AS Gender, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Apple Airpods Headphones'
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id
            GROUP BY c.gender;"
headphones_gender <- dbGetQuery(con, query)

query <- "SELECT gender AS Gender, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_desc = 'Wired Headphones'
            AND p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id
            GROUP BY c.gender;"
wired <- dbGetQuery(con, query)

headphones_gender$NumSold <- headphones_gender$NumSold + wired$NumSold
headphones_gender

#Bar Chart Generation (Orders Which Gender Purchased the most "Apple Airpods Headphones" and "Wired Headphones")
headphones_gender %>% arrange(desc(Gender)) %>%
  ggplot(aes(x=NumSold, y=reorder(Gender, NumSold))) +
  geom_bar(stat = "identity", fill="blue", alpha=.6) +
  labs(x="Units Sold", y = "Gender")

#------------------------------------------------------------------------
#7. Other Interesting Trend in the Sales Data: Do Men or Women Buy More Products from Sales Group Inc.?

#Determines Total Number of Products Sold to a Particular Gender
query <- "SELECT gender AS Gender, COUNT(o.order_total) AS NumSold
          FROM product_tbl p, order_items_tbl items, order_tbl o, customer_tbl c
          WHERE p.product_id = items.product_id AND items.order_id = o.order_id AND o.customer_id = c.customer_id
            GROUP BY c.gender;"
gender <- dbGetQuery(con, query)
gender

#Bar Chart Generation (Orders Which Gender Purchased the most Products from Sales Group Inc.)
gender %>% arrange(desc(Gender)) %>%
  ggplot(aes(x=NumSold, y=reorder(Gender, NumSold))) +
  geom_bar(stat = "identity", fill="blue", alpha=.6) +
  labs(x="Units Sold", y = "Gender")

dbDisconnect(con) #Disconnects from the database
