#install.packages("RMariaDB")
#install.packages("tidyverse")
library(tidyverse)
library(RMariaDB)

#Connection Information to Access the Database
getConn <- function(){
  con <- dbConnect(
    drv = RMariaDB::MariaDB(),
    host = 'Apollo',
    user = 'agrieco896',
    password = 'agrieco896@123', #OR .rs.askForPassword("Database Password:"),
    port = 3306,
    dbname = 'CS345')
  cat(paste("\tHost: ", slot(con,'host'), "\n\tDatabase: ", slot(con,'db')))
  return(con)
}

con <- getConn() #Connects to the database

query <- "show databases;" #SQL Query to be sent to the database
df <- dbGetQuery(con, query) #Results taken from database and saved into the "df" dataframe
df

query <- "use CS345;"
dbExecute(con, query) #Executes command to be run on the database (doesn't return anything)

query <- "describe customer_tbl;" #Returns what fields and what datatypes are being used in a given table
df <- dbGetQuery(con, query)
df

query <- "SELECT customer_id, gender, age, st 
          FROM customer_tbl 
          LIMIT 20;" #Limits the number of rows (first 20) to be returned
df <- dbGetQuery(con, query)
df

query <- "SELECT customer_id, gender, age, st 
          FROM customer_tbl 
          WHERE st='NY';" #Only records where state is equal to New York will be shown
df <- dbGetQuery(con, query)
df

query <- "SELECT customer_id, gender, age, st 
          FROM customer_tbl 
          WHERE st='NY' AND gender = 'M';" #Only records where state is equal to New York and gender = Male will be shown
df <- dbGetQuery(con, query)
df

query <- "describe order_items_tbl"
df <- dbGetQuery(con, query)

#oo is an Alias for a particular table
query <- "SELECT oo.order_id, oo.product_id, oo.quantity, oo.price_ea
          FROM order_items_tbl oo;"
df <- dbGetQuery(con, query)

query <- "SELECT *
          FROM product_tbl
          WHERE product_id = 100;"
df <- dbGetQuery(con, query)

#Joins
query <- "SELECT oo.order_id, oo.product_id, p.product_desc
          FROM order_items_tbl oo, product_tbl p
          WHERE oo.product_id = p.product_id;" #Join the two tables on where the have the same matching product id between both tables
df <- dbGetQuery(con, query)

query <- "SELECT customer_id
          FROM order_tbl
          WHERE order_total > 300;"
df <- dbGetQuery(con, query)

query <- "SELECT o.*, c.age
          FROM order_tbl o, customer_tbl c
          WHERE o.customer_id = 110732 AND o.customer_id = c.customer_id;"
df <- dbGetQuery(con, query)

query <- "SELECT c.city, c.st, o.order_id, o.customer_id, o.order_total
          FROM customer_tbl c, order_tbl o
          WHERE c.city = 'Los Angeles' AND c.st = 'CA' AND c.customer_id = o.customer_id;"
df <- dbGetQuery(con, query)

query <- "SELECT items.order_id, c.city, c.st, o.order_id, o.customer_id, COUNT(o.order_id) AS cnt, o.order_date
          FROM customer_tbl c, order_tbl o, order_items_tbl items
          WHERE c.city = 'Los Angeles' AND c.st = 'CA' 
                AND c.customer_id = o.customer_id AND items.order_id = o.order_id
          GROUP BY o.order_date
          ORDER BY cnt DESC;"
df <- dbGetQuery(con, query)

query <- "SELECT items.product_id AS id, p.product_desc AS dsc, MONTH(o.order_date) AS month
          FROM order_items_tbl items, product_tbl p, order_tbl o, customer_tbl c
          WHERE c.st = 'CA'
            AND items.product_id = p.product_id AND c.customer_id = o.customer_id AND o.order_id = items.order_id;"
df <- dbGetQuery(con, query)

df %>% group_by(month) %>%
  summarize(n1=n()) %>%
  arrange(desc(n1)) %>%
  ggplot(aes(x=n1, y=reorder(month, n1))) +
  geom_bar(stat = "identity", fill="blue", alpha=.6) +
  labs(x="Total Products Sold in CA", y = "Month of Sale")

dbDisconnect(con) #Disconnects from the database
