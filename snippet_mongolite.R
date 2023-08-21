########################
#### Test mongolite ####
########################
library(tidyverse)
options(stringsAsFactors = FALSE)

## before install mongolite, requiers : 
## sudo yum install openssl-devel cyrus-sasl-devel
library(mongolite)

## some helps :
##  https://jeroen.github.io/mongolite/
##  https://jeroen.github.io/mongolite/query-data.html#query-syntax
##  https://rdrr.io/cran/mongolite/man/mongo.html

# url <- "mongodb://mongodb.egid.local"
url <- "mongodb://mongo4-calcul" ## url de mongo - dev (last version)

#### Dans la collection Info_Gene :

cars_collection <- mongo(collection = "cars_collection",
 db = "TEST",
 url = url,
 verbose = FALSE, options = ssl_options()) 
print(cars_collection) # to see some methods

## See collection (the table)
alldata <- cars_collection$find('{}') ## import all data
class(alldata) ## df
dim(alldata)
alldata

#### INSERT
what_to_insert <- mtcars %>% 
  as.data.frame() %>% 
  rownames_to_column("CarsModel") %>% 
  mutate(Brand = map_chr(.x = CarsModel, .f = function(x) unlist(strsplit(x = x, split = " "))[1]))

cars_collection$insert(what_to_insert)

## insert by query
# Query_to_insert <- paste0(
#   "{ \"Gene_Symbol\" : \"",ready_to_insert$Gene_Symbol,
#   "\", \"ENST\" : \"",ready_to_insert$ENST,
#   "\", \"RefSeq_mRNA\" : \"",ready_to_insert$RefSeq_mRNA,
#   "\", \"RefSeq_peptide\" : \"",ready_to_insert$RefSeq_peptide,
#   "\", \"Group\" : [ ",ready_to_insert$Group," ] }"
# )
# Info_Gene$insert(Query_to_insert) 
## Array type well created BUT NA fields treated as character and Still duplicates created /!\

what_to_insert_multi <- '{ "Brand" : "Nimp", "CarsModel" : "model1",  "Info" : [ "model1", "model1"] }'
cars_collection$insert(what_to_insert_multi)

## GETBACK DATA 
Merce_lines <- cars_collection$find('{"Brand" : "Merc"}')
print(Merce_lines)

## upsert

# By specifying the upsert option to true, the update operation either updates matching document(s) 
## or inserts a new document if no matching document exists.
key_to_update <- '{ \"Brand\" : \"Nimp\" , \"CarsModel\" : \"model2\"}'
data_to_insertOrUpdate <- '{\"$set\":{ \"Info\" : [ \"Info1\", \"Info2\"] }}'
cars_collection$update(query = key_to_update, update = data_to_insertOrUpdate, upsert = TRUE)

key_to_update <- '{ \"Brand\" : \"Nimp\" , \"CarsModel\" : \"model1\"}'
data_to_insertOrUpdate <- '{\"$set\":{ \"Info\" : [ \"Info1_maj\", \"Info2_maj\"] }}'
cars_collection$update(query = key_to_update, update = data_to_insertOrUpdate, upsert = TRUE)



#### REMOVE fields : update + unset 
Key_to_check <- "{ \"CarsModel\" : \"model1\" }"
Fields_to_remove <- "{ \"$unset\" : { \"Info\" : \"\" , \"Brand\" : \"\"} }"
cars_collection$update(Key_to_check,Fields_to_remove, upsert = TRUE, multiple= TRUE)


#### test2 
cars_collection2 <- mongo(collection = "cars_collection2",
 db = "TEST",
 url = url,
 verbose = FALSE, options = ssl_options()) 

xx = mtcars %>% group_by(cyl) %>% nest
xx ## abject tibble in 3D (data colmun contain a table)

cars_collection2$insert(xx)

re_get_data <- as.data.frame(
  cars_collection2$find(query = '{"cyl": 6}',
  fields = '{"data" : true, "_id":0}')$data
)

## test an image

my_img <- "myboxplot.png"
fs$upload(
  path = my_img,
  name = basename(my_img), 
  content_type = NULL, 
  metadata = NULL
) ## write my image (or any other file) into MongoDB (via GridFS)

fs$read(
  name = basename(my_img), 
  con = NULL, progress = TRUE
) ## read it, to check if it exists etc

fs$download(
  name = basename(my_img), 
  path = "/disks/PROJECT/SB_mboissel/Data/test_boxplot.png"
) ## get back to disk
