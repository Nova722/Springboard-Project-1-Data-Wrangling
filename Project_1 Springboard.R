#0 Uploaded data 
refine_original = read.csv("C:/Users/chris/Desktop/data_wrangling/data_wrangling_test/refine_original.csv", header = TRUE, sep = ",")
View(refine_original)

#1 clean up brand names
refine_original$company <- sub(pattern = ".*s.*", replacement = "philips", x = refine_original$company, ignore.case = TRUE)
refine_original$company <- sub(pattern = ".*k.*", replacement = "akzo", x = refine_original$company, ignore.case = TRUE)
refine_original$company <- sub(pattern = ".*ou.*", replacement = "van\\ houten", x = refine_original$company, ignore.case = TRUE)
refine_original$company <- sub(pattern = "^uni.*", replacement = "unilever", x = refine_original$company, ignore.case = TRUE)

#2 separate product code and number
library(tidyr)
refine_original <- separate(refine_original, Product.code...number, c("product.code", "product.no"), sep = "-")

#3 add product categories
refine_original <- mutate(refine_original, product.category)

for (i in 1:nrow(refine_original)) {
  if (refine_original$product.code[i] == "p") {refine_original$product.category[i] = "Smartphone"} 
  else if(refine_original$product.code[i] == "v") {refine_original$product.category[i] = "TV"} 
  else if (refine_original$product.code[i] == "x") {refine_original$product.category[i] = "Laptop"} 
  else if (refine_original$product.code[i] == "q") {refine_original$product.category[i] = "Tablet"}
}

#4 add full address for geocoding
library(tidyr)
refine_original <- unite(refine_original, "full_address", address, city, country, sep = "," )

#5 create binary variables for company and product
refine_original <- mutate(refine_original, "company_philips" = 0, "company_akzo" = 0, "company_van_houten" = 0, "company_unilever" = 0)

for (i in 1:nrow(refine_original)) {
if (refine_original$company[i] == "philips") {refine_original$company_philips[i] = 1} 
else if(refine_original$company[i] == "akzo") {refine_original$company_akzo[i] = 1} 
else if (refine_original$company[i] == "van houten") {refine_original$company_van_houten[i] = 1} 
else if (refine_original$company[i] == "unilever") {refine_original$company_unilever[i] = 1}
}

refine_original <- mutate(refine_original, product_smartphone = 0, product_tv = 0, product_laptop = 0, product_tablet = 0)
for (i in 1:nrow(refine_original)) {
  if (refine_original$product.category[i] == "Smartphone") {refine_original$product_smartphone[i] = 1} 
  else if(refine_original$product.category[i] == "TV") {refine_original$product_tv[i] = 1} 
  else if (refine_original$product.category[i] == "Laptop") {refine_original$product_laptop[i] = 1} 
  else if (refine_original$product.category[i] == "Tablet") {refine_original$product_tablet[i] = 1}
}

#export file to .csv
write.csv(refine_original, "refine_clean.csv")





