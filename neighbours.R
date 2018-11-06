scriptPath <- function() {getSrcDirectory(scriptPath);}
setwd(scriptPath())
rm(list=ls())



library(data.table)
library(openxlsx)
library(rvest)



#fetch and parse country neighbour tables (5.11.2018)
x = read_html("https://en.wikipedia.org/wiki/List_of_countries_and_territories_by_land_and_maritime_borders")


# hack to keep neighbours separated by | in cells (https://stackoverflow.com/questions/30921626/can-rvest-keep-inline-html-tags-such-as-br-using-html-table)
xml_find_all(x, ".//br") %>% xml_add_sibling("p", "|")
xml_find_all(x, ".//br") %>% xml_remove()

x = x %>%
  html_node("table.wikitable") %>%
  html_table()

# needs a lot of fixing:
colnames(x) = c("country", "nland", "nmar", "ntot","neighbours")

neighbours_orig = x

x$country   = gsub("\\([^)]*\\)", "", x$country)
x$neighbours = gsub("\\([^)]*\\)", "", x$neighbours)
x$country   = gsub("\\[[^)]*\\]", "", x$country)
x$neighbours = gsub("\\[[^)]*\\]", "", x$neighbours)

# fix entries with additional text
x[93,]$country = "French Southern and Antarctic Lands"
x[75,]$neighbours = "Eritrea | Ethiopia | Somalia | Yemen"
x[158,]$neighbours = "Algeria | Cape Verde | Mali | Senegal"
x[168,]$neighbours = "Algeria | Portugal |  Spain"
x[221,]$neighbours = "Bosnia and Herzegovina |  Bulgaria |  Croatia |  Hungary |  Republic of Macedonia | Montenegro | Romania | Kosovo"
x[229,]$neighbours = "Djibouti | Ethiopia | Kenya | Yemen"
x[254,]$neighbours = "Armenia | Azerbaijan | Bulgaria | Cyprus | Iran | Iraq | Russia | Syria | Ukraine"

# remove territories that include islands
x = x[-c(18,74,91,176,179,187, 262,nwrite_rows = 40),]

x$country = trimws(x$country)

#construct matrix that indicades which country neighbours which
country_adj = matrix(nrow = NROW(x), ncol = NROW(x), data = F)
rownames(country_adj) = x$country
colnames(country_adj) = x$country

adj_names = x$country
for(i  in 1:NROW(x)){
  
  cr = x[i,]
  
  # parse all neighbours of current country to remove leading and trailing whitespaces
  neighs = gsub("(^\\s+)|(\\s+$)", "", unlist(strsplit(cr$neighbours, "|", fixed = T), use.names = F))
  
  # patch them back together to form the regex
  neighs = paste0("^", neighs, "$" , collapse = "|")  
  
  # match them with the first column 
  matches = grep(neighs, x$country)
  
  # store result in matrix
  country_adj[i,matches] = T
}


# # Manually change some country names to harmonize them with the names used by the GBD
# adj_names[grepl("^São Tomé and Príncipe$", adj_names)] = "Sao Tome and Principe"
# adj_names[grepl("^Russia$",adj_names)] = "Russian Federation"
# adj_names[grepl("^East Timor$", adj_names)] = "Timor-Leste"
# adj_names[grepl("^Republic of Macedonia$", adj_names)] = "Macedonia"
# adj_names[grepl("^United States Virgin Islands$", adj_names)] = "Virgin Islands, U.S."
# adj_names[grepl("^Côte d'Ivoire$", adj_names)] = "Cote d'Ivoire"
# adj_names[grepl("^Bahamas$", adj_names)] = "The Bahamas"
# adj_names[grepl("^Republic of the Congo$", adj_names)] = "Congo"

rownames(country_adj) = adj_names
colnames(country_adj) = adj_names


# generate JSON file
t = lapply(rownames(country_adj), function (x) { colnames(country_adj)[country_adj[rownames(country_adj) == x, ]] })
names(t) = rownames(country_adj)
json = toJSON(t, pretty = T)


