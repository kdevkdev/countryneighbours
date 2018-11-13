# license: GPLv3


scriptPath <- function() {getSrcDirectory(scriptPath);}
setwd(scriptPath())
rm(list=ls())


library(data.table)
library(openxlsx)
library(rvest)
library(jsonlite)



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

# fix entries with additional text
x[93,]$country = "French Southern and Antarctic Lands" # French southern and Antartics Lands Excluding Adelie Land

x[75,]$neighbours = "Eritrea | Ethiopia (L) | Somalia | Yemen (M)" # Djibouti
x[158,]$neighbours = "Algeria (L) | Cape Verde (M) | Mali (L) | Senegal | Western Sahara" # Mauritania
x[168,]$neighbours = "Algeria | Portugal (M) | Spain | Western Sahara" # Morocco
# Serbia
x[221,]$neighbours = "Bosnia and Herzegovina  (L)|  Bulgaria  (L)| Croatia  (L)| Hungary  (L)|  Republic of Macedonia  (L)| Montenegro  (L)| Romania  (L)| Kosovo (L)"
x[229,]$neighbours = "Djibouti | Ethiopia  (L)| Kenya | Yemen (M)" # Somalia
# Turkey
x[254,]$neighbours = "Armenia (L)| Azerbaijan (L)| Bulgaria | Cyprus (M)| Egypt (M)| Georgia | Greece | Iran (L)| Iraq (L)| Russia (M)| Syria | Ukraine (M)"

# remove territories that include islands
# 18: Australia including islands
# 74: Denmark including islands 
# 91: France including collectives and territories
# 176: Netherlands including islands
# 179: New Zealand including islands
# 187: Norway including territories
# 262: Uk including overseas territories and dependencies
# 264: United stas including insular areas
x = x[-c(18,74,91,176,179,187, 262, 264),]


#construct matrix that indicades which country neighbours which
country_adj = matrix(nrow = NROW(x), ncol = NROW(x), data = F)
rownames(country_adj) = x$country
colnames(country_adj) = x$country

# remove brackets ()[] and stuff inside from country
x$country   = gsub("\\([^)]*\\)", "", x$country)
x$country   = gsub("\\[[^]]*\\]", "", x$country)


# cut possible spaces from country column
#x$country = gsub("(^\\s+)|(\\s+$)", "", x$country)
x$country = trimws(x$country)

# replace United Nations Buffer Zone in Cyprus with nothing as it is not listed as a country in the first column
x$neighbours = gsub("United Nations Buffer Zone in Cyprus \\(L\\)\\[8\\]", "", x$neighbours)
x$neighbours = gsub("United Nations Buffer Zone in Cyprus", "", x$neighbours)

adj_names = x$country
for(i  in 1:NROW(x)){
  
  cr = x[i,]
  
  if(cr$neighbours != ""){
  
    # parse all neighbours of current country 
    rawneighs = unlist(strsplit(cr$neighbours, "|", fixed = T), use.names = F)
    
    # remove empty strings from the vector
    rawneighs = rawneighs[rawneighs != ""]
    
    # remove brackets and contents
    neighs = gsub("\\[[^]]*\\]", "", rawneighs)
    neighs = gsub("\\([^)]*\\)", "", neighs)
    
    # remove leading and trailing whitespaces (by matching whitespaces and replacing them with "")
    neighs = gsub("(^\\s+)|(\\s+$)", "", neighs)
    
    # try to parse the kind of border according to bracket suffic after country name: Land (L), maritime (M), or both (blank, according to wiki)
    # 1 is land, 2 is maritime, 3 is both
    neightypes = ifelse(grepl("\\(L\\)", rawneighs), 1, ifelse(grepl("\\(M\\)", rawneighs),2,3))
    
    # go through each neighbour and match it with the first column
    matches = sapply(neighs, function(n, countries){ which(countries %in% n)[1] }, x$country)
    
    # store result in matrix
    country_adj[i,matches] = neightypes
  }
}


# Manually change some country names to harmonize them with the names used by the GBD
adj_names[grepl("^São Tomé and Príncipe$", adj_names)] = "Sao Tome and Principe"
adj_names[grepl("^Russia$",adj_names)] = "Russian Federation"
adj_names[grepl("^East Timor$", adj_names)] = "Timor-Leste"
adj_names[grepl("^Republic of Macedonia$", adj_names)] = "Macedonia"
adj_names[grepl("^United States Virgin Islands$", adj_names)] = "Virgin Islands, U.S."
adj_names[grepl("^Côte d'Ivoire$", adj_names)] = "Cote d'Ivoire"
adj_names[grepl("^Bahamas$", adj_names)] = "The Bahamas"
adj_names[grepl("^Republic of the Congo$", adj_names)] = "Congo"

rownames(country_adj) = adj_names
colnames(country_adj) = adj_names


# generate JSON file
t = lapply(rownames(country_adj), function (x) { 
          
              ns = country_adj[which(rownames(country_adj) == x),]
              ns = ns[ns>0]
          
              list(names = names(ns), type = ns)
       })
names(t) = rownames(country_adj)
json = toJSON(t, pretty = T)

write(json, "countries.json")
saveRDS(country_adj, "neighbours.rds")

