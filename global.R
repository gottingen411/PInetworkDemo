my_author_names = c(
  'Graham',
  'McKinley',
  'Matar',
  'Kalliadasis',
  'Karpitschka',
  'Lauga',
  'Cates',
  'Stone',
  'Arnold',
  'Morris'
)
my_IDs = c(
  'BK1w9CMAAAAJ',
  'uLxFfGkAAAAJ',
  'mLXz_NAAAAAJ',
  '71Vf8GEAAAAJ',
  'nKvTMosAAAAJ',
  'pCZrETgAAAAJ',
  'dxG_sPMAAAAJ',
  'GfNjESUAAAAJ',
  'wil5NhcAAAAJ',
  'whV8vY0AAAAJ'
)

library(dplyr)
library(igraph)
library(visNetwork)
my_df = data.frame() #master data frame containing all network info

#read data from csv files into my_df
for (i in c(1:10)) {
  pathtodata = './data/'
  pathtodata = paste0(pathtodata, my_IDs[i])
  df_to_add = read.csv(pathtodata, stringsAsFactors = FALSE) %>% select(., -1) %>% mutate(., networkID =
                                                                                            my_IDs[i])
  my_df = rbind(my_df, df_to_add)
}

# function taking in a URL string to output an ID
fmt_url = function(str) {
  if (str == '') {
    return("B7vSqZsAAAAJ")
  } else{
    len_url = nchar(str) # end of url string
    usr_i = len_url - 17 # initial position of user id in url
    ID = substr(str, start = usr_i, stop = usr_i + 11)
    return(ID)
  }
}
# function to get data frame for network nodes. The df is formatted such that it can be used by visNetwork
get_vis_nodes = function(df) {
  df1 = filter(df, !(df[, 2] %in% c(
    "Sort By Title", "Sort By Year", "Sort By Citations"
  )))
  nodesVis = data.frame(id = unique(c(unique(df1[, 1]), unique(df1[, 2]))), label =
                          unique(c(unique(df1[, 1]), unique(df1[, 2]))))
  return(nodesVis)
  
}

# function to get data frame for network edges. The df is formatted such that it can be used by visNetwork

get_vis_edges = function(df) {
  df1 = filter(df, !(df[, 2] %in% c(
    "Sort By Title", "Sort By Year", "Sort By Citations"
  )))
  edgeVis <- df1 %>% select(., from = 1, to = 2) %>% mutate(., weight =
                                                              1)
  return(edgeVis)
  
}
# function to get data frame for network nodes. The df is formatted such that it can be used by igraph

get_igraph_edges = function(df) {
  df1 = filter(df, !(df[, 2] %in% c(
    "Sort By Title", "Sort By Year", "Sort By Citations"
  )))
  edgeIG <- df1 %>% select(., from = 1, to = 2)
  return(edgeIG)
  
}

#function for the demo: take in the author ID to extract the part of the master df belonging to that author
network_extract = function(ID) {
  df1 = filter(my_df, !(
    my_df[, 2] %in% c("Sort By Title", "Sort By Year", "Sort By Citations")
  ) & my_df[, 3] == ID)
  
  return(df1)
  
}

#function to get the list of coauthors of a specific author, given the author's df for edges and the desirable number of coauthors
coauthor_slice = function(df, n_coauthors) {
  if (n_coauthors > 50) {
    return('Error: too many coauthors! (n_coauthors>50)')
  }
  raw_slice = df[1:n_coauthors, 2]
  slice = raw_slice[!is.na(raw_slice)]
  slice = append(slice, df[1, 1], after = 0)
  return(slice)
}
#function to get a df such that each author has n_coauthors
coauthor_process = function(df, n_coauthors, caslice) {
  if (n_coauthors > 50) {
    return('Error: too many coauthors! (n_coauthors>50)')
  }
  processed_df = data.frame()
  dummy_df = data.frame()
  for (x in caslice) {
    dummy_df = filter(df, df[, 1] == x)
    processed_df = rbind(processed_df, dummy_df[1:n_coauthors, ])
  }
  return(processed_df[!is.na(processed_df[, 1]), ])
}