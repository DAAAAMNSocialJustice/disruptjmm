# Resources ---------------------------------------------------------------
## https://github.com/twitterdev/getting-started-with-the-twitter-api-v2-for-academic-research
## https://github.com/cjbarrie/academictwitteR

# Prepare Working Environment ---------------------------------------------
#install.packages("academictwitteR")         ## Twitter's official package to access the Twitter Academic Research Product Track V2 API Endpoint
#install.packages("devtools")                ## To use tidytags functions
#devtools::install_github("bretsw/tidytags") ## To use tidytags functions

## Load libraries
library(academictwitteR)
library(tidytags)
library(tidyverse)
library(lubridate)
library(stringr)
library(visNetwork)
library(igraph)


# Authenticate using Twitter API ------------------------------------------
## Set authorization credentials which allows the user to store their bearer token in the .Renviron file.
#set_bearer() ## Only need to run once


# Query Twitter API to obtain data ----------------------------------------
## Query API with these parameters and save as JSON files
tweets <- get_all_tweets(query = "disruptJMM",
                         start_tweets = "2006-04-01T00:00:00Z",
                         end_tweets = "2021-10-01T00:00:00Z",
                         bearer_token = get_bearer(),
                         n = Inf,
                         data_path = "data/",
                         bind_tweets = FALSE)


# Bind JSONs into data.frame; Multiple options depending on need ----------
## Tidy: "Opinionated” format the devs believe to contain all essential columns for social media research
twitterData_tidy  <- bind_tweets(data_path = "data/", user = TRUE, output_format = "tidy")

## Vanilla: Direct output from jsonlite::read_json.
## Can display columns such as text just fine. But some columns such as retweet_count are nested in list-columns.
#tweets <- bind_tweets(data_path = "data/")
#users  <- bind_tweets(data_path = "data/", user = TRUE)

## Tibble: Tibble version of above
#tweets_tibble <- bind_tweets(data_path = "data/") %>% as_tibble
#users_tibble  <- bind_tweets(data_path = "data/", user = TRUE) %>% as_tibble

## Raw: List of data frames containing all of the data extracted in the API call.
#tweets_raw <- bind_tweets(data_path = "data/", output_format = "raw")
#users_raw  <- bind_tweets(data_path = "data/", user = TRUE, output_format = "raw")


# Visualize Tweet Data Over Time ------------------------------------------
## Create new data frame from twitterData_tidy
timeSeriesMonth <- twitterData_tidy

## Convert dates in new data frame from characters to format year-month-date
timeSeriesMonth$created_at <- as.POSIXct(timeSeriesMonth$created_at) 

## Create a month variable then group by months to summarize tweet numbers
timeSeriesMonth <- timeSeriesMonth %>% 
                   select(created_at) %>%
                   mutate(month = floor_date(created_at, "month")) %>%
                   group_by(month) %>% 
                   summarize(tweets = n())

## Plot #disruptJMM Tweets Over Time (2020-2021)
plot_timeSeriesMonth <- ggplot(timeSeriesMonth, aes(x = month, y = tweets)) + 
                        geom_point() + 
                        geom_line() +
                        theme_minimal() +
                        scale_x_datetime(date_breaks = "2 months",
                                         date_labels = "%b, \n%Y") +
                        theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
                              plot.title = element_text(face = "bold")) +
                        labs (x = "Date", y = "Tweet Count",
                              title = "#disruptJMM Tweets Over Time (2020-2021)",
                              subtitle = "JMM 2020: Jan. 15th - 18th \nJMM 2021: Jan. 06th - 09th",
                              caption = "\nSource: Twitter's API via academictwitteR")

## View plot
plot_timeSeriesMonth


## Create new data frame from twitterData_tidy
timeSeriesDay2020 <- twitterData_tidy %>% 
                     filter(str_detect(created_at, "2020-01"))

## Convert dates in new data frame from characters to format year-month-date
timeSeriesDay2020$created_at <- as.POSIXct(timeSeriesDay2020$created_at)

## Create a month variable then group by days to summarize tweet numbers
timeSeriesDay2020 <- timeSeriesDay2020 %>% 
                     select(created_at) %>%
                     mutate(day = floor_date(created_at, "day")) %>%
                     group_by(day) %>% 
                     summarize(tweets = n())

## Plot #disruptJMM Tweets Over Time During JMM Conference (2020)
plot_timeSeriesDay2020 <- ggplot(timeSeriesDay2020, aes(x = day, y = tweets)) + 
                          geom_point() + 
                          geom_line() +
                          theme_minimal() +
                          scale_x_datetime(date_breaks = "1 day",
                                           date_labels = "%a, \n%d") +
                          theme(axis.text.x = element_text(angle = 0,
                                                           vjust = 1,
                                                           hjust = 0.5),
                                                           plot.title = element_text(face = "bold")) +
                          labs (x = "Date", y = "Tweet Count",
                                title = "#disruptJMM Tweets During JMM (2020)",
                                subtitle = "JMM 2020: Jan. 15th - 18th",
                                caption = "\nSource: Twitter's API via academictwitteR")

## View plot
plot_timeSeriesDay2020


## Create new data frame from twitterData_tidy
timeSeriesDay2021 <- twitterData_tidy %>% 
                     filter(str_detect(created_at, "2021-01"))

## Convert dates in new data frame from characters to format year-month-date
timeSeriesDay2021$created_at <- as.POSIXct(timeSeriesDay2021$created_at)

## Create a month variable then group by days to summarize tweet numbers
timeSeriesDay2021 <- timeSeriesDay2021 %>% 
                     select(created_at) %>%
                     mutate(day = floor_date(created_at, "day")) %>%
                     group_by(day) %>% 
                     summarize(tweets = n())

## Plot #disruptJMM Tweets Over Time During JMM Conference (2021)
plot_timeSeriesDay2021 <- ggplot(timeSeriesDay2021, aes(x = day, y = tweets)) + 
                          theme_minimal() +
                          geom_point() + 
                          geom_line() +
                          scale_x_datetime(date_breaks = "1 day",
                                           date_labels = "%a, \n%d") +
                          theme(axis.text.x = element_text(angle = 0,
                                                           vjust = 1,
                                                           hjust = 0.5),
                                                           plot.title = element_text(face = "bold")) +
                          labs (x = "Date", y = "Tweet Count",
                                title = "#disruptJMM Tweets During JMM (2021)",
                                subtitle = "JMM 2021: Jan. 6th - 9th",
                                caption = "\nSource: Twitter's API via academictwitteR")

## View plot
plot_timeSeriesDay2021


## Plot the top #disruptJMM Tweeters in the dataset
plot_topTweeters <- twitterData_tidy %>% 
                    select(user_username) %>% 
                    group_by(user_username) %>% 
                    summarize(n=n()) %>%
                    arrange(desc(n)) %>% 
                    top_n(8, n) %>% 
                    ggplot(aes(x = reorder(user_username, - n), y = n)) +
                    theme_classic() +
                    scale_fill_grey(start = 0.25, end = 2.75) +
                    geom_bar(stat = "identity") +
                    theme(axis.text.x = element_text(angle = 335,
                                                     vjust = 0.5,
                                                     hjust = 0.5),
                          plot.title = element_text(face = "bold")) +
                    labs (x = "Username", y = "Tweet Count",
                          title = "Top #disruptJMM Tweeters",
                          subtitle = "2020 - 2021",
                          caption = "\nSource: Twitter's API via academictwitteR")

## View plot
plot_topTweeters


# Save Dave Visualizations ------------------------------------------------
## Create a folder for data visualizations
dir.create("visualizations")

## Save plot as a png file
ggsave(filename = "plot_timeSeriesMonth.png",
       plot = plot_timeSeriesMonth,
       path = "visualizations/")

## Save plot as a png file
ggsave(filename = "plot_timeSeriesDay2020.png",
       plot = plot_timeSeriesDay2020,
       path = "visualizations/")

## Save plot as a png file
ggsave(filename = "plot_timeSeriesDay2021.png",
       plot = plot_timeSeriesDay2021,
       path = "visualizations/")

## Save plot as a png file
ggsave(filename = "plot_topTweeters.png",
       plot = plot_topTweeters,
       path = "visualizations/")



# Prepare Data for Social Network Analysis (SNA) --------------------------
## Use tidytags package for its SNA functions
## Provides additional variables to the academictwitteR package; also some differently named variables

# Provides meta data using tweet_id
twitterData_SNA           <- tidytags::pull_tweet_data(id_vector = twitterData_tidy$tweet_id)
# Calculate 10 additional attributes and add these to the data frame as new columns
twitterData_SNA_processed <- tidytags::process_tweets(twitterData_SNA)
# Iteratively reconstruct reply threads in an upstream direction, that is, retrieving tweets composed earlier than replies in the data set
twitterData_SNA_upstream  <- get_upstream_replies(twitterData_SNA_processed)


# Create an Edgelist ------------------------------------------------------
## Create initial edgelist using most complete data set (twitterData_SNA_upstream)
edgelist <- create_edgelist(twitterData_SNA_upstream)

## Order edgelist alphabetically
edgelist <- edgelist[order(edgelist$sender),]

## Check out the initial edgelist
head(edgelist, 20)
dplyr::count(edgelist, edge_type, sort = TRUE)

## Add user level data to edgelist to create most complete edgelist
edgelist_complete <- add_users_data(edgelist) ## Useful for when needing lots of data accessible for other tasks
dplyr::glimpse(edgelist_complete)


# Create a Nodes List -----------------------------------------------------
## Create a breakout data frame to temporarily store sender nodes
senders <- edgelist %>% 
  select(sender) %>% 
  distinct() %>% 
  mutate(label = sender)

## Create a breakout data frame to temporarily store receiver nodes
receivers <- edgelist %>% 
  select(receiver) %>% 
  distinct() %>% 
  mutate(label = receiver)

## Join sender and receiver node lists into one data frame
nodes <- full_join(senders, receivers, by = "label")

## Order node list alphabetically
nodes <- nodes[order(nodes$label),]

## Add a ID column to node list
nodes <- nodes %>% 
         rowid_to_column("id") %>%
         rename(node = label)

# Add user data to node list to create most complete nodes list
nodes_complete <- add_users_data(nodes) ## Useful for when needing lots of data accessible for other tasks

# Create node-only node list
nodes <- nodes %>% 
         select(id, node)


# Create Data Frame for In & Out Degrees ----------------------------------
## Create a breakout data frame to hold the total out-degree values for each node
outDegrees <- edgelist %>% 
              group_by(sender) %>%
              summarise(outdegree = n()) %>% 
              ungroup()
outDegrees

## Create a breakout data frame to hold the total in-degree values for each node
inDegrees <- edgelist %>% 
             group_by(receiver) %>%
             summarise(indegree = n()) %>% 
             ungroup()
inDegrees

## Join the out-degree and in-degree data frame to existing nodes_complete data frame
nodes_complete <- left_join(x = nodes_complete, y = outDegrees, by = c("node" = "sender"))
nodes_complete <- left_join(x = nodes_complete, y = inDegrees, by = c("node" = "receiver"))

## Recast out-degree and in-degree NA values into 0 values
nodes_complete <- nodes_complete %>% 
                  mutate(outdegree = ifelse(is.na(outdegree), 0, outdegree)) %>% 
                  mutate(indegree = ifelse(is.na(indegree), 0, indegree))


# Create Break-Out Edgelists -------------------------------------------------------
## Find number of connections (ties) between each node (edge)
ties_per_node <- edgelist %>% 
                 group_by(sender, receiver) %>%
                 summarise(ties = n()) %>% 
                 ungroup()
ties_per_node

## Find number of connections (ties) between each node (edge) by "edge_type"
ties_per_edge_type <- edgelist %>% 
                      group_by(sender, receiver, edge_type) %>%
                      summarise(ties = n()) %>% 
                      ungroup()
ties_per_edge_type

## Join nodes to ties_per_node list to add the ties between unique node
edges <- ties_per_node %>% 
         left_join(nodes, by = c("sender" = "node")) %>% 
         rename(from = id) %>% 
         left_join(nodes, by = c("receiver" = "node")) %>%
         rename(to = id)

## Select the order for columns. Must have from, to, and ties as first 3 for visNetwork function to run
edges <- edges %>% 
         select(from, to, ties, sender, receiver)


# Save the Above Created Data Frames as CSV Files -------------------------
## Create a folder for csv files used for SNA
dir.create("networkData")

## Save files as csv files
## NOTICE: SOME COLUMNS, LIKE tweet_id, ARE SAVING IN SCIENTIFIC NOTATION. NEED TO FIX.
## FOR NOW, JUST WORK FROM DATA SETS BUILD IN SCRIPT INSTEAD OF SAVED FILES
write_csv(nodes,              file = "networkData/nodes.csv") ## Used for visNetwork
write_csv(edges,              file = "networkData/edges.csv") ## Used for visNetwork
write_csv(edgelist,           file = "networkData/edgelist.csv") ## Starting point for the rest of these
write_csv(nodes_complete,     file = "networkData/nodes_complete.csv") ## Useful for when needing lots of data accessible for other tasks
write_csv(edgelist_complete,  file = "networkData/edgelist_complete.csv") ## Useful for when needing lots of data accessible for other tasks
write_csv(twitterData_tidy,   file = "networkData/twitterData_tidy.csv") ## Binded JSONs used to build plots and edgelist
write_csv(ties_per_node,      file = "networkData/ties_per_node.csv") ## Holds edge data
write_csv(ties_per_edge_type, file = "networkData/ties_per_edge_type.csv") ## Holds edge data

# Load Saved Data Sets from Storage ---------------------------------------
## Uncomment and load as needed depending on what datasets will be used
#nodes              <- as_tibble(read_csv(file = "networkData/nodes.csv"))
#edges              <- as_tibble(read_csv(file = "networkData/edges.csv"))
#edgeslist          <- as_tibble(read_csv(file = "networkData/edgelist.csv"))
#nodes_complete     <- as_tibble(read_csv(file = "networkData/nodes_complete.csv"))
#edgelist_complete  <- as_tibble(read_csv(file = "networkData/edgelist_complete.csv"))
#twitterData_tidy   <- as_tibble(read_csv(file = "networkData/twitterData_tidy.csv"))
#ties_per_node      <- as_tibble(read_csv(file = "networkData/ties_per_node.csv"))
#ties_per_edge_node <- as_tibble(read_csv(file = "networkData/ties_per_edge_type.csv"))


# Prepare Network Data for Network Graphs ---------------------------------
## Add create a "value" variable that the visNetwork graph can use for scaling node size
nodes <- nodes_complete %>% 
         group_by(node) %>%
         mutate(value = outdegree + indegree) %>%
         select(id, node, value)

## Change "node" variable to "label" so visNetwork can use "label" column as user ID selector
nodes <- nodes %>%
         mutate(label = node)

## Create a "width" variable that the visNetwork graph can use to scale connection size
edges <- edges %>% 
         mutate(width = ties / 10)


# Create Network Graphs ---------------------------------------------------
## Create a network graph using the Fruchterman-Reingold layout algorithm
networkGraph1 <- visNetwork(nodes, edges, main = "Network Graph for a Twitter Hashtag") %>%
                 visIgraphLayout(layout = "layout_with_fr") %>%
                 visNodes(color = list(background = "lightblue", border = "black", highlight = "orange")) %>% 
                 visEdges(arrows = "to", color = list(highlight = "orange")) %>%
                 visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), 
                            nodesIdSelection = list(enabled = TRUE, useLabels = TRUE, selected = "223")) %>%
                 visInteraction(multiselect = TRUE) %>% 
                 visLayout(randomSeed = 123)
networkGraph1


## Create a network graph using a simple layout algorithm
networkGraph2 <- visNetwork(nodes, edges, main = "Network Graph for a Twitter Hashtag") %>%
                 visIgraphLayout(layout = "layout_nicely") %>%
                 visNodes(color = list(background = "lightblue", border = "black", highlight = "orange")) %>% 
                 visEdges(arrows = "to", color = list(highlight = "orange")) %>%
                 visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), 
                           nodesIdSelection = list(enabled = TRUE, useLabels = TRUE, selected = "223")) %>%
                 visInteraction(multiselect = TRUE) %>% 
                 visLayout(randomSeed = 123)
networkGraph2


## Create a network graph using a rectangular grid layout algorithm
networkGraph3 <- visNetwork(nodes, edges, main = "Network Graph for a Twitter Hashtag") %>%
                 visIgraphLayout(layout = "layout_on_grid") %>%
                 visNodes(color = list(background = "lightblue", border = "black", highlight = "orange")) %>% 
                 visEdges(arrows = "to", color = list(highlight = "orange")) %>%
                 visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), 
                             nodesIdSelection = list(enabled = TRUE, useLabels = TRUE, selected = "223")) %>%
                 visInteraction(multiselect = TRUE) %>% 
                 visLayout(randomSeed = 123)
networkGraph3


## Create a network graph using the Davidson-Harel layout algorithm
networkGraph4 <- visNetwork(nodes, edges, main = "Network Graph for a Twitter Hashtag") %>%
                 visIgraphLayout(layout = "layout_with_dh") %>%
                 visNodes(color = list(background = "lightblue", border = "black", highlight = "orange")) %>% 
                 visEdges(arrows = "to", color = list(highlight = "orange")) %>%
                 visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), 
                             nodesIdSelection = list(enabled = TRUE, useLabels = TRUE, selected = "223")) %>%
                 visInteraction(multiselect = TRUE) %>% 
                 visLayout(randomSeed = 123)
networkGraph4


# Save visNetwork Plots as HTML pages -------------------------------------
visSave(graph = networkGraph1, file = "visualizations/networkGraph1.html")
visSave(graph = networkGraph2, file = "visualizations/networkGraph2.html")
visSave(graph = networkGraph3, file = "visualizations/networkGraph3.html")
visSave(graph = networkGraph4, file = "visualizations/networkGraph4.html")

