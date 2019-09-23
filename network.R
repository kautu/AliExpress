library('data.table') # data manipulation
library('tibble') # data wrangling

library(dplyr)
library(tidyr)
#library(ggplot2)
library(igraph) # Network Analysis and Visualization

feature.composition <- as.tibble(fread('feature_composition.csv'))[,-1]
test.spectral <- as.tibble(fread('test_spectral.csv'))[,-1]
test.gossypium <- subset(test.spectral, cluster6 %in% as.matrix(feature.composition[1,-3]))  


## Proximity
gossypium.edge <- as.tibble(fread('gossypium_edge.csv'))[,-1] %>% 
  mutate(from = as.character(from),
         to = as.character(to))
gossypium.network <- graph_from_edgelist(as.matrix(gossypium.edge[,-1]))

item.centrality <- as.data.frame(matrix(0, length(V(gossypium.network)), 2))
colnames(item.centrality) <- c('node', 'centr_eigen')
item.centrality[,1] <- V(gossypium.network)$name
#node.centrality[,2] <- centr_degree(network.multiple)$res
item.centrality[,2] <- centr_eigen(gossypium.network, directed = FALSE)$vector

item.nodes <- V(gossypium.network)$name
test.nodes <- as.character(test.gossypium$item_id)
#ranking <- match(test.nodes, item.nodes)
nodes <- intersect(test.nodes, item.nodes)
# 
adamic.adar <- similarity(gossypium.network, nodes[4], method = 'invlogweighted')
adamic.adar[order(adamic.adar, decreasing = TRUE)[1:100]]
prediction.link[4,2:31] <-as.numeric(item.nodes[order(adamic.adar, decreasing = TRUE)[1:30]])

prediction.link <- matrix(0, 52780, 31) 
prediction.link[,1] <- nodes
prediction.probability <- matrix(0, 52780, 31)
prediction.probability[,1] <- nodes
for(i in c(1:length(nodes))) {
adamic.adar <- similarity(gossypium.network, nodes[i], method = 'invlogweighted')
prediction.probability[i, 2:31] <- adamic.adar[order(adamic.adar, decreasing = TRUE)[1:30]]
prediction.link[i,2:31] <- as.numeric(item.nodes[order(adamic.adar, decreasing = TRUE)[1:30]])
}

gossypium.probability <- data.frame(prediction.probability) %>%
  mutate(X1 = as.character(X1))

gossypium.link <- data.frame(prediction.link, stringsAsFactors = FALSE) %>%
  mutate(X1 = as.character(X1),
         X2 = as.integer(X2))

gossypium.rate <- test.gossypium[,-4:-9][,-1] %>%
  mutate(item_id = as.character(item_id)) %>%
  left_join(gossypium.probability, by = c('item_id' = 'X1'))

gossypium <- test.gossypium[,-4:-9][,-1] %>%
  mutate(item_id = as.character(item_id)) %>%
  left_join(gossypium.link, by = c('item_id' = 'X1')) %>%
  left_join(item.centrality, by = c('item_id' = 'node'))

#
gossypium <- read.csv('gossypium.csv', colClasses = 'character')

gossypium.selective <- gossypium %>%
  mutate(centr_eigen = as.numeric(centr_eigen)) %>%
  group_by(buyer_admin_id) %>%
  top_n(-1, centr_eigen) %>% # -1
  distinct() 

#write.table(gossypium.selective[,-34][,-2:-3], 'prediction_selective.csv', seq = ',', row.names = FALSE, col.names = FALSE)
#gossypium.selective.centrality <- gossypium.selective[,-34][,-2:-3]

gossypium.summary <- gossypium %>%
  filter(is.na(X2) == TRUE) %>%
  group_by(buyer_admin_id) %>%
  summarise(item_count = n(),
            item_rate = ceiling(30/n()))

gossypium.likema <- as.data.frame(subset(gossypium.selective, !gossypium.selective$buyer_admin_id %in% hirsutum.selective$buyer_admin_id), stringsAsFactors = TRUE) %>%
  mutate(buyer_admin_id = as.factor(buyer_admin_id))

gossypium.hirsutum <- subset(gossypium, gossypium$buyer_admin_id %in% subset(gossypium.summary, item_count <= 2)$buyer_admin_id)
purchase.hirsutum <- subset(test.spectral, test.spectral$buyer_admin_id %in% gossypium.hirsutum$buyer_admin_id)


#write.csv(gossypium, 'gossypium.csv', row.names = FALSE)
#write.csv(gossypium.probability, 'gossypium_probability.csv', row.names = FALSE)

gossypium.na <- subset(gossypium, is.na(X2) == FALSE)
gossypium.an <- subset(gossypium, is.na(X2) == TRUE)
length(setdiff(gossypium.an$buyer_admin_id, gossypium.na$buyer_admin_id))
n_distinct(gossypium.na$buyer_admin_id)

submit.selective <- read.csv('prediction_selective.csv', header = FALSE) 
submit <- read.csv('Antai_AE_round1_submit_20190715.csv', header = FALSE) 
#uncompleted <- setdiff(submit$X152, gossypium.selective$buyer_admin_id)
uncompleted <- setdiff(submit[,1], submit.selective[,1])

length(setdiff(gossypium.selective$buyer_admin_id, test.barbadense$buyer_admin_id))
length(setdiff(uncompleted, test.hirsutum$buyer_admin_id))

#test.barbadense <- subset(test.spectral, cluster6 %in% as.matrix(feature.composition[2:3,-3]))  
#test.hirsutum <- subset(test.spectral, cluster6 %in% as.matrix(feature.composition[4,-3]))  

#write.csv(test.hirsutum, 'test_hrisutum.csv', row.names = FALSE)

##
AliExpress <- as.tibble(fread('Antai_AE_round1_train_20190626.csv'))
item.attr <- as.tibble(fread('Antai_AE_round1_item_attr_20190626.csv'))
AliExpress.item <- left_join(AliExpress, item.attr, by = 'item_id') 
purchase <- AliExpress.item %>%  
  filter(buyer_country_id == 'yy') %>%
  mutate(buyer_admin_id = as.factor(buyer_admin_id),
         item_id = as.factor(item_id),
         cate_id = as.factor(cate_id),
         store_id = as.factor(store_id))
#
test.hirsutum <- read.csv('test_hrisutum.csv')

test.uncompleted <- subset(test.spectral, buyer_admin_id %in% uncompleted) %>%
  mutate(buyer_admin_id = as.factor(buyer_admin_id),
         cate_id = as.factor(cate_id))
test.uncompleted.summary <- test.uncompleted %>%
  group_by(buyer_admin_id, cate_id) %>%
  summarise(count = n()) %>%
  top_n(1, count) 

purchase.uncompleted <- subset(purchase, purchase$cate_id %in% test.uncompleted.summary$cate_id) %>%
  group_by(cate_id, item_id) %>%
  summarise(occurence = n())

purchase.uncompleted.composition <- purchase.uncompleted %>%
  group_by(cate_id) %>%
  summarise(appearance = sum(occurence))

purchase.forecast <- left_join(purchase.uncompleted, purchase.uncompleted.composition, by = 'cate_id') %>%
  mutate(rate = occurence/appearance) %>%
  group_by(cate_id) %>%
  arrange(desc(rate)) %>%
  top_n(30, rate)

forecast.purchase <- purchase.forecast[,-3:-4]
forecast.purchase$order <- 1
forecast.unselective <- forecast.purchase %>%
  group_by(cate_id) %>%
  mutate(sequence = cumsum(order)) %>%
  filter(sequence <= 30)

unselective.forecast <- spread(forecast.unselective[,-3:-4], key = sequence, value = item_id)

submit.uncompleted <- left_join(test.uncompleted.summary, unselective.forecast, by = 'cate_id')  

submit.uncompleted.summary <- submit.uncompleted %>%
  group_by(buyer_admin_id) %>%
  summarise(count = n()) %>%
  filter(count > 1)

submit.single <- subset(submit.uncompleted, !buyer_admin_id %in% submit.uncompleted.summary$buyer_admin_id)[,-2:-3]


random.item <- as.matrix(subset(submit.uncompleted, buyer_admin_id %in% submit.uncompleted.summary$buyer_admin_id)[,-1:-3], 1)
submit.random <- data.frame(matrix(0, dim(submit.uncompleted.summary)[1], 31))
submit.random[,1] <- submit.uncompleted.summary$buyer_admin_id
submit.random[,-1] <- sample(random.item, 30 * length(submit.uncompleted.summary$buyer_admin_id), replace = TRUE)

submit.random[is.na(submit.random)] <- sample(random.item, length(submit.random[is.na(submit.random)]), replace = TRUE)
length(submit.random[is.na(submit.random)])

#submit.selective <- read.csv('prediction_selective.csv', header = FALSE, colClasses = 'character') 
submit.selective <- gossypium.selective[,-34][,-2:-3]
#submit.likema <- subset(submit.selective, 
#                        submit.selective$buyer_admin_id %in% subset(gossypium.summary, item_count > 2)$buyer_admin_id)

colnames(submit.random) <- colnames(submit.single)
colnames(submit.selective) <- colnames(submit.single)

submit.gossypium <- gossypium.likema[,-2:-3]
submit.hirsutum <- data.frame(hirsutum.selective[,-41:-42][,-3:-10]) %>%
  mutate(buyer_admin_id = as.factor(buyer_admin_id))

colnames(submit.gossypium) <- colnames(submit.single)
colnames(submit.hirsutum) <- colnames(submit.single)
submit.selective <- bind_rows(submit.gossypium, submit.hirsutum)

prediction <- bind_rows(submit.selective, bind_rows(submit.single, submit.random))

submit <- read.csv('Antai_AE_round1_submit_20190715.csv', header = FALSE, colClasses = 'character') 
colnames(submit) <- colnames(submit.single)
#length(intersect(submit[,1], prediction[,1]))
link <- left_join(submit[,1:2], prediction, by = c('buyer_admin_id'))

 sample(link[,3:7], length(link[is.na(link)]), replace = TRUE)
#length(link[is.na(link)])

write.csv(link[,-2], 'link_prediction.csv', quote = FALSE, row.names = FALSE)
