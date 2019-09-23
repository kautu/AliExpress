library('data.table') # data manipulation
library('tibble') # data wrangling

library(dplyr)
library(tidyr)
library(ggplot2)

library(igraph) # Network Analysis and Visualization

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
AliExpress.test <- as.tibble(fread('Antai_AE_round1_test_20190626.csv'))
purchase.test <- left_join(AliExpress.test, item.attr, by = 'item_id') %>%
  mutate(buyer_admin_id = as.factor(buyer_admin_id),
         item_id = as.factor(item_id),
         cate_id = as.factor(cate_id),
         store_id = as.factor(store_id))


## BUYER # repeated purchase
cate.repeated <- purchase %>%
  group_by(buyer_admin_id) %>%
  summarise(purchase_count = max(irank),
            cate_count = n_distinct(cate_id),
            store_count = n_distinct(store_id)) %>%
  mutate(purchase_cate = purchase_count/cate_count,
         purchase_store = purchase_count/store_count)

test.repeated <- purchase.test %>%
  group_by(buyer_admin_id) %>%
  summarise(purchase_count = max(irank),
            cate_count = n_distinct(cate_id),
            store_count = n_distinct(store_id)) %>%
  mutate(purchase_cate = purchase_count/cate_count,
         purchase_store = purchase_count/store_count)

library(tree)
buyer.tree <- tree(purchase_cate ~ purchase_count + cate_count + store_count, cate.repeated)

summary(buyer.tree)
plot(buyer.tree)
text(buyer.tree, pretty = 0)

buyer.cluster <- cate.repeated %>%
  mutate(buyer_tree = as.factor(buyer.tree$where))

buyer.cluster.composition <- buyer.cluster %>%
  group_by(buyer_tree) %>%
  summarize(composition = n(),
            mean_cate = round(mean(purchase_cate), 6),
            mean_store = mean(purchase_store))

ggplot(buyer.cluster, aes(log(purchase_count), log(cate_count), color = buyer_tree)) +
  geom_point(alpha = 1/10) 
          
ggplot(buyer.cluster.composition, aes(composition, mean_cate)) +
  geom_bar(stat_count()) #


test.cluster <- test.repeated %>%
  mutate(mean_cate = round(predict(buyer.tree, newdata = test.repeated), 6)) %>%
  left_join(buyer.cluster.composition, by = 'mean_cate')

test.cluster.composition <- test.cluster %>%
  group_by(buyer_tree) %>%
  summarize(composition = n(),
            mean_cate = round(mean(purchase_cate), 6),
            mean_store = mean(purchase_store))

# floating philosophy rational
singular.store.buyer <- subset(buyer.cluster, purchase_store == 1)
singular.store.test <- subset(test.cluster, purchase_store == 1)

single.cate.buyer <- subset(buyer.cluster, cate_count == 1)
single.cate.test <- subset(test.cluster, cate_count == 1)

single.store.buyer <- subset(buyer.cluster, store_count == 1)
single.store.test <- subset(test.cluster, store_count == 1)

# Exp
buyer.cluster21 <- subset(buyer.cluster, buyer_tree == 21) %>%
  filter(!buyer_admin_id %in% singular.store.buyer$buyer_admin_id) %>%
  mutate(delta = purchase_count - store_count)

## # 
cate.buyer <- purchase %>%
  group_by(cate_id, buyer_admin_id) %>%
  summarise(purchases = n()) 
cate.buyer[which(cate.buyer$purchases > 1),3] <- 0
cate.buyer.repeat <- cate.buyer %>%  
  group_by(cate_id) %>%
  summarise(buyer_repeated = n() - sum(purchases),
            repeated_purchase_rate = (n() - sum(purchases))/n())

## #
category <- purchase %>%
  group_by(cate_id) %>%
  summarise(cate_purchase = n(),
            cate_buyer = n_distinct(buyer_admin_id),
            cate_item = n_distinct(item_id),
            cate_store = n_distinct(store_id)) %>%
  mutate(purchase_buyer = cate_purchase/cate_buyer, # Average Repeated Purchases
         cate_freq = cate_buyer/138678) %>% # Buyer Appearance Rate
  left_join(cate.buyer.repeat, by = 'cate_id')
  
ggplot(category, aes(log(cate_buyer), log(cate_item))) +
  geom_point(alpha = 1/10)
# blossoms
ggplot(category, aes(log(cate_buyer), repeated_purchase_rate)) +
  geom_point(alpha = 1/10)  
  
ggplot(category, aes(log(cate_item), repeated_purchase_rate)) +
  geom_point(alpha = 1/10) +
  geom_smooth(method = 'auto') 

#  Generalized additive models with integrated smoothness estimation
ggplot(category, aes(log(cate_item), cate_freq)) +
  geom_point(alpha = 1/10) +
  geom_smooth(method = 'auto') + ## GAM
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11))

# Cumulative Loyalty or Consistent Necessity in Category with higher orders
ggplot(category, aes(log(cate_purchase), buyer_repeated)) +
  geom_point(alpha = 1/20)

##
## # Multiple Category Segmentation
buyer.cate <- purchase %>%
  group_by(buyer_admin_id, cate_id) %>%
  summarise(category = n(),
            distance = mean(irank))

buyer.separate <- buyer.cate %>%
  group_by(buyer_admin_id) %>%
  summarise(cate_count = n())

buyer.multiple <- subset(buyer.cate, buyer.cate$buyer_admin_id %in% subset(buyer.separate, buyer.separate$cate_count <=7)$buyer_admin_id)

buyer.multiple$order <- 1
buyer.multiple <- buyer.multiple %>%
  group_by(buyer_admin_id) %>%
  mutate(sequence = cumsum(order))

multiple.buyer <- spread(buyer.multiple[, c('buyer_admin_id', 'sequence', 'cate_id')], key = sequence, value = cate_id)
colnames(multiple.buyer)[2:8] <- c('Cat1', 'Cat2', 'Cat3', 'Cat4', 'Cat5', 'Cat6', 'Cat7')
##  Notre Chico
multiple.fortaleza <- multiple.buyer %>% transmute(edge = paste(Cat7, Cat6, sep = '_'))
multiple.pativilca <- multiple.buyer %>% transmute(edge = paste(Cat6, Cat5, sep = '_'))
multiple.supe      <- multiple.buyer %>% transmute(edge = paste(Cat5, Cat4, sep = '_'))
multiple.huaura    <- multiple.buyer %>% transmute(edge = paste(Cat4, Cat3, sep = '_'))
multiple.jaguay    <- multiple.buyer %>% transmute(edge = paste(Cat3, Cat2, sep = '_'))
multiple.quebrada  <- multiple.buyer %>% transmute(edge = paste(Cat2, Cat1, sep = '_'))

multiple.edge <- bind_rows(multiple.fortaleza, multiple.pativilca, multiple.supe, multiple.huaura, multiple.jaguay, multiple.quebrada) %>%
  separate(edge, c('from', 'to'), sep = '_') %>%
  filter(from != 'NA' & to != 'NA')

edge.centrality <- multiple.edge %>%
  group_by(from) %>%
  summarise(node.degree = n())

edge.degree <- multiple.edge %>%
  group_by(from, to) %>%
  summarise(edge.appearance = n(),
            edge.buyer = n_distinct(buyer_admin_id)) %>%
  mutate(diff = edge.appearance - edge.buyer) %>%
  left_join(edge.centrality, by = 'from') %>%
  mutate(probability = edge.appearance/node.degree) # apriori

## #
network.multiple <- graph_from_edgelist(as.matrix(edge.degree[,1:2]), directed = FALSE)

node.centrality <- as.data.frame(matrix(0, 3355, 3))
colnames(node.centrality) <- c('node', 'centr_degree', 'centr_eigen')
node.centrality[,1] <- V(network.multiple)$name
node.centrality[,2] <- centr_degree(network.multiple)$res
node.centrality[,3] <- centr_eigen(network.multiple, directed = FALSE)$vector

ggplot(node.centrality, aes(centr_eigen, centr_degree)) +
  geom_point(alpha = 1/10)


category.centrality <- left_join(category, node.centrality, by = c('cate_id' = 'node'))
category.centrality[is.na(category.centrality)] <- 0

#adjacency <- as_adjacency_matrix(network.multiple)
laplacian <- laplacian_matrix(network.multiple)
eigenvalues <- eigen(laplacian)$value
#diff(eigenvalues[1:20])
plot(eigenvalues[1:100])
#
#eigenvectors <- eigen(laplacian)$vectors[,1:6]
category.grouping <- as.data.frame(matrix(0, 3355, 7))
category.grouping[,1] <- V(network.multiple)$name
category.grouping[,2:7] <- eigen(laplacian)$vectors[,1:6]

#plot(category.grouping$V7)
clustering3 <- kmeans(category.grouping[,2:4], 6)
clustering6 <- kmeans(category.grouping[,2:7], 12)

category.cluster <- as.data.frame(matrix(0,3355, 3))
category.cluster[,1] <- V(network.multiple)$name
category.cluster[,2] <- clustering3$cluster
category.cluster[,3] <- clustering6$cluster
colnames(category.cluster) <- c('node', 'cluster3', 'cluster6')
#category.cluster <- category.cluster %>%
 # mutate(cluster3 = as.factor(cluster3),
 #        cluster6 = as.factor(cluster6))

category.spectral <- left_join(category.centrality, category.cluster, by = c('cate_id' = 'node'))
category.spectral[is.na(category.spectral)] <- 0
category.spectral <- category.spectral %>%
  mutate(cluster3 = as.factor(cluster3),
         cluster6 = as.factor(cluster6))

ggplot(category.spectral, aes(centr_eigen, centr_degree, color = cluster6)) +
  geom_point(alpha = 1/10)

spectral.composition <- category.spectral %>%
  group_by(cluster6) %>%
  summarise(category = n(),
            appearance = mean(cate_freq),
            repeated = mean(repeated_purchase_rate),
            centrality = mean(centr_eigen))

## 
purchase.spectral <- left_join(purchase, category.spectral[,c('cate_id', 'centr_eigen', 'cluster6')], by = 'cate_id') %>%
  group_by(buyer_admin_id, cluster6) 
 #summarise(communities = n()) %>%
 #spread(key = cluster6, value = communities)
#purchase.spectral[is.na(purchase.spectral)] <- 0

test.spectral <- left_join(purchase.test, category.spectral[,c('cate_id', 'centr_eigen', 'cluster6')], by = 'cate_id') %>%
  group_by(buyer_admin_id, cluster6) 
 #summarise(communities = n()) %>% #  spread(key = cluster6, value = communities)
 ##test.spectral[is.na(test.spectral)] <- 0

#
edge.cluster <- edge.degree %>%
  left_join(category.spectral[,c('cate_id', 'cluster6')], by = c('from' = 'cate_id')) %>%
  left_join(category.spectral[,c('cate_id', 'cluster6')], by = c('to' = 'cate_id')) %>%
  group_by(cluster6.x, cluster6.y) %>%
  summarise(connection = n()) 

edge.cluster$cluster6.x <- factor(edge.cluster$cluster6.x, levels = c('5', '10', '4', '11', '7', '1', '12', '8', '6', '2', '3', '9'))
edge.cluster$cluster6.y <- factor(edge.cluster$cluster6.y, levels = c('5', '10', '4', '11', '7', '1', '12', '8', '6', '2', '3', '9'))

cluster.edge <- spread(edge.cluster, key = cluster6.y, value = connection)
cluster.edge[is.na(cluster.edge)] <- 0

ggplot(edge.cluster, aes(cluster6.x, cluster6.y, fill = log(connection))) +
  geom_tile(alpha = 4.5/5) +
  scale_fill_gradient(low = 'green', high = 'red')

edge.composition <- edge.cluster %>%
  group_by(cluster6.x) %>%
  summarise(occurence = sum(connection))
cluster.composition <- left_join(edge.cluster, edge.composition, by = 'cluster6.x') %>%
  mutate(probability = connection/occurence)

composition.cluster <- spread(cluster.composition[,-3:-4], key = cluster6.y, value = probability)
composition.cluster[is.na(composition.cluster)] <- 0

#gragh.cluster <- graph_from_adjacency_matrix(as.matrix(cluster.edge[7:12,7:12]), weighted=TRUE, mode="undirected")
#plot(gragh.cluster)

#
cluster.feature <- cluster.composition %>%
  group_by(cluster6.x) %>%
  top_n(5, probability)

##
order <- left_join(purchase, category.spectral[,c('cate_id', 'centr_eigen', 'cluster6')], by = 'cate_id') %>%
  group_by(buyer_admin_id, cluster6) %>%
  summarise(count = n())
order.composition <- order %>%
  group_by(buyer_admin_id) %>%
  summarise(category = sum(count))
order.buyer <- left_join(order, order.composition, by = 'buyer_admin_id') %>%
  mutate(percent = count/category) %>%
  arrange(buyer_admin_id, desc(percent))

## #
test <- left_join(purchase.test, category.spectral[,c('cate_id', 'centr_eigen', 'cluster6')], by = 'cate_id') %>%
  group_by(buyer_admin_id, cluster6) %>%
  summarise(count = n())
test.composition <- test %>%
  group_by(buyer_admin_id) %>%
  summarise(category = sum(count))
test.content <- left_join(test, test.composition, by = 'buyer_admin_id') %>%
  mutate(quality = count/category) %>%
  arrange(buyer_admin_id, desc(quality))

test.feature <- test.content %>%
  group_by(buyer_admin_id) %>%
  top_n(5, quality) %>%
  left_join(cluster.feature, by = c('cluster6' = 'cluster6.x')) %>%
  mutate(link = quality * probability) %>%
  top_n(5, link) 

feature.composition <- test.feature %>%
  group_by(buyer_admin_id) %>%
  top_n(2, link) %>%
  group_by(cluster6, cluster6.y) %>%
  summarise(link = n()) %>%
  arrange(desc(link))


purchase.gossypium <- subset(purchase.spectral, cluster6 %in% as.matrix(feature.composition[1,-3]))
test.gossypium <- subset(test.spectral, cluster6 %in% as.matrix(feature.composition[1,-3]))  

linkage.gossypium <- rbind(purchase.gossypium, test.gossypium)
linkage.gossypium$order <- 1
gossypium.linkage <- linkage.gossypium[,-4:-10][,-1] %>%  
  group_by(buyer_admin_id) %>%
  mutate(order = 1,
         sequence = cumsum(order)) %>%
  filter(sequence < 13) %>%
  spread(key = sequence, value = item_id)
colnames(gossypium.linkage)[3:14] <- c('i1', 'i2', 'i3', 'i4', 'i5', 'i6', 'i7', 'i8', 'i9', 'i10', 'i11', 'i12')
##  Notre Chico
gossypium.edge <- bind_rows(gossypium.linkage %>% transmute(edge = paste(i1, i2, sep = '_')),
                            gossypium.linkage %>% transmute(edge = paste(i2, i3, sep = '_')),
                            gossypium.linkage %>% transmute(edge = paste(i3, i4, sep = '_')),
                            gossypium.linkage %>% transmute(edge = paste(i4, i5, sep = '_')),
                            gossypium.linkage %>% transmute(edge = paste(i5, i6, sep = '_')),
                            gossypium.linkage %>% transmute(edge = paste(i6, i7, sep = '_')),
                            gossypium.linkage %>% transmute(edge = paste(i7, i8, sep = '_')),
                            gossypium.linkage %>% transmute(edge = paste(i8, i9, sep = '_')),
                            gossypium.linkage %>% transmute(edge = paste(i9, i10, sep = '_')),
                            gossypium.linkage %>% transmute(edge = paste(i10, i11, sep = '_')),
                            gossypium.linkage %>% transmute(edge = paste(i11, i12, sep = '_'))
                            ) %>%
  separate(edge, c('from', 'to'), sep = '_') %>%
  filter(from != 'NA' & to != 'NA')  
gossypium.network <- graph_from_edgelist(as.matrix(gossypium.edge[,-1]))

write.csv(feature.composition, 'feature_composition.csv')
write.csv(gossypium.edge, 'gossypium_edge.csv')
write.csv(test.spectral, 'test_spectral.csv')

test.nodes <- test.gossypium$item_id 
item.nodes <- V(gossypium.network)$name

ranking <- match(test.nodes, item.nodes)

adamic.adar <- similarity(gossypium.network, method = 'invlogweighted')


# 
purchase.category <- left_join(purchase, buyer.cluster, by = 'buyer_admin_id') %>%
  left_join(category.spectral, by = 'cate_id') %>%
  filter(cluster6 %in% as.matrix(feature.composition[1:5,-3]))

purchase.category.composition <- purchase.category %>%
  group_by(cluster6) %>%
  summarise(order = n(),
            buyer = n_distinct(buyer_admin_id),
            item = n_distinct(item_id),
            category = n_distinct(cate_id),
            store = n_distinct(store_id))

purchase.gossypium <- subset(purchase.category, cluster6 %in% as.matrix(feature.composition[1,-3])) 
purchase.gossypium.count <- purchase.gossypium %>%
  group_by(buyer_admin_id) %>%
  summarise(item_count = n())

order.gossypium <- purchase.gossypium %>%
  group_by(buyer_admin_id, item_id) %>%
  summarise(appearance = n()) %>%
  left_join(purchase.gossypium.count, by = 'buyer_admin_id') %>%
  mutate(rate = appearance/item_count)

test.gossypium <- subset(test.feature, cluster6 == as.character(feature.composition[1,1]) & cluster6.y == as.character(feature.composition[2,1]))
  
#prediction <- left_join(purchase.test, order.gossypium, by = 'item_id') %>%
#  left_join(test.gossypium, by = c('buyer_admin_id.y' = 'buyer_admin_id'))

prediction <- left_join(purchase.test, order.gossypium, by = 'item_id')
  
#neighbors(network.multiple, 1754)

graph.multiple <- graph_from_edgelist(as.matrix(subset(edge.degree[,1:2], from == '2081'| to == '2081')), directed = FALSE)
#plot(graph.multiple)

V(graph.multiple)$size <- 5 
V(graph.multiple)$frame.color <- "white" 
V(graph.multiple)$color <- "orange" 
V(graph.multiple)$label <- "" 
E(graph.multiple)$arrow.mode <- 0 
plot(graph.multiple)

l <- cbind(1:vcount(graph.multiple), c(1, vcount(graph.multiple):2)) 
l <- layout_as_star(graph.multiple, center = '85')
l <- layout_in_circle(graph.multiple)
plot(graph.multiple, layout = layout_nicely(graph.multiple, dim = 3))



  group_by(buyer_admin_id) %>%
  summarise(text = ~paste(cate_id, sep = "_"))

filter <- subset(category, cate_freq > 0.001211)
filter.network <- subset(cate.network[,-4], cate.network$cate_id %in% filter$cate_id)
filter.network$category <- 1

network.cate <- spread(filter.network, 
                       key = cate_id, value = category)  
network.cate[is.na(network.cate)] <- 0
