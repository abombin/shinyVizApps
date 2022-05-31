p<-ggtree::ggtree(data)
metaTree<-p$data%>% dplyr::inner_join(metaDat, c('label'='uuid'))
plotTree<-p+geom_point(data=metaTree, aes( text=label, x = x,
                                        y = y, color=region))


plotly::ggplotly(plotTree)



plotTree<-ggtree::ggtree(data)%<+% metaDat
treeMet<-plotTree$data
plotMeta<-plotTree+geom_point(data=treeMet, aes( label=label, x = x,
                                           y = y, color=region))
plotly::ggplotly(plotMeta)


p1 <- ggtree(tree)
metat <- p1$data %>%
  dplyr::inner_join(dat, c('label' = 'id'))
p2 <- p1 +
  geom_point(data = metat,
             aes(x = x,
                 y = y,
                 colour = grp,
                 label = id))





n_samples <- 20
n_grp <- 4
tree <- ape::rtree(n = n_samples)
# CREATE SOME METADATA ----------------------------------------------------
id <- tree$tip.label
set.seed(42)
grp <- sample(LETTERS[1:n_grp], size = n_samples, replace = T)
dat <- tibble::tibble(id = id,
                      grp = grp)
# PLOT THE TREE -----------------------------------------------------------
p1 <- ggtree(tree)
metat <- p1$data %>%
  dplyr::inner_join(dat, c('label' = 'id'))
p2 <- p1 +
  geom_point(data = metat,
             aes(x = x,
                 y = y,
                 colour = grp,
                 label = id))
plotly::ggplotly(p2)



### working function

plotTree<-ggtree::ggtree(data)%<+% metaDat
treeMet<-plotTree$data
plotMeta<-plotTree+geom_point(data=treeMet, aes( label=label, x = x,
                                                 y = y, color=region))
plotly::ggplotly(plotMeta)
