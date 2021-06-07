## compare conn scores across PCA iterations - troubleshoot

comp <- merge(pca_conn_scores[,6:7], pca_conn_DR_test_scores[,7:8], by='net_id')
names(comp) <- c('net_id','NoDR','WithDR')

summary(comp)
comp$Diff <- comp$WithDR - comp$NoDR
comp$AbsDiff <- abs(comp$Diff)

summary(comp)
hist(comp$Diff)
hist(comp$AbsDiff)


pca_conn_DR_test <- princomp(~ edge_dens + min_cut_lat + net_lakes_n + vert_btwn_centr_norm_mean + artic_pct_inv + DamRate_inv + maxkmNS,
                        data=clus_dat, cor=T, scores=T)
par(mfrow=c(1,1))
screeplot(pca_conn_DR_test, type='l')
summary(pca_conn_DR_test)
loadings(pca_conn_DR_test)
eigenvals(pca_conn_DR_test)

fviz_pca_var(pca_conn_DR_test,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_cos2(pca_conn_DR_test, choice='var', axes=1:2)


# To get a composite of first 2 components, can do pythagorean on scores for PCs 1 and 2, but also can extend pythagorean theorem to use all axes
pca_conn_DR_test_scores <- as.data.frame(scores(pca_conn_DR_test))
pca_conn_DR_test_scores$PCconnall <- sqrt((pca_conn_DR_test_scores$Comp.1 ^2) + (pca_conn_DR_test_scores$Comp.2 ^2))# +
                                       #(pca_conn_DR_test_scores$Comp.3 ^2) + (pca_conn_DR_test_scores$Comp.4 ^2) +
                                       #(pca_conn_DR_test_scores$Comp.5 ^2) + (pca_conn_DR_test_scores$Comp.6 ^2)+
                                        # (pca_conn_DR_test_scores$Comp.7 ^2))
pca_conn_DR_test_scores$net_id <- rownames(pca_conn_DR_test_scores)

pca_conn_scores_test_shp <- merge(lake_network_pts, pca_conn_DR_test_scores, by='net_id', all.x=F)
pca_conn_scores_test_shp_df <- as.data.frame(pca_conn_scores_test_shp@data)
pca_conn_scores_test_shp_df$xCor <- pca_conn_scores_test_shp@coords[,1]
pca_conn_scores_test_shp_df$yCor <- pca_conn_scores_test_shp@coords[,2]

pca_conn_scores.point4<-ggplot(pca_conn_scores_test_shp_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=PCconnall), size=2) +
  ggtitle('Network conn score')
pca_conn_scores.point4$labels$colour = 'Score' # change legend title
pca_conn_scores.point4 + geom_path(data=states_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  #scale_colour_brewer(palette = 'Set1') +
  scale_color_continuous(low='firebrick', high='dodgerblue')+
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())


#write.csv(pca_conn_DR_test_scores, file='Data/Networks/pca_network_conn_scores_DR_2PCs.csv')

dams_df <- dat[,c('net_id','DamRate','DamRate_inv')]
pca_conn_DR_test_scores$net_id <- rownames(pca_conn_DR_test_scores)
pca_conn_DR_test_scores <- merge(pca_conn_DR_test_scores, dams_df, by='net_id')

plot(PCconnall ~ DamRate_inv, data=pca_conn_DR_test_scores, pch=16)
hist(pca_conn_DR_test_scores$PCconnall)

comp <- merge(comp, dams_df, by='net_id')

plot(WithDR ~ DamRate, data=comp, pch=16) #seems to raise the floor, curve it a bit
plot(NoDR ~ DamRate, data=comp, pch=16)


# parallel analysis to determine number of PCs?
# https://www.r-bloggers.com/2016/04/determining-the-number-of-factors-with-parallel-analysis-in-r/

library(relimp, pos = 4)
library(paran)

paran(clus_dat[c("edge_dens","min_cut_lat","net_lakes_n","vert_btwn_centr_norm_mean",
                 "artic_pct_inv","DamRate_inv")], iterations = 5000, centile = 0, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = FALSE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      width = 640, height = 640, grdevice = "png", seed = 0)

# example from R documentation
paran(USArrests, iterations=5000)
