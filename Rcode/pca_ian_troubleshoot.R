## compare conn scores across PCA iterations - troubleshoot

comp <- merge(pca_conn_scores[,6:7], pca_conn_DR_test_scores[,7:8], by='net_id')
names(comp) <- c('net_id','NoDR','WithDR')

summary(comp)
comp$Diff <- comp$WithDR - comp$NoDR
comp$AbsDiff <- abs(comp$Diff)

summary(comp)
hist(comp$Diff)
hist(comp$AbsDiff)


pca_conn_DR_test <- princomp(~ edge_dens + min_cut_lat + net_lakes_n + vert_btwn_centr_norm_mean + artic_pct_inv + log(DamRate_inv) + maxkmNS,
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
pca_conn_DR_test_scores$PCconnall <- sqrt((pca_conn_DR_test_scores$Comp.1 ^2) + (pca_conn_DR_test_scores$Comp.2 ^2) +
                                       (pca_conn_DR_test_scores$Comp.3 ^2) + (pca_conn_DR_test_scores$Comp.4 ^2) +
                                       (pca_conn_DR_test_scores$Comp.5 ^2) + (pca_conn_DR_test_scores$Comp.6 ^2)+
                                         (pca_conn_DR_test_scores$Comp.7 ^2))

dams_df <- dat[,c('net_id','DamRate','DamRate_inv')]
pca_conn_DR_test_scores$net_id <- rownames(pca_conn_DR_test_scores)
pca_conn_DR_test_scores <- merge(pca_conn_DR_test_scores, dams_df, by='net_id')

plot(PCconnall ~ DamRate_inv, data=pca_conn_DR_test_scores, pch=16)
hist(pca_conn_DR_test_scores$PCconnall)

comp <- merge(comp, dams_df, by='net_id')

plot(WithDR ~ DamRate, data=comp, pch=16) #seems to raise the floor, curve it a bit
plot(NoDR ~ DamRate, data=comp, pch=16)
