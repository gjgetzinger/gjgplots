# make up some data 
sites <- paste0('site',1:10)
sites_class <- c('sw', 'gw', 'ww')[cut(1:10, c(0,3,5,10))]
cpds <- paste0('cpd', 1:20)
cpds_class <- c('pest', 'pharm', 'indust')[cut(1:20, c(0,5,15,20))]

conc <- sapply(cpds_class, function(cpd_class){
	rnorm(
		n = length(sites), 
		mean = switch(cpd_class, pest = 20, pharm = 10, indust = 15), 
		sd = 5
		)
	})

conc_m <- matrix(
	conc, 
	ncol = length(cpds), nrow = length(sites), 
	dimnames = list(sites, cpds)
	)

cpds_class <- factor(cpds_class)
sites_class <- factor(site_class)
col_side_col <- c('green', 'magenta', 'blue')[as.numeric(cpds_class)]
row_side_col <- c('red', 'yellow', 'orange')[as.numeric(sites_class)]

# a basic heatmap 
heatmap(conc_m, ColSideColors = col_side_col, RowSideColors = row_side_col)

# a heatmap using a package (...just OK IMO)
library(gplots)
heatmap.2(conc_m, ColSideColors = col_side_col, RowSideColors = row_side_col)

# my prefered approach (taking control of plotting means that you are not restricted to a predefined layout...if you can dream it you can do it! )
# do clustering on 'raw' values 
hc_row <- hclust(d = dist(conc_m))
hc_col <- hclust(d = dist(t(conc_m)))

# if you want, you can use scaled values in clustering by 
hc_row <- hclust(dist(apply(conc_m,2,scale)))
hc_col <- hclust(dist(apply(conc_m,1,scale)))

# reorder the orginal data by cluster
m <- t(conc_m[hc_row$order,hc_col$order])

# set up plotting layout and parameters
mat <- cbind(matrix(c(7,2,7,1,3,4,7,5,7), ncol = 3), matrix(rep(6,3)))
layout(mat, widths = c(0.25,1,0.1,0.5), heights = c(0.25,1,0.1))
par(mar = rep(0,4), oma = c(10,4,4,10))
# plot dendrograms 
plot(as.dendrogram(hc_col), axes = F, leaflab = 'none', xaxs = 'i', yaxs = 'i')
plot(as.dendrogram(hc_row), horiz = T, axes= F, leaflab = 'none', xaxs = 'i', yaxs = 'i')
# plot main heatmap 
image(m, axes = F, xaxs = 'i', yaxs = 'i', 	col = heat.colors(20))
# plot column- and row-side color bars and add labels 
par(mar = c(0.5,0,0.5,0))
image(matrix(as.numeric(cpds_class)[hc_col$order]), col = c('green', 'magenta', 'blue'), axes = F)
mtext(side = 1, at = seq(0,1, length.out = nrow(m)),  text = rownames(m), las = 2)
par(mar = c(0,0.5,0,0.5))
image(t(matrix(as.numeric(sites_class)[hc_row$order])), col = c('red', 'yellow', 'orange'), axes = F)
mtext(side = 4, at = seq(0,1, length.out = ncol(m)),  text = colnames(m), las = 2)
# add some legends 
par(mar = c(4,0,4,0))
plot(0:1,0:1, 'n', axes = F, xlab = '', ylab = '')
legend(levels(cpds_class), x = 'topright', xpd = T, fill = c('green', 'magenta', 'blue'), inset = 0.1, title = 'cpd class', bty = 'n')
legend(levels(sites_class), x = 'bottomright', xpd = T, fill = c('red', 'yellow', 'orange'), inset=0.1, title = 'site class', bty = 'n')



