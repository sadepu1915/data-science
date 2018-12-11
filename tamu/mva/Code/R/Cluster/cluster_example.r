####
#### Toy example to illustrate hierarchical clustering.
####

D <- as.dist(matrix(c(0, 1, 11, 5, 1, 0, 2, 3, 11, 2, 0, 4, 5, 3, 4, 0), nrow = 4))

D <- as.dist(matrix(c(0, 3,2,5,0,0,4,1,0,0,0,7,0,0,0,0), nrow = 4))
par(mfrow = c(2, 2))

hc_single <- hclust(D, method = "single")
hc_single
plot(hc_single)

hc_complete <- hclust(D, method = "complete")
plot(hc_complete)

hc_average <- hclust(D, method = "average")
plot(hc_average)
