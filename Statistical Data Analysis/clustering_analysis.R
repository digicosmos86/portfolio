source("common.R")
library(cluster)

wc <- read.csv("word_count_1.csv", stringsAsFactors = FALSE)
convert_month <- c(
  "October" = "Month1",
  "November" = "Month2",
  "December" = "Month3",
  "January" = "Month4",
  "February" = "Month5",
  "March" = "Month6",
  "April" = "Month7",
  "May" = "Month8",
  "June" = "Month9"
)

newdistance <- function(x, y) {
  weight <- seq(from=0.5,to=1.5, length.out = length(x))
  x1 <- x*weight
  y1 <- y*weight
  sum((x1-y1)^2)
}

wc$m <- convert_month[wc$Month.of.Start.time]

wc_wide <- wc %>%
  select(Subjectid, Wordcount, m) %>%
  spread(m, Wordcount, fill = 0) %>%
  select(-Month1 , -Month9) %>%
  rename(ID=Subjectid) %>%
  mutate(ID = factor(ID))

describe(wc_wide)

str(wc)
wc_matrix <- as.matrix(wc_wide %>% select(Month2:Month8))
heatmap(t(wc_matrix), Rowv = NA, scale = 'none', distfun = function(x) {daisy(x, metric = "euclidean", weights = seq(0.5,2.0,ncol(x)))})

clusters <- hclust(daisy(wc_matrix, metric="euclidean", weights = seq(0.5, 2, ncol(wc_matrix))))
plot(clusters)
wc_wide$member <- cutree(clusters, 4)
heatmap(wc_matrix[wc_wide$member==4,], Colv = NA, scale = 'none', distfun = function(x) {daisy(x, metric = "euclidean", weights = seq(0.5,2.0,ncol(x)))})

clusters1 <- hclust(daisy(wc_matrix, metric="euclidean"))
plot(clusters1)
heatmap(t(wc_matrix), Rowv = NA, scale = 'none', distfun = function(x) {daisy(x, metric = "euclidean")})
par(mfrow = c(1,2))
heatmap(t(wc_matrix), Rowv = NA, scale = 'none', distfun = function(x) {daisy(x, metric = "euclidean")})
heatmap(wc_matrix, Colv = NA, scale = 'none', distfun = function(x) {
  weights = seq(from=0.5,to=3.0,length.out=ncol(x))^0.5
  daisy(x*weights, metric = "euclidean")})

heatmap(wc_matrix, Colv = NA, scale = 'none', distfun = function(x) {
  weights = c(seq(from=0.6,to=2.0,length.out=(ncol(x)-1)), 3)^0.5
  daisy(log10(x+1)*weights, metric = "euclidean")})
dev.off()

par(mfrow = c(3,3))
lapply(wc_wide1, function(x) {
  y = hist(log(x), breaks = 100, col = "maroon", main = NA, xlab = NA, ylab=NA)
})

wc_wide1 <- select(wc_wide, Month2:Month8)
wc_wide1[wc_wide1<0]

compute_new_distance <- function(x, suppressLast = FALSE, log = TRUE) {
  from <- 0.5
  to <- 2.0
  if (suppressLast) {
    weights = c(seq(from=from, to=to, length.out=ncol(x)-1), from)
  } else {
    weights = seq(from, to, length.out=ncol(x))
  }
  if (log) 
    x <- log(x+1)
  dist(x*weights, method = "euclidean")
}

heatmap(wc_matrix, Colv = NA, distfun = function(x) { compute_new_distance(x, TRUE, TRUE)}, scale = "row", symm = FALSE)
heatmap(wc_matrix, Colv = NA, scale = "row", symm = FALSE)
cluster1 <- hclust(compute_new_distance(wc_matrix, TRUE, TRUE))
plot(cluster1)
wc_wide$member <- cutree(cluster1, 3)
heatmap(wc_matrix[wc_wide$member == 3,], Colv = NA, distfun = function(x) { compute_new_distance(x, TRUE, TRUE)}, scale = "row")
UDIO_DATASET_CLUSTER <- left_join(wc_wide, UDIO_DATASET_ANALYTIC)
table(UDIO_DATASET_CLUSTER$IEP, UDIO_DATASET_CLUSTER$member)
hist(UDIO_DATASET_CLUSTER[UDIO_DATASET_CLUSTER$member == 2 & UDIO_DATASET_CLUSTER$IEP == "1", "POST_RAPID_WR_AB"])

plot_df <- UDIO_DATASET_CLUSTER %>%
  select(ID, CLASS_ID, IEP, member, PRE_QRI_PCT, POST_QRI_PCT) %>%
  gather(TIME, SCORE, PRE_QRI_PCT, POST_QRI_PCT) %>%
  mutate(TIME = factor(TIME, levels = c("PRE_QRI_PCT", "POST_QRI_PCT")), ID=as.factor(ID))

ggplot(plot_df, aes(TIME, SCORE, group = ID, color = CLASS_ID)) + geom_path(stat="summary", fun.y = "mean") + facet_grid(IEP~member)

plot_df1 <- UDIO_DATASET_CLUSTER %>%
  select(ID, CLASS_ID, IEP, member, PRE_QRI_PCT, POST_QRI_PCT) %>%
  gather(TIME, SCORE, PRE_QRI_PCT, POST_QRI_PCT) %>%
  mutate(TIME = factor(TIME, levels = c("PRE_QRI_PCT", "POST_QRI_PCT")), ID=as.factor(ID))

ggplot(plot_df, aes(TIME, SCORE, group = member, color = CLASS_ID)) + geom_path(stat="summary", fun.y = "mean") + facet_grid(IEP~member)



