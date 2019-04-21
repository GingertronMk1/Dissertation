require("ggplot2")
require("tidyr")

results <- read.csv("results.csv",header=TRUE,quote="\"")
results$means <- rowMeans(results[, c("controls","understandability","difficulty","stress_level","aesthetics")])
results$pre_tune <- sapply(results$pre_tune, function(x) (if (x == "true") {TRUE} else {FALSE}))

head(results)

meanVGeneral <- ggplot(results, aes(x=means,y=general_gameplay,color=pre_tune, shape=first_played)) +
                geom_point(size=3) +
                scale_x_continuous(breaks=c(0,2,4,6,8,10), limits = c(0, 10)) +
                scale_y_continuous(breaks=c(0,2,4,6,8,10), limits = c(0, 10)) +
                labs(x="Mean of initial 5 aspects"
                    ,y="Overall gameplay"
                    ,color="Pre-Tune?"
                    ,shape="First version played")

ggsave(meanVGeneral, file="meanVGeneral.png")

results2 <- results
results2$pre_tune <- NULL
results2$first_played <- NULL
results2$means <- NULL
results2$general_gameplay <- NULL
results2 <- gather(results2, key="key", value="value")
print(results2)

boxes <- ggplot(results2, aes(x=key, y=value)) +
         geom_boxplot() +
         labs(x="Aspect of gameplay", y="Similarity Score") +
         ggtitle("Ratings including pre-tune")

ggsave(boxes, file="boxes.png")


results3 <- subset(results, pre_tune == TRUE)
results3$pre_tune <- NULL
results3$first_played <- NULL
results3$means <- NULL
results3$general_gameplay <- NULL
results3 <- gather(results3, key="key", value="value")
print(results3)

boxesPost <- ggplot(results3, aes(x=key, y=value)) +
         geom_boxplot() +
         labs(x="Aspect of gameplay", y="Similarity Score") +
         ggtitle("Ratings post-tune")

ggsave(boxesPost, file="boxesPost.png")


