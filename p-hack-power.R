# load packages
library(plyr) 
library(ggplot2)


# Set up the similation arguments:
#   deltas 0 to 1, in increments of .01
#   batch sizes of 5, 15, 30, 50, 75 per cell
#   up to 1, 2, 3, 4, or 5 batches per study
delta <- rep(c(0:100), each = 25)
delta <- delta / 100
simid <- c(1:length(delta))
batchSize <- rep(c(5, 15, 30, 50, 75), each = 5)
nbatches <- rep(c(1, 2, 3, 4, 5), 5)
batchSize <- rep(batchSize, 101)
nbatches <- rep(nbatches, 101)
maxSize <- nbatches * batchSize


# Create a data frame containing the arguments for each similation
simValues <- data.frame(cbind(simid, delta, batchSize, nbatches, maxSize))


# Run the simulations
source("p-hack-return-power.R")
set.seed(4)
results <- ddply(simValues, 
                 .variables = "simid",
                 function (x) phack(initialN=x$batchSize, hackrate=x$batchSize,
                                    grp1M=x$delta, maxN=x$maxSize, 
                                    alternative="two.sided", sims=1000, 
                                    graph=FALSE, output="power", quiet=TRUE, 
                                    ), 
                 .progress="text")


# Add the power stats to the data frame
simValues$power <- results$V1


# Create a vector categorizing power as below/above fifty percent
simValues$fifty.percent <- ifelse(simValues$power >= .5, c("above"), c("below"))


# A function to map batchSize and nbatches to labels
batch_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="batchSize") { 
    value <- paste(value, "per cell")
  }
  if (var=="nbatches") { 
    value <- paste(value, "batches")
  }
  return(value)
}


# Function to force the axis labels to two decimals
fmt <- function(){
  f <- function(x) as.character(round(x,2))
  f
}


# plot the power for each sim by delta, batch size, and number of batches
simsplot <- ggplot(simValues, aes(x=delta, y=power, colour=fifty.percent)) + 
  geom_point() + facet_grid(batchSize ~ nbatches, labeller=batch_labeller) + 
  scale_x_continuous(labels = fmt()) +
  theme(legend.position="none", axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"), 
        axis.text.x  = element_text(colour="#000000"), 
        axis.text.y  = element_text(colour="#000000"))


# save that plot!
ggsave(filename = "simsplot.png", plot = simsplot, dpi = 100, 
       width = 8, height=7)