#                               phack
# Description
  # Compares the results for a balanced two independent groups study with given population
  # means and SDs and of a given sample size when using p-hacking versus not.

  # This code is a modified version of Ryne Sherman's original phack function,
  # available at http://rynesherman.com/phack.r . The modifications change the 
  # function in four ways:
  #     1.Allow for testing two additional decision rules: 
  #        a. Continuing only if the means are in the predicted direction;
  #        b. Stopping only when p < .05 has been observed a set number of  
  #           times, either overall or in a row.
  #     2. Add an output argument that changes what is returned, either the
  #        original output (data frame of study results) or the proportion of 
  #        studies returning a significant result.
  #     3. Add a quiet argument that allows running the function without
  #        printing summary results.
  #     4. Quiet the startup message for the psyc package.

# Usage
  # phack(initialN=30, hackrate=5, grp1M=0, grp2M=0, grp1SD=1, grp2SD=1, 
  # maxN=200, successN=1, sequential=FALSE, direction="FALSE"either", alpha=.05, 
  # alternative="greater", graph=TRUE, sims=1000, output="original", 
  # quiet=FALSE)

# Arguments
  # initialN         A numeric element indicating the sample sizes for each
  #                  group in the initially designed study.
  # hackrate         A numeric element indicating the sample size to add
  #                  to each group after a non-signficant result is found.
  # grp1M            A numeric element indicating the population mean for group 1.
  # grp2M            A numeric element indicating the population mean for group 2.
  # grp1SD           A numeric element indicating the population SD for group 1.
  # grp2SD           A numeric element indicating the populatino SD for group 2.
  # maxN             A numeric element indicating the maximum sample size for both
  #                  groups. Or in other words, the size at which the researcher
  #                  will terminate the study regardless of significance.
  # successN         A numeric element indicating the number of critical p-values 
  #                  that must be observed before stopping.
  # sequential       A logical indicating whether critical p-value must be observed
  #                  sequentially (TRUE) or not (FALSE).
  # direction        A character vector indicating whether to check if the means
  #                  are in the predicted direction before continuing. "neither"
  #                  (the default) indicates that the researcher will continue
  #                  regardless of direction. "greater" indicates the researcher
  #                  will only continue if the Group 1 mean is greater than the
  #                  Group 2 mean. "less" indicates the opposite.
  # alpha            A numeric element betweeen .00 and 1.00 indicating the desired
  #                  type I error rate.
  # alternative      A character vector indicating the "sidedness" of the hypothesis
  #                  test. "greater" (the default) indicates that the researcher
  #                  expects group 1 to be greater than group 2. "less" indicates
  #                  the opposite. "two.sided" indicates that the researcher
  #                  has not predicted which group will be greater. Two-tailed
  #                  p-values are used for "two.sided", otherwise one-tailed p-values
  #                  are used.
  # graph            A logical indicating whether graphical output should be displayed.
  # sims             A numeric element indicating the number of simulations with
  #                  the provided inputs that should be run.
  # output           A character vector element indicating the type of output to
  #                  be returned, either a data frame of individual study 
  #                  statistics ("original") or the proportion of significant
  #                  results observed ("power")
  # quiet            A logical indicating whether to print summary results of
  #                  simulation (FALSE) or not (TRUE)

# Details
  # "P-Hacking" is the practice of conducting a study, then checking for statistical
  # signficance (e.g. p < .05). If no statistical significance is found, one collects
  # more data and tests for signficance with the new total sample. This procedure
  # (known as hacking) is repeated until the researcher finds a statistically significant
  # result or gives up (i.e., decides to do a different study). This function simulates
  # this p-hacking procedure for a two independent groups study with equal sample sizes
  # in each group. Using the defaults as an example, this simulates the following:
  # A researcher has a theory that group 1 will have a higher mean on some measured variable
  # than group 2. In reality however, both groups have equal means (grp1M=0; grp2M=0)
  # and standard deviations (grp1SD=1; grp2SD=1). The researcher begins by collecting
  # 30 observations for each group (initialN=30) and conducts a two-sample t-test
  # with the alternative hypothses that group 1 has a higher mean than group 2
  # (alternative="greater"). If the resultant p-value is less than or equal to .05
  # (alpha=.05), the researcher stops the study. If the p-value is greater than .05,
  # the researcher adds 5 new observations to each group (hackrate=5) and conducts
  # the two-sample t-test again. This procedure is repeated until statistical significance
  # is reached or until researcher has 200 participants in each group and decides to give
  # up (maxN=200). This entire procedure is then repeated 1000 times (sims=1000) and
  # the results for each trial are output. Some summaries are output in text format.
  # "P-Curves" are plotted for all results reaching statistical signficance for both
  # the initial study and the p-hacked studies for comparison. The resultant data.frame
  # can be used for further investigation.

# Requires
  # {psych}

# See Also

phack <- function(initialN=30, hackrate=5, grp1M=0, grp2M=0, grp1SD=1, grp2SD=1,
                      maxN=200, successN=1, sequential=FALSE, 
                      direction="either", alpha=.05, alternative="greater", 
                      graph=TRUE, sims=1000, output="original", quiet=FALSE) {
  suppressPackageStartupMessages(require(psych))
  outmat <- matrix(NA, nrow=sims, ncol=7)
  for(i in 1:sims) {
    right.dir <- FALSE
    grp1 <- rnorm(initialN, grp1M, grp1SD)
    grp2 <- rnorm(initialN, grp2M, grp2SD)
    initial.r <- cor(c(grp1,grp2), c(rep(1,length(grp1)),rep(0,length(grp2))))
    hackcount <- 0
    initialp <- t.test(grp1, grp2, alternative=alternative)$p.value
    currp <- initialp
    pcritN <- 0
    if(direction == "greater" & (mean(grp1) - mean(grp2)) > 0 | 
       direction == "less" & (mean(grp1) - mean(grp2)) < 0 |
       direction == "either") {
         right.dir <- TRUE
       }
    while(maxN > length(grp1) & pcritN < successN & right.dir == TRUE) {
      hackcount <- hackcount + 1
      grp1 <- c(grp1, rnorm(hackrate, grp1M, grp1SD))
      grp2 <- c(grp2, rnorm(hackrate, grp2M, grp2SD))
      currp <- t.test(grp1, grp2, alternative=alternative)$p.value
      if(currp < alpha) {pcritN <- pcritN + 1} 
      if(currp >= alpha & sequential==TRUE) {pcritN <- 0}
      if(direction == "greater" & (mean(grp1) - mean(grp2)) <= 0 | 
         direction == "less" & (mean(grp1) - mean(grp2)) >= 0) {
           right.dir <- FALSE
         }
    }
    Nadded <- length(grp1) - initialN
    est.r <- cor(c(grp1, grp2), c(rep(1, length(grp1)),rep(0, length(grp2))))
    outmat[i,] <- c(initialp, hackcount, currp, Nadded, initial.r, est.r, pcritN)
  }
  outmat <- data.frame(outmat)
  colnames(outmat) <- c("Initial.p", "Hackcount", "Final.p", "NAdded", 
                        "Initial.r", "Final.r", "N.p.crit")
  InitialSigProb <- sum(outmat$Initial.p <= alpha) / sims
  AvgHacks <- mean(outmat$Hackcount)
  FinalSigProb <- sum(outmat$Final.p <= alpha) / sims
  AvgNAdded <- mean(outmat$NAdded)
  AvgTotN <- mean(initialN + outmat$NAdded)
  StopProb <- sum((initialN + outmat$NAdded)==maxN) / sims
  if(quiet==FALSE) {
    cat("Proportion of Original Samples Statistically Significant =", InitialSigProb, "\n")
    cat("Proportion of Samples Statistically Significant After Hacking =", FinalSigProb, "\n")
    cat("Probability of Stopping Before Reaching Significance =", StopProb, "\n")
    cat("Average Number of Hacks Before Significant/Stopping =", AvgHacks, "\n")
    cat("Average N Added Before Significant/Stopping =", AvgNAdded, "\n")
    cat("Average Total N", AvgTotN, "\n")
    cat("Estimated r without hacking", round(fisherz2r(mean(fisherz(outmat$Initial.r))),2), "\n")
    cat("Estimated r with hacking", round(fisherz2r(mean(fisherz(outmat$Final.r))),2), "\n")
    cat("Estimated r with hacking", round(fisherz2r(mean(fisherz(outmat$Final.r[outmat$Final.p<alpha]))),2), "(non-significant results not included)", "\n") 
  }
  if(graph==TRUE) {
    op <- par(mfrow=c(2,1), las=1, font.main=1)
    hist(outmat$Initial.p[outmat$Initial.p<alpha], xlab="p-values", main="P-curve for Initial Study",
      sub=paste("Number of Significant Studies = ", InitialSigProb*sims, " (", InitialSigProb, ")", sep=""), col="cyan", freq=FALSE)
    lines(density(outmat$Initial.p[outmat$Initial.p<alpha]))
    hist(outmat$Final.p[outmat$Final.p<alpha], xlab="p-values", main="P-curve for p-Hacked Study",
      sub=paste("Number of Significant Studies = ", FinalSigProb*sims, " (", FinalSigProb, ")", sep=""), col="cyan", freq=FALSE)
    lines(density(outmat$Final.p[outmat$Final.p<alpha]))
  }
  if(output=="original") return(outmat)
  if(output=="power") return(FinalSigProb)
}
