# p-hack-simulation
R code for simulating p-hacking, based on Ryne Sherman's phack function

**p-hack-return-power.R** is my modification (so far) of the [original phack function](http://rynesherman.com/phack.r). The modifications change the 
function in four ways:

1. Allow for testing two additional decision rules: 
  1. Continuing only if the means are in the predicted direction;
  2. Stopping only when *p* < .05 has been observed a set number of times, either overall or in a row.
2. Add an `output` argument that changes what is returned, either the original output (data frame of study results) or the proportion of studies returning a significant result.
3. Add a `quiet` argument that allows running the function without printing summary results.
4. Quiet the startup message for the psyc package.

  
**p-hack-power.R** runs a series of simulations with the following properties:
* deltas 0 to 1, in increments of .01
* batch sizes of 5, 15, 30, 50, 75 per cell
* up to 1, 2, 3, 4, or 5 batches per study
* *p*<sub>crit</sub> = .05
* two-tailed *t*-tests
* 1,000 studies per combination of delta, batch size, & number of batches