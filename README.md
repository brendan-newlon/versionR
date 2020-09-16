# versionR

R functions for saving and restoring timestamped versions of R objects within a subfolder of the working directory.

Use these functions to archive and restore dated and timestamped versions of functions or data objects like dataframes, lists, strings and so on.

# Functions:

## saveVersion(x)
Creates a folder in your working directory called "Env" and a subfolder called "Versions" (if these don't yet exist), then saves the variable object "x" to a date-timestamped RDS file in that directory. 

## latest(x)
This function looks in Env/Versions for the most recent version of the variable object "x" (by date-timestamp) that you saved using the saveVersion function, and restores it as variable "x" in your current R global environment. 

## save_all_vars()
This is similar to saveVersion, but does not require any argument. It saves date-timestamped RDS files for all variables that exist in the open global environment to a new subfolder in ../Env/Versions/save_all_vars

## restore_all_vars()
This is the parallel function to latest(). It looks for the most recent set of variables saved in the ../Env/Versions/save_all_vars subfolder and restores all of them (without timestamps) to your global R environment.
