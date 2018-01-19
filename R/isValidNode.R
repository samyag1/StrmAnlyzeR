isValidNode <- function(comids, ftypes, validComIds){
  stopifnot(sum(is.na(comids)) == 0)
  isStreamRiver = !is.na(ftypes) & ftypes == 'StreamRiver'
  isValidArtificial = comids %in% validComIds
  return(isStreamRiver | isValidArtificial)
}