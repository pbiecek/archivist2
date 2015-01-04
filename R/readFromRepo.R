##    archivist2 package for R
##
#' @title Read an Artifact from a Repository
#'
#' @description
#' \code{readFromRepo} reads an artifact from a local, GitHub or remote \link{Repository}
#' To learn more about artifacts visit \link[archivist]{archivist2-package}.
#' 
#' @details
#' Functions \code{readFromRepo} reads artifacts from the archivist Repositories 
#' stored in a local folder or on Github or remotely. It takes \code{md5hash} as a
#' hook of an artifact, which is a result from \link{writeToRepo} function.
#' For every artifact, \code{md5hash} is a unique string of length 32 that comes out as a result of 
#' \link[digest]{digest} function, which uses a cryptographical MD5 hash algorithm. For more information see \link{md5hash}.
#' 
#' Important: instead of giving the whole \code{md5hash} character, the user can simply give first few characters of the \code{md5hash}.
#' For example, \code{a09dd} instead of \code{a09ddjdkf9kj33dcjdnfjgos9jd9jkcv}. All artifacts with the same corresponing \code{md5hash} 
#' abbreviation will be loaded from \link{Repository}. NOTE: in remote repo only the first match is returned and warrning is triggered.
#' 
#' One may notice that \code{writeToRepo} remembers the original artifact name. By default the \code{readFromRepo()} 
#' returns the value, but by setting \code{value = FALSE} you will force this function to load the artifact directly
#' into the parent environemnt.
#' 
#' @note
#' You can specify one \code{md5hash} (or its abbreviation) per function call. 
#' 
#' @param repo A character denoting an existing repository from which an artifact will be loaded.
#' If set to \code{NULL} (by default), uses the default repository specified in \link{setRepo}.
#' 
#' @param md5hash A character assigned to the artifact as a result of a cryptographical hash function with MD5 algorithm, or it's abbreviation.
#' 
#' @param value If \code{FALSE} then artifacts are loaded into the Global Environment with their original names, 
#' if \code{TRUE} (default) then artifacts are returned as a list of values (if there is more than one artifact)
#' or as a single value (if there is only one arfifact that matches md5hash).
#'
#' @author 
#' Przemyslaw Biecek, \email{przemyslaw.biecek@gmail.com}, Marcin Kosinski, \email{m.p.kosinski@gmail.com}
#' 
#' @examples
#' \dontrun{
#' # TODO: create examples or move from archivist
#' }
#' @family archivist2
#' @rdname loadFromRepo
#' @export
readFromRepo <- function( md5hash, repo = NULL, value = FALSE ){
  stopifnot( is.character( md5hash ) )
  stopifnot( is.character( repo ) | is.null( repo ) )
  stopifnot( is.logical( value ) )
  
  if (is.null(repo))  repo <- get(".currentRepo", envir = .ArchivistEnv )
  
  if (isRemoteRepo(repo)) {
    val_nam <- readFromRemoteRepo(md5hash, repo)
  }
  if (isLocalRepo(repo)) { # local repo -> directory
    val_nam <- readFromLocalRepo(md5hash, repo)
  }
  if (isGitHubRepo(repo)) {
    val_nam <- readFromGitHubRepo(md5hash, repo)
  }
  # 
  
  
}

readFromLocalRepo <- function( md5hash, repo = NULL){
  repo <- endWithSlash(repo)
  
  # what if abbreviation was given
  md5hashList <- executeSingleQuery( dir = repo, 
                             paste0( "SELECT DISTINCT artifact, name FROM tag WHERE artifact LIKE '",md5hash,"%'" ) )
  md5hash <- as.character( md5hashList[, 1] )
  names <- as.character( md5hashList[, 2] )
  
  # using sapply in case abbreviation mode found more than 1 md5hash
  list_values <- lapply(md5hash, function{
    .nameEnv <- new.env()
    nam <- load( file = paste0( repo, "gallery/", md5hash[i], ".rda" ), envir = .nameEnv ) 
    get(x = nam, envir = .nameEnv)
  })
  
  list(values=list_values, 
       names=names)
}

readFromGitHubRepo <- function( md5hash, repo = NULL){
  # md5hash, repo = NULL, user = NULL, branch = "master", repoGit = FALSE, value = FALSE ){
  # it is assumed that repo for github is:
  # GitHub:user/repo/branch/dir....
  
  repoDirGit <- substr(repo, 8, 10^5)
  
  # checking artifacts names and values
  Temp <- downloadGitHubDB( repoDirGit )
  md5hashList <- executeSingleQuery( dir = Temp, realDBname = FALSE,
                                     paste0( "SELECT DISTINCT artifact, name FROM tag WHERE artifact LIKE '",md5hash,"%'" ) )
  md5hash <- as.character( md5hashList[, 1] )
  names <- as.character( md5hashList[, 2] )
    
  file.remove( Temp )
      
  # load artifacts from Repository
  # returns objects as value
  list_values <- lapply(md5hash, function{
    tmpobjectS <- getBinaryURL( paste0( get( x = ".GithubURL", envir = .ArchivistEnv), "/", repoDirGit, 
                          "/gallery/", x, ".rda"), ssl.verifypeer = FALSE )    
    tmpf <- tempfile()
    writeBin( tmpobjectS, tmpf )
    # load
    .nameEnv <- new.env()
    nam <- load( file = tmpf, envir = .nameEnv ) 
    unlink(tmpf)
    #return
    get(x = nam, envir = .nameEnv)
  })
  
  list(values=list_values, 
       names=names)
}
