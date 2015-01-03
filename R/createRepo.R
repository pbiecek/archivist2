##    archivist2 package for R
##
#' @title Create an Empty Local Repository in a Given Directory
#'
#' @description
#' \code{createRepo} creates an empty \link{Repository} in a given directory in which archived artifacts will be stored.
#' 
#' 
#' @details
#' At least one Repository must be initialized before using other functions from the \pkg{archivist2} package. 
#' When working in groups, you share this directory with other users (vis dropbox),
#' you can use github repository, or you can use remote/cloud repository.
#' 
#' In local directory all artifacts are going to be saved in the local Repository folder, with an SQLite 
#' database attached in a file named \code{backpack}. 
#' After calling \code{writeToRepo} function, every artifact will be archived in a \code{md5hash.rda} file. 
#' This file will be saved in a folder (under \code{repoDir} directory) named 
#' \code{gallery}. For every artifact, \code{md5hash} is a unique string of length 32 that comes out as a result of 
#' \link[digest]{digest} function, which uses a cryptographical MD5 hash algorithm.
#' 
#' To learn more about artifacts visit \link[archivist2]{archivist2-package}.
#' 
#' Created \code{backpack} database is a useful and fundamental tool for remembering artifact's 
#' \code{name}, \code{class}, \code{archiving date} etc. (that are remembered as \link{Tags}),
#' or for keeping artifact's \code{md5hash}.
#' 
#' Besides the \code{backpack} database, \code{gallery} folder is created in which all 
#' artifacts will be archived.
#' 
#' After every \code{writeToRepo} call the database is refreshed, so an artifact is available 
#' immediately in \code{backpack.db} database for other collaborators.
#' 
#' @param repoDir A character that specifies the directory for the Repository to be made.
#' 
#' @param force If \code{force = TRUE} function call forces to create \code{repoDir} directory if
#' it did not exist. Default set to \code{force = TRUE}.
#' 
#' @param setAsDefault If \code{setAsDefault = TRUE} newly created repo is set as the default one.
#' 
#' @author 
#' Marcin Kosinski, \email{m.p.kosinski@gmail.com}, Przemyslaw Biecek, \email{przemyslaw.biecek@gmail.com}
#'
#' @examples
#' \dontrun{
#' exampleRepoDir <- tempdir()
#' createRepo( repoDir = exampleRepoDir )
#'
#' # check the state of an empty Repository
#' summaryRepo(  repoDir = exampleRepoDir )
#' 
#' # removing an example Repositories
#' deleteRepo( exampleRepoDir )
#' rm( exampleRepoDir )
#' }
#' @family archivist2
#' @rdname createyRepo
#' @export
createRepo <- function( repoDir, force = TRUE, setAsDefault = TRUE){
  stopifnot( is.character( repoDir ) )
  
  if ( !file.exists( repoDir ) & !force ) 
    stop( paste0("Directory ", repoDir, " does not exist. Try with force=TRUE.") )
  if ( !file.exists( repoDir ) & force ){
    cat( paste0("Directory ", repoDir, " did not exist. Create an empty one.") )
    dir.create( repoDir )
  }
  
  repoDir <- checkDirectory( repoDir )
  
  # create connection
  backpack <- getConnectionToDB( repoDir, realDBname = TRUE )
  
  # create tables
  artifact <- data.frame(md5hash = "",
                         name = "",
                         createdDate = as.character( now() ), 
                         stringsAsFactors = FALSE ) 
  
  tag <- data.frame(artifact = "", 
                    tag = "", 
                    createdDate = as.character( now() ), 
                    stringsAsFactors = FALSE )
  
  # insert tables into database
  dbWriteTable( backpack, "artifact", artifact, overwrite = TRUE, row.names = FALSE )
  dbWriteTable( backpack, "tag", tag, overwrite = TRUE, row.names = FALSE )
  
  
  dbGetQuery(backpack, "delete from artifact")
  dbGetQuery(backpack, "delete from tag")
  
  dbDisconnect( backpack )
  
  # if gallery folder does not exist - make it
  if ( !file.exists( file.path( repoDir, "gallery" ) ) ){
    dir.create( file.path( repoDir, "gallery" ), showWarnings = FALSE)
  }
}

addArtifact <- function( md5hash, name, dir ){
  # creates connection and driver
  # send insert
  executeSingleQuery( dir,
              paste0( "insert into artifact (md5hash, name, createdDate) values",
                      "('", md5hash, "', '", name, "', '", as.character( now() ), "')" ) )
}

addTag <- function( tag, md5hash, createdDate = now(), dir ){
 executeSingleQuery( dir,
              paste0("insert into tag (artifact, tag, createdDate) values ",
                     "('", md5hash, "', '", tag, "', '", as.character( now() ), "')" ) )
}

# realDBname was needed because Github version function uses temporary file as database
# and they do not name this file as backpack.db in repoDir directory
getConnectionToDB <- function( repoDir, realDBname ){
    if ( realDBname ){
      conn <- dbConnect( get( "sqlite", envir = .ArchivistEnv ), paste0( repoDir, "backpack.db" ) )
    }else{
      conn <- dbConnect( get( "sqlite", envir = .ArchivistEnv ), repoDir )
    }
    return( conn )
}
  
executeSingleQuery <- function( dir, query, realDBname = TRUE ) {
  conn <- getConnectionToDB( dir, realDBname )
  res <- dbGetQuery( conn, query )
  dbDisconnect( conn )
  return( res )
}

readSingleTable <- function( dir, table, realDBname = TRUE ){
  conn <- getConnectionToDB( dir, realDBname )
  tabs <- dbReadTable( conn, table )
  dbDisconnect( conn )
  return( tabs )
}

# for Github version function that requires to load the database
downloadDB <- function( repo, user, branch, repoDirGit ){
   if( is.logical( repoDirGit ) ){
     URLdb <- paste0( get( ".GithubURL", envir = .ArchivistEnv) , user, "/", repo, "/", branch, "/backpack.db") 
   }else{
     URLdb <- paste0( get( ".GithubURL", envir = .ArchivistEnv) , user, "/", repo, "/", branch, "/", repoDirGit, "/backpack.db") 
   }
   db <- getBinaryURL( URLdb, ssl.verifypeer = FALSE )
   Temp2 <- tempfile()
   file.create( Temp2 )
   writeBin( db, Temp2 )
   return( Temp2 )
}

checkDirectory <- function( directory ){
  # check if global repository was specified by setLocalRepo
  if ( is.null(directory) ){
    directory <- get(  ".repoDir", envir = .ArchivistEnv )    
  } else {
    # check if repoDir has "/" at the end and add it if not
    if ( !grepl("/$", x = directory , perl=TRUE) ) {
      directory <- paste0(  directory, "/"  )
    }
  }
  return( directory )
}

isRemoteRepo <- function(repo) {
  (substr(repo, 1, 7) == "http://") || (substr(repo, 1, 8) == "https://")
}

isGitHubRepo <- function(repo) {
  substr(repo, 1, 7) == "GitHub:"
}

isLocalRepo <- function(repo) {
  (!isRemoteRepo(repo)) & (!isGitHubRepo(repo))
}
