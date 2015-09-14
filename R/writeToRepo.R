##    archivist2 package for R
##
#' @title Write an Artifact into a Repository
#'
#' @description
#' \code{writeToRepo} function writes an artifact to the local or remote \link{Repository}.
#' To learn more about artifacts visit \link[archivist]{archivist2-package}.
#' 
#' @details
#' \code{writeToRepo} function writes an artifact to the local Repository (a directory) or remote Repository.
#' In local repository artifacts are saved with additional properties like tags,
#' which make them easier to find. 
#' Every artifact is archived in a \code{md5hash.rda} file. For every artifact, \code{md5hash} is a 
#' unique string of length 32 that comes out as a result of 
#' \link[digest]{digest} function, which uses a cryptographical MD5 hash algorithm.
#' 
#' A specific \code{Tag}-relation 
#' is going to be added to the \code{backpack} dataset in case there is a need to load 
#' the artifact with it's related data set - see \link{readFromRepo}. Default settings
#' may be changed by using the \code{archiveTag} argument with the \code{FALSE} value.
#' 
#' \code{Tags} are artifact's attributes, different for various artifact's classes. For more detailed 
#' information check \link{Tags}
#' 
#' Archived artifact can be searched in the \code{backpack} dataset by using the
#' \link{searchInRepo} functions. Artifacts can be searched by their \link{Tags}, 
#' \code{names}, \code{classes} or \code{archiving date}.
#' 
#' Supported artifact's classes are (so far): 
#' \itemize{
#'  \item \code{lm},
#'  \item \code{data.frame},
#'  \item \code{ggplot},
#'  \item \code{htest},
#'  \item \code{trellis},
#'  \item \code{twins (result of agnes, diana or mona function)},
#'  \item \code{partition (result of pam, clara or fanny fuction)},
#'  \item \code{lda},
#'  \item \code{qda},
#'  \item \code{glmnet},
#'  \item \code{survfit}.
#'  }
#'  
#' To check what \code{Tags} will be extracted for various artifacts see \link{Tags}.
#' 
#' @return
#' As a result of this function a character string is returned, which determines
#' the \code{md5hash} of the artifact that was used in the \code{writeToRepo} function. 
#' 
#' @seealso
#'  For more detailed information check the \pkg{archivist2} package 
#' \href{https://github.com/pbiecek/archivist2#-the-list-of-use-cases-}{Use Cases}.
#' The list of supported artifacts and their tags is available on \code{wiki} on \pkg{archivist2} 
#' \href{https://github.com/pbiecek/archivist2/wiki/archivist2-package---Tags}{Github Repository}.
#' 
#' 
#' @note 
#' One can specify his own \code{Tags} for artifacts by setting artifact's attribute 
#' before call of the \code{writeToRepo} function like this: 
#' \code{attr(x, "tags" ) = c( "name1", "name2" )}, where \code{x} is artifact 
#' and \code{name1, name2} are \code{Tags} specified by an user.
#' 
#' @param artifact An R object to be saved. For supported artifacts see details.
#' 
#' @param archiveTags A logical value denoting whether to archive tags from the \code{artifact}.
#' 
#' @param algo The algorithm to be used for hashing. Will be passed to \code{digest()}.
#' 
#' @param repo A character denoting an repository in which an artifact will be saved.
#' If set to \code{NULL} (by default), uses the \code{repo} specified in \link{setRepo}.
#' 
#' @param compress The type of compression, passed to \code{save()}.
#' 
#' @param compression_level The level of compression, passed to \code{save()}.
#' 
#' @author 
#' Przemyslaw Biecek, przemyslaw.biecek_at_gmail.com, Marcin Kosinski, m.p.kosinski_at_gmail.com
#'
#' @examples
#' # objects preparation
#' \dontrun{
#' # TODO: create examples or move from archivist
#' }
#' @family archivist2
#' @rdname writeToRepo
#' @export
writeToRepo <- function(artifact, repo = NULL, archiveTags = TRUE, 
                        algo = "md5", compress = "xz", compression_level = 9){
  stopifnot( is.logical( archiveTags  ) )
  stopifnot( is.character( repo ) | is.null( repo ) )
  
  md5hash <- digest( artifact, algo = algo)
  objectName <- deparse( substitute( artifact ) )

  if (is.null(repo))  repo <- get(".currentRepo", envir = .ArchivistEnv )
  
  allTags = ""
  if (archiveTags) {
    extractedTags <- extractTags( artifact, objectNameX = objectName )
    userTags <- attr( artifact, "tags" ) 
    allTags <- c( extractedTags, userTags )
  }
  
  if (isRemoteRepo(repo)) {
    writeToRemoteRepo(md5hash, objectName, artifact, allTags, repo, 
                      compress=compress, compression_level=compression_level)
  }
  if (isLocalRepo(repo)) { # local repo -> directory
    writeToLocalRepo(md5hash, objectName, artifact, allTags, repo, 
                     compress=compress, compression_level=compression_level)
  }
  if (isGitHubRepo(repo)) 
      stop("GitHub repos are read-only")
  # 
  return( md5hash )
}


writeToRemoteRepo <- function(md5hash, objectName, artifact, allTags, repo, compress = "xz", compression_level = 9) {
  tmpdir <- tempdir()
  tmpfile <- paste0(tmpdir, "/", md5hash, ".rda")
  save(artifact, file=tmpfile, compress = compress, compression_level = compression_level)

  repoUploadUrl <- paste(endWithSlash(repo), get(".remoteUploadScript", envir = .ArchivistEnv) )
  result <- POST(repoUploadUrl,
                 body = list(fileToUpload = upload_file(tmpfile),
                             name = objectName,
                             md5hash = md5hash,
                             createdDate = as.character(now()),
                             tags = paste(allTags, collapse="\n"),
                             submit = get(".remoteRepoAntiSpam", envir = .ArchivistEnv )))
  unlink(tmpfile)
  result <- rawToChar(result$content)
  if (result != get( ".remoteConfirmation", envir = .ArchivistEnv ))
    stop(result)
}

writeToLocalRepo <- function(md5hash, objectName, artifact, allTags, repo, compress = "xz", compression_level = 9) {
  repo <- checkDirectory( repo )
  
  # check if that artifact might have been already archived
  check <- executeSingleQuery( dir = repo , realDBname = TRUE,
                               paste0( "SELECT * from artifact WHERE md5hash ='", md5hash, "'") )[,1] 
  if ( length( check ) > 0) 
    stop( "This artifact was already archived\n")
  
  # save artifact to .rd file
    assign( value = artifact, x = md5hash, envir = .ArchivistEnv )
    save( file = paste0(repo, "gallery/", md5hash, ".rda"),  
          list = md5hash, envir = .ArchivistEnv,
          compress = compress, compression_level = compression_level)
    rm(list = md5hash, envir = .ArchivistEnv)
  
  addArtifact( md5hash, name = sQuote(objectName), dir = repo ) 
  
  # whether to add tags
  if (archiveTags) {
    sapply( allTags, addTag, md5hash = md5hash, dir = repo )
  }
  
}
