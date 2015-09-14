##    archivist2 package for R
##
#' @title Set Default Repository
#'
#' @description
#' \code{setRepo} sets default path to the \link{Repository}. 
#' It might be a local repo (directory) GitHub repo (read only) or remote one.
#' For all \code{archivist2} functions if \code{repo = NULL} then the default repo is used.
#' 
#' @details
#' See examples.
#' 
#' @seealso
#' 
#' \href{https://github.com/pbiecek/archivist2/wiki}{https://github.com/pbiecek/archivist2/wiki}
#' 
#' @param localDir A path to existing directory with Reposiotry. 
#' 
#' @param remoteRepo A http:// or https:// addres of script that accepts POST and GET requests.
#'  
#' @param githubUser For github repository it's a github user name.
#' @param githubRepo For github repository it's a github repository name.
#' @param githubBranch For github repository it's a github branch (by default master).
#' @param githubDir For github repository it's a github direcotry (by default global).
#'
#' @author 
#' Przemyslaw Biecek, przemyslaw.biecek_at_gmail.com, 
#' 
#' @examples
#' \dontrun{
#' # examples
#' # TODO: create examples or move from archivist
#' }
#' @family archivist2
#' @rdname setRepo
#' @export
setRepo <- function( localDir = NULL,
                     remoteRepo = NULL,
                     githubUser = NULL,
                     githubRepo = NULL,
                     githubBranch = "master",
                     githubDir = ""){
  if (is.null(localDir) & is.null(remoteRepo) &
        is.null(githubUser) & is.null(githubRepo)) 
    stop("You must select local, github or remote repo")

  if (!is.null(githubUser) & !is.null(githubRepo))
    assign( ".currentRepo", paste("GitHub:", githubUser, "/", githubRepo, "/", githubBranch, "/", githubDir), envir = .ArchivistEnv )
  if (!is.null(remoteRepo))
    assign( ".currentRepo", remoteRepo, envir = .ArchivistEnv )
  if (!is.null(localDir))
    assign( ".currentRepo", localDir, envir = .ArchivistEnv )
}
