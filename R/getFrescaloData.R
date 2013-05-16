#' Get the data required to perform Frescalo analysis
#' 
#' Mark Hill's Frescalo method calculates trends in the frequency of a species 
#' over time. It attempts to correct the frequency with which a species has been
#' recorded in a series of time periods using the total amount of recording. 
#' This is done by identifying the commonest species in a neighbourhood around a
#' given location and then quantifying the recording effort in terms of the 
#' proportion of the commonest species that were recorded in the neighbourhood. 
#' The basic assumption is that the more recording, the greater the proportion 
#' of commoner species that will be discovered. One of the inputs required is a 
#' set of observations consisting of unique combinations of location, species 
#' and time period. \code{getFrescaloData} extracts this information from
#' observations obtained from the NBN Gateway and writes them to a file in a
#' suitable format for Frescalo. Grid squares are used to provide the locations.
#' 
#' @export
#' @import foreach
#' @import doParallel
#' @param file the file to which output will be written
#' @param species a data.frame with two columns, \code{tvk} and \code{name}, 
#'   containing TVKs and names for the species you want to model (see details). 
#'   These should be character columns, not factors!
#' @param datasets a list of the dataset IDs (8-character strings) for datasets 
#'   on the NBN Gateway from which you want data extracted. If this is not 
#'   included, all publicly available data will be extracted.
#' @param periods a list with two items, \code{breakYear} and \code{plabel},
#'   which specify the periods to use (see details).
#' @param location the size of grid square to use for locations. Can be
#'   \code{sq10km} (default), \code{sq5km}, \code{tetrad} or \code{sq1km}.
#' @param gridSystem indicates whether to allow \code{OSGB} or \code{OSNI} grid 
#'   squares to be used as locations. If it is left NULL (default), then grid
#'   squares from either system will be used.
#' @return integer the number of unique data rows written to the output file
#' @details \subsection{Specifying species}{The format of the \code{species} 
#'   parameter is the same as that for \code{\link{getSDMdata}}, i.e. a data 
#'   frame with columns \code{tvk} and \code{name}, but a large number of 
#'   species will normally be involved. This is because the Frescalo method 
#'   corrects for recording effort using the amount of recording of common 
#'   species in the same group. You will therefore need to list all the species 
#'   covered, e.g. all the species in a family. This is most conveniently done
#'   as an external file with a line for each TVK. In preparing this file, you
#'   will also need to consider how the species should be aggregated. For 
#'   example, there may be subspecies or named forms where you wish to combine 
#'   the observations under the name of the species or species which need to be 
#'   aggregated, like a recently split group of sibling species. This can be 
#'   achieved by assigning the same entry in the \code{name} column to two or 
#'   more entries in the \code{tvk} column.
#'   
#'   Example (extracts from a CSV file):
#'   
#' \code{name,tvk}\cr
#' \code{Anasimyia contracta,NBNSYS0000007039}\cr
#' \code{Anasimyia interpuncta,NBNSYS0000007040}\cr
#' \code{Anasimyia lineata,NBNSYS0000007041}\cr
#' \code{...}\cr
#' \code{Platycheirus peltatus agg,NBNSYS0000006879}\cr
#' \code{Platycheirus peltatus agg,NBNSYS0000006886}\cr
#' \code{Platycheirus peltatus agg,NBNSYS0000033188}\cr
#' \code{...}\cr
#' \code{Volucella bombylans,NBNSYS0000007094}\cr
#' \code{Volucella bombylans,NBNSYS0000172195}
#' 
#' Here \emph{Platycheirus peltatus} was split into a group of sibling species 
#' recently, so they are being combined under the aggregate name 
#' \emph{Platycheirus peltatus agg}. Also the named forms of \emph{Volucella 
#' bombylans} are being combined under the species' name.
#' 
#' If this is stored as \code{syrphidae.csv}, it can be loaded as follows:\cr
#' \code{sp <- read.csv("/path/syrphidae.csv", as.is=TRUE)}
#' }
#' \subsection{Constructing periods}{Periods are specified as a list with two
#' items: \code{breakYear} and \code{plabel}. \code{breakYear} should contain a
#' list of year numbers starting at the earliest year you want to include and
#' finishing with the latest. The number and size of steps is up to you and
#' periods do not have to be of equal sizes. \code{plabel} provides the labels
#' which will be used to identify the periods in the output file. The labels
#' should be in character format and there should be one less label than the
#' number of breaks.
#' 
#' For example:\cr
#' \code{periods <- list()}\cr
#' \code{periods$breakYear <- seq(from=1980, to=2012, by=2)}\cr
#' \code{periods$plabel <- as.character(seq(from=1980, to=2010, by=2))}
#' 
#' It is believed to be good practice to choose your break points so that
#' roughly equal numbers of observations fall in each period. Since the amount
#' of recording (or at least the number of records that have been captured in
#' databases!) has tended to increase over time for many recording schemes, this
#' implies that the earlier periods will most likely need to be longer than the
#' more recent ones.
#' }
#' \subsection{Parallelism}{If the number of species to be covered is 
#' substantial, this function will take some time (many minutes) to run. The way
#' that the function works is to find all unique values in the \code{name} 
#' column of the \code{species} parameter, then process each of these separately
#' - using the corresponding \code{tvk} entry(s) to call 
#' \code{\link{getOccurrences}} and get observations for that species. The
#' unique location/species name/period combinations are then extracted and
#' appended to the growing output file. This is "embarrassingly parallel" and
#' scales almost linearly with the number of CPUs available to run it, i.e. a
#' dual core machine will take only slightly more than half as long as a single
#' core machine. The \code{foreach} package \code{\%dopar\%} operator is used.
#' Therefore you can use any available methods to register a parallel backend
#' for \code{foreach} before calling this function. If no backend is registered
#' and multiple CPUs are detected, they will be used automatically.}
#' @author Stuart Ball, JNCC \email{stuart.ball@@jncc.gov.uk}
#' @seealso \code{\link{gridRef}}, \code{\link{getOccurrences}}, \code{\link{getSDMdata}}
#' @references Hill, M.O., 2012. Local frequency as a key to interpreting
#'   species occurrence data when recording effort is not known. \emph{Methods
#'   in Ecology and Evolution}, 3, 195-205.
#'   \url{http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2011.00146.x/pdf}
#'   
#' @examples \dontrun{ 
#' sp <- data.frame(tvk=c("NBNSYS0000007094","NBNSYS0000172195"),
#'                  name="Volucella bombylans", stringsAsFactors = FALSE)
#' fname <- paste(tempdir(), "/vol_bomb.csv", sep="")
#' periods <- list()
#' periods$breakYear <- seq(from=1980, to=2012, by=2)
#' periods$plabel <- as.character(seq(from=1980, to=2010, by=2))
#' getFrescaloData(fname, species=sp, datasets="SGB00001", periods=periods)
#' }
#' 
getFrescaloData <- function(file=NULL, species=NULL, datasets=NULL, periods=NULL,
                            location=c("sq10km","sq5km","tetrad","sq1km"),
                            gridSystem=NULL) {
    
    ## check parameters
    location <- match.arg(location)
    if (!is.null(gridSystem)) {
        gridSystem <- match.arg(gridSystem, c("OSGB","OSNI"))
    } 
    
    ## check the species parameter
    if (is.null(species)) stop("species parameter required")
    if(!"tvk" %in% colnames(species)) stop("tvk column required in species parameter")
    if(!"name" %in% colnames(species)) stop("name column required in species parameter")
    
    ## check the periods parameter
    if (is.null(periods)) stop("periods parameter required")
    if(!"plabel" %in% names(periods)) stop("plabel item required in periods parameter")
    if(!"breakYear" %in% names(periods)) stop("breakYear item required in periods parameter")
    
    ## check file name
    if (!is.null(file)) {
        file.create(file)
        if (!file.exists(file)) {
            stop(paste("cannot create file:", file))
        }
    } else {
        stop("an output file name is required")
    }
    
    splist <- unique(species$name)
    
    ## has a parallel backend already been registered
    if (!getDoSeqRegistered()) {
        ## do we have more than one core
        ncores = detectCores()
        if (ncores > 1) {
            ## if so register with doParallel
            cl <- makeCluster(ncores)
            registerDoParallel(cl)
        }
    }
    
    ## satisfy R CMD check
    s <- NULL
    name <- NULL
    foreach(s=1:length(splist), .packages=c("rnbn")) %dopar% {
        spname <- splist[s]
        ## get the TVK(s) corresponding to this name
        tvks <- subset(species, subset=(name==spname), select="tvk")$tvk
        
        ## get the occurrences from the NBN
        dt <- getOccurrences(tvks=tvks, datasets=datasets, 
                             startYear=min(periods$breakYear),
                             endYear=max(periods$breakYear))
        
        if ((!is.null(dt)) && (nrow(dt) > 0)){
            spname <- gsub(" ", "_", spname)
            ## dump columns we are not interested in and any duplicate rows
            dt <- unique(dt[, c("location", "startDate", "endDate", "dateTypekey")])
            ## presize the matrix to keep the loop fast
            out <- matrix(nrow=nrow(dt), ncol=3)
            for (i in 1:nrow(dt)) {
                g <- gridRef(dt$location[i], format=location)
                if (!is.null(g)) {
                    if ((is.null(gridSystem)) || (gridSystem == g$system)) {
                        out[i,1] <- g$gfmt
                        out[i,2] <- spname
                        out[i,3] <- datePart(dt[i,2], dt[i,3], dt[i,4], "y")
                    }
                }
            }
            ## coerce the matrix to a data.frame
            out <- as.data.frame(out, stringsAsFactors=FALSE)
            ## code the periods from years
            out$V3 <- cut(as.integer(out$V3), breaks=periods$breakYear, 
                          labels=periods$plabel, include.lowest=TRUE)
            ## drop duplicates
            out <- unique(out)
            ## drop rows with NAs
            out <- out[which(complete.cases(out)),]
            ## append to output file
            write.table(out, file, sep=",", row.names=FALSE, col.names=FALSE,
                        append=TRUE, quote=FALSE)
        }
    }
    
    ## shut down the cluster we created
    if (exists("cl")) stopCluster(cl)
}
