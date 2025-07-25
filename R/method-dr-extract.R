##' dr_extract generic
##' 
##' @title dr_extract
##' @rdname dr-extract
##' @param result DrResult object
##' @return a list that contains components to construct a 'DrResult' object.
##' @export
##' @author Guangchuang Yu
dr_extract <- function(result) UseMethod("dr_extract")


##' @method dr_extract Rtsne
#' @export
dr_extract.Rtsne <- function(result) {
    ## Rtsne::Rtsne
    drdata <- as.data.frame(result$Y)
    eigenvalue <- NULL
    stress <- NULL
    list(drdata = drdata, eigenvalue = eigenvalue, stress = stress)
}

##' @method dr_extract prcomp
#' @export
dr_extract.prcomp <- function(result) {
    ## stats::prcomp
    drdata <- as.data.frame(result$x)
    eigenvalue <- result$sdev
    stress <- NULL
    list(drdata = drdata, eigenvalue = eigenvalue, stress = stress)
}


##' @method dr_extract ape
#' @export
dr_extract.ape <- function(result) {
    ## ape::pcoa
    drdata <- as.data.frame(result$vectors)
    stress <- NULL
    eigenvalue <- as.numeric(result$values$Eigenvalues)
    list(drdata = drdata, eigenvalue = eigenvalue, stress = stress)
}

##' @method dr_extract smacof
#' @export
dr_extract.smacof <- function(result) {
    ## smacof::mds
    drdata <- as.data.frame(result$conf)
    eigenvalue <- NULL
    stress <- result$stress
    stress <- format(stress, digits=4)
    list(drdata = drdata, eigenvalue = eigenvalue, stress = stress)
}

##' @method dr_extract ecodist
#' @export
dr_extract.ecodist <- function(result) {
    ## ecodist::pco
    drdata <- as.data.frame(result$vectors)
    stress <- NULL
    eigenvalue <- as.numeric(result$values)
    list(drdata = drdata, eigenvalue = eigenvalue, stress = stress)
}


##' @method dr_extract ade4
#' @export
dr_extract.ade4 <- function(result) {
    ## ade4::dudi.pco
    drdata <- as.data.frame(result$tab)
    stress <- NULL
    eigenvalue <- as.numeric(result$eig)
    list(drdata = drdata, eigenvalue = eigenvalue, stress = stress)
}

##' @method dr_extract uwot
## @author Lang Zhou
#' @export
dr_extract.uwot <- function(result) {
    ## uwot::umap
    ## uwot::tumap
    ## uwot::lvish
    drdata <- as.data.frame(result)
    eigenvalue <- stress <- NULL
    list(drdata = drdata, eigenvalue = eigenvalue, stress = stress)
}

##' @method dr_extract default
#' @export
dr_extract.default <- function(result) {
    drdata <- eigenvalue <- stress <- NULL
    if ("points" %in% names(result)) {
        drdata <- as.data.frame(result$points)
    }
    if ("eig" %in% names(result)) {
        eigenvalue <- as.numeric(result$eig)
    }
    if ("stress" %in% names(result)) {
        stress <- result$stress
        stress <- format(stress, digits=4)
    }
    list(drdata = drdata, eigenvalue = eigenvalue, stress = stress)
}

## vegan::metaMDS
## no eigenvalue
##' @method dr_extract monoMDS
#' @export
dr_extract.monoMDS <- dr_extract.default
    
## MASS::sammon
##' @method dr_extract MASS
#' @export
dr_extract.MASS <- dr_extract.monoMDS

## vegan::wcmdscale
## no stress
##' @method dr_extract wcmdscale
#' @export
dr_extract.wcmdscale <- dr_extract.default
    
## labdsv::pco
##' @method dr_extract labdsv
#' @export
dr_extract.labdsv <- dr_extract.wcmdscale

