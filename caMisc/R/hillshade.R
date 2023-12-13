##' Surface reflectance assuming Lambertian surfaces
##'
##' @param z Matrix of terrain levels
##' @param azimuth Azimuth (North clockwise) of light source
##' @param altitude Altitude (/elevation) angle of light source
##' @return matrix of brightness
##' @author Christoffer Moesgaard Albertsen
##' @references https://people.csail.mit.edu/bkph/papers/Hill-Shading.pdf
##' @examples
##'  z <- getPixelMatrix("~/Downloads/download.png", TRUE)[,,1]
##'  col <- c("lightblue",colorRampPalette(c("darkgreen","green","brown","white"))(254))
##'  v <- col2rgb(col[z+1])
##' pic <- simplify2array(list(matrix(v[1,],nrow(z),ncol(z)),
##'                            matrix(v[2,],nrow(z),ncol(z)),
##'                            matrix(v[3,],nrow(z),ncol(z))))
##' par(mfrow = n2mfrow(4))
##' h1 <- hillshadeReflectance(z,method="Lambertian")
##' imagePlot(pic/255 * as.vector(h1))
##' h1 <- hillshadeReflectance(z,method="Wiechel")
##' imagePlot(pic/255 * as.vector(h1))
##' h1 <- hillshadeReflectance(z,method="Marsik")
##' imagePlot(pic/255 * as.vector(h1))
##' h1 <- hillshadeReflectance(z,method="LommelSeeliger")
##' imagePlot(pic/255 * as.vector(h1))
hillshadeReflectance <- function(z, azimuth = -45, altitude = 45, method = c("Lambertian","Wiechel","Marsik","LommelSeeliger")){
    method <- match.arg(method)
    ## Terrain
    p <- (z[as.vector(pmin(row(z)+1,nrow(z))) + (as.vector(col(z))-1) * nrow(z)] - z[as.vector(pmax(row(z)-1,1)) + (as.vector(col(z))-1) * nrow(z)]) / 2
    q <- (z[as.vector(row(z)) + (as.vector(pmin(col(z)+1,ncol(z)))-1) * nrow(z)] - z[as.vector(row(z)) + (as.vector(pmax(col(z)-1,1))-1) * nrow(z)]) / 2
    ## Light source
    theta0 <- (90 - altitude) * pi / 180
    phi0 <- azimuth * pi / 180
    p0 <- cos(phi0) * tan(theta0)
    q0 <- -sin(phi0) * tan(theta0)
    ## Reflectance
    if(method == "Lambertian"){
        R <- (1 + p0 * p + q0 * q) / (sqrt(1 + p0^2 + q0^2) * sqrt(1 + p^2 + q^2))
    }else if(method == "Wiechel"){
        R <- 2 * (1 + p0 * p + q0 * q) / ((1 + sqrt(1 + p0^2 + q0^2)) * (1 + p^2 + q^2))
    }else if(method == "Marsik"){
        R <- 10 ^((p0*p + q0*q) / (sqrt(p0^2 + q0^2)))
    }else if(method == "LommelSeeliger"){
        R <- (1 + 1 / sqrt(1 + p0^2 + q0^2)) / (1 + sqrt(1 + p0^2 + q0^2) / (1 + p0 * p + q0 * q))
    }else{
        stop("Wrong method")
    }
    R <- pmin(1,pmax(0,R))
    matrix(R,nrow(z),ncol(z))
}
