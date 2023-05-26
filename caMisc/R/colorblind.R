## https://personal.sron.nl/~pault/data/colourschemes.pdf

##' Emulate green blindness in colors
##'
##' Based on https://personal.sron.nl/~pault/data/colourschemes.pdf
##' @param red Red RGB value (or color HEX code or name - alpha is ignored)
##' @param green Green RGB value
##' @param blue Blue RGB value
##' @param alpha alpha value to use
##' @param names Names for the resulting vector
##' @param maxColorValue Maximum color value for red, green, blue
##' @return New colors
##' @author Christoffer Moesgaard Albertsen
##' @export
greenBlindness <- function(red, green, blue, alpha = 1, names = NULL, maxColorValue = 255){
    if(is.character(red)){
        xx <- col2rgb(red)
        red <- xx["red",]
        green <- xx["green",]
        blue <- xx["blue",]
    }        
    R <- (4211 + 0.677 * (green/maxColorValue * 255) ^ 2.2 +
          0.2802 * (red/maxColorValue * 255) ^2.2)^1/2.2
    G <- (4211 + 0.677 * (green/maxColorValue * 255) ^ 2.2 +
          0.2802 * (red/maxColorValue * 255) ^2.2)^1/2.2
    B <- (4211 + 0.98724 * (blue/maxColorValue * 255) ^ 2.2 +
          0.02138 * (green/maxColorValue * 255) ^ 2.2 -
          0.02138 * (red/maxColorValue * 255) ^2.2)^1/2.2
    rgb(R,G,B,alpha = alpha, names = names, maxColorValue = 1)
}

    
