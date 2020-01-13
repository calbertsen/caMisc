## https://personal.sron.nl/~pault/data/colourschemes.pdf


greenBlindness <- function(red, green, blue, alpha = 1, names = NULL, maxColorValue = 255){
    R <- (4211 + 0.677 * (green/maxColorValue * 255) ^ 2.2 +
          0.2802 * (red/maxColorValue * 255) ^2.2)^1/2.2
    G <- (4211 + 0.677 * (green/maxColorValue * 255) ^ 2.2 +
          0.2802 * (red/maxColorValue * 255) ^2.2)^1/2.2
    B <- (4211 + 0.98724 * (blue/maxColorValue * 255) ^ 2.2 +
          0.02138 * (green/maxColorValue * 255) ^ 2.2 -
          0.02138 * (red/maxColorValue * 255) ^2.2)^1/2.2
    rgb(R,G,B,alpha = alpha, names = names, maxColorValue = 1)
}

    
