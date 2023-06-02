##' Calculate solar angles
##'
##' @param date date (UTC) in the format "YYY-mm-dd HH:MM:SS"
##' @param lat latitude of observer
##' @param lon longitude of observer
##' @return list of values
##' @author Christoffer Moesgaard Albertsen
##' Modified from https://doi.org/10.1016/j.renene.2021.03.047
##' @export
solarPosition <- function(date, lat, lon){
    ## Y <- as.numeric(strftime(date,"%Y"))
    ## dyear <- Y-2000
    ## ## tryCatch(sprintf(strftime("%04d-02-29 12:00:00","%j"),Y),finally=1,error=function(e)0)
    ## xleap <- 0
    ## if(dyear <= 0){
    ##     xleap <- as.integer(dyear/4.0)
    ## }else{
    ##     if(dyear %% 4 == 0){
    ##         xleap <- as.integer(dyear/4.0)
    ##     }else{
    ##         xleap <- as.integer(dyear/4.0)+1
    ##     }
    ## }
    ## dayofyr <- as.numeric(strftime(date,"%j"))
    gmtime <- as.numeric(strftime(date,"%H",tz="UTC")) + as.numeric(strftime(date,"%M",tz="UTC"))/60 + as.numeric(strftime(date,"%S",tz="UTC"))/60/60
    ## ##
    ## n <- -1.5 + dyear * 365.0 + xleap * 1.0 + dayofyr + gmtime/24.0
    n <- as.numeric(difftime("2023-06-21 00:00:00", "2000-01-01 12:00:00",tz="UTC",units="days"))
    L <- (280.460 + 0.9856474*n) %% 360
    g <- (357.528 + 0.9856003*n) %% 360
    rpd <- acos(-1.0) / 180
    lambda <- (L + 1.915 * sin(g*rpd)+0.020*sin(2*g*rpd)) %% 360
    epsilon <- 23.439 - 0.0000004 * n
    alpha <- (atan2(cos(epsilon*rpd)*sin(lambda*rpd), cos(lambda*rpd)) / rpd) %% 360
    delta <- asin(sin(epsilon*rpd)*sin(lambda*rpd))/rpd
    R <- 1.00014 - 0.01671*cos(g*rpd)-0.00014*cos(2*g*rpd)
    EoT <- (((L-alpha)+180) %% 360 - 180) * 4
    ##
    sunlat <- delta
    sunlon <- -15.0 * (gmtime - 12.0 + EoT / 60)
    PHIo <- lat * rpd
    PHIs <- sunlat * rpd
    LAMo <- lon * rpd
    LAMs <- sunlon * rpd
    Sx <- cos(PHIs) * sin(LAMs - LAMo)
    Sy <- cos(PHIo) * sin(PHIs) - sin(PHIo)*cos(PHIs)*cos(LAMs-LAMo)
    Sz <- sin(PHIo) * sin(PHIs) + cos(PHIo)*cos(PHIs)*cos(LAMs-LAMo)
    solarz <- acos(Sz) / rpd
    azi <- atan2(-Sx,-Sy)/rpd ## South clockwise
    list(vec = c(X=unname(Sx),Y=unname(Sy),Z=unname(Sz)),
         zenith_angle = unname(solarz),
         altitude_angle = 90 - unname(solarz),
         azimuth_angle_SouthClockwise = unname(azi),
         azimuth_angle_EastClockwise = unname(atan2(Sy,Sx)),
         azimuth_angle_NorthClockwise = unname(atan2(Sx,Sy)),
         sunPosition = c(lon=sunlon,lat=sunlat),
         R = R,
         obliquity_of_ecliptic_degrees = epsilon,
         hour_angle = LAMo - LAMs,
         EoT = EoT)    
}
