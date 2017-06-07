#' Visualize GPX files
#'
#' Interactive Map from OSMtracker GPS tracks
#'
#' @return ReturnValue
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
#' @seealso \code{\link{help}}, \code{\link{help}}
#' @keywords aplot
#' @importFrom plotKML readGPX
#' @importFrom OSMscale earthDist
#' @export
#' @examples
#'
#' file <- system.file("extdata/Dranse_2017-06-06.gpx", package="visGPX")
#' visGPX(file)
#'
#' @param file  GPX filename
#' @param df    Optional data.frame with columns lon,lat,ele (num), time (POSIXct)
#'              and runtime (num, in seconds).
#'              DEFAULT: NULL (generated from \code{file})
#' @param dummy Dummy
#' @param \dots Further arguments passed to \code{\link{plot}}
#'
visGPX <- function(
file,
df=NULL,
dummy,
...
)
{
# check if plotKML is available:
if(!requireNamespace("plotKML", quietly=TRUE))
  stop("plotKML is not installed. Please first run  install.packages('plotKML')")

# Read from file if df is not provided:
if(!is.null(df))
{
df <- plotKML::readGPX(file)
df <- df$tracks[[1]][[1]]
df$ele <- as.numeric(df$ele)
df$time <- as.POSIXct(df$time, format="%Y-%m-%dT%H:%M:%SZ")
df$runtime <- as.numeric(df$time - df$time[1])
}

warning("Dude, it's done.")
return(NULL)

# compute speed at each point:
df$timediff <- c(0, as.numeric(diff(df$runtime))  )
df$dist <- OSMscale::earthDist("lat", "lon", data=df, along=TRUE)


#
# d$km <- cumsum(d$dist)/1000 # Strecke
# d$kmh <- d$dist/d$diff/1000*3600
# d$kmh[1:11] <- 0
#
# hist(d$diff, breaks=40, col=4)
# hist(d$diff[d$diff<10], breaks=40, col=4)
# hist(d$dist, breaks=40, col=4)
# hist(d$kmh, breaks=40, col=4)
#
# # Werte >10 km/h auf NA setzen:
# d$kmh[d$kmh>10] <- NA
# # Glätten (Mittelwert der halben Minute um Datenpunkt herum):
# d$kmhglatt <- movAv(d$kmh, 12)
# hist(d$kmhglatt, breaks=40, col=4)
#
#
#
#
# # Streckenverlauf berechnen
# d$dist <- distance(d$xm, d$ym, along=TRUE)
# d$km <- cumsum(d$dist)/1000 # Strecke
# kmmarker <- sapply(seq(1,38,1), function(x) which.min(abs(d$km - x)))
#
#
# par(mar=c(0,0,0,0))
# plot(map15t)
# scaleBar(map15t, x=0.5, y=0.1, type="bar")
# points(y~x, data=d, col="black", pch=16, cex=0.4)
# colPoints(x,y,kmhglatt, data=d, zlab="km/h", legargs=list(mar=c(3,1,3,1), cex=2), pch=16, cex=0.3)
# textField(d$x[kmmarker], d$y[kmmarker], seq(1,38,1), cex=0.3)
#
# #d$zeit2 <- as.numeric(d$time - min(d$time,na.rm=T))/3600
# #d$zeit2[is.na(d$zeit2)] <- 0
#
#
# # Verlaufsplots:
# plot(d$km, d$kmh, type="l", las=1, xlab="Distanz [km]",
#      ylab="Geschwindigkeit  [km/h]")
# abline(v=seq(0,40,5), col=8)
# plot(d$km, d$kmhglatt, type="l", las=1, xlab="Distanz [km]",
#      ylab="Geschwindigkeit Geglättet  [km/h]")
# abline(v=seq(0,40,5), col=8)
# plot(d$time, d$kmh, type="l", las=1, xlab="Zeit [UTC (=GMT)]",
#      ylab="Geschwindigkeit  [km/h]")#, xaxt="n")
# #r <- range(d$time, na.rm=T)
# #axis.POSIXct(1, at = seq(r[1], r[2], by = "hour"), format = "%H")
#


}
