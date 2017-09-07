#' Visualize GPX files
#' 
#' Interactive Map from OSMtracker GPS tracks
#' 
#' @return ReturnValue
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
#' @seealso \code{\link{help}}, \code{\link{help}}
#' @keywords aplot
#' @importFrom plotKML readGPX
#' @importFrom OSMscale earthDist pll pmap pointsMap projectPoints
#' @importFrom berryFunctions movAv classify seqPal round0 lim0 checkFile addAlpha colPoints textField
#' @importFrom leaflet %>% leaflet addTiles addCircleMarkers addCircles addPolylines addLabelOnlyMarkers labelOptions
#' @importFrom graphics hist plot lines par
#' @importFrom utils tail
#' @export
#' @examples
#' file <- system.file("extdata/Dranse_2017-06-06.gpx", package="visGPX")
#' # file <- "../../Sonstiges/2017-06-10_PORT.gpx"
#' visGPX(file, threshold_na=13, plot_static=FALSE)
#' 
#' file <- system.file("extdata/rad.gpx", package="visGPX")
#' visGPX(file, plot_static=FALSE) # with waypoints
#' 
#' \dontrun{# Excluded from checks to reduce computing time (map download)
#' dranse <- visGPX(file, threshold_na=13)
#' 
#' # Exporting:
#' pdf("dranse.pdf", height=5)
#' visGPX(file, bgmap=dranse$bgmap) # speed up by re-using static map
#' dev.off()
#' htmlwidgets::saveWidget(dranse$map, "dranse.html")
#' }
#' 
#' @param file    GPX filename
#' @param df      Optional data.frame with columns lon,lat,ele (num) + time (POSIXct)
#'                DEFAULT: NULL (internally generated from \code{file})
#' @param element Numeric: Element in GPX tracks list to be used. DEFAULT: 1
#' @param threshold_na Values above this speed are set to NA for smoothing.
#'                Use e.g. 15 (kmh) for implausible values when walking.
#'                DEFAULT: NA (ignored)
#' @param smooth  Smoothing width passed to \code{berryFunctions::\link{movAv}}
#'                for speed column. DEFAULT: 5
#' @param plot_interactive Logical: plot leaflet map? DEFAULT: TRUE
#' @param plot_static Logical: plot static graphs? DEFAULT: TRUE
#' @param bgmap   Static background map from \code{OSMscale::\link{pointsMap}}.
#'                Plotting is done faster if provided. DEFAULT: NULL
#' @param mfrow   Multiple figures \link{par}ameter. DEFAULT: c(2,2)
#' @param keeppar Logical: keep paramters of plot (to add to last panel /
#'                outer margins)? DEFAULT: FALSE
#' @param \dots   Further arguments passed to \code{OSMscale::\link{pointsMap}}
#' 
visGPX <- function(
file,
df=NULL,
element=1,
threshold_na=NA,
smooth=9,
plot_interactive=TRUE,
plot_static=TRUE,
bgmap=NULL,
mfrow=c(2,2),
keeppar=FALSE,
...
)
{

# Read from file if df is not provided ----
if(is.null(df))
{
berryFunctions::checkFile(file)
df <- plotKML::readGPX(file)
wp <- df$waypoints
df <- df$tracks[[1]][[element]]
df$ele <- as.numeric(df$ele)
df$time <- strptime(df$time, format="%Y-%m-%dT%H:%M:%SZ")
}

# compute speed at each point ----

df$run_time_s <- as.numeric(df$time - df$time[1])
df$run_time_s <- c(0, diff(df$run_time_s) )
df$run_dist_m <- OSMscale::earthDist("lat", "lon", data=df, r=6371e3, along=TRUE)
df$speed_kmh <- df$run_dist_m/df$run_time_s/1000*3600
df$speed_kmh[1] <- 0


# Smoothing speed ----

# Set values >x km/h to NA:
df$speed_NAs <- df$speed_kmh
if(!is.na(threshold_na)) df$speed_NAs[df$speed_NAs>threshold_na] <- NA
# Smoothing (average of values around point):
df$speed_smooth_kmh <- berryFunctions::movAv(df$speed_NAs, width=smooth)

# kilometer markers ----

df$run_dist_cum <- cumsum(df$run_dist_m)/1000 # path length in km
kms <- max(round(df$run_dist_cum))
if(kms>=1)
{
kms <- seq(1,kms,1)
kmmarker <- sapply(kms, function(x) which.min(abs(df$run_dist_cum - x)))
}
kmmarker

avg <- tail(df$run_dist_cum,1)/as.numeric(difftime(tail(df$time,1), df$time[1], units="h"))
avg <- round(avg,3)



# Interactive map ----

if(plot_interactive)
{
df$col <- seqPal(100)[classify(df$speed_smooth_kmh)$index]
df$col[is.na(df$col)] <- "grey"
df$display <- paste0(berryFunctions::round0(df$speed_kmh,2,2), " kmh <br>",
                                  round0(df$run_dist_cum,2,2), " km <br>",
                     df$time, "<br>", round(df$lat,6), ", ", round(df$lon,6))

map <- leaflet(df) %>% addTiles() %>%
       addPolylines(~lon, ~lat, color="white", weight=15, opacity=1) %>%
       addCircleMarkers(~lon, ~lat, popup=~display, stroke=F, color=~col,
                        fillOpacity=1, radius=6) %>%
       #addPolylines(~lon, ~lat) %>%
       addLabelOnlyMarkers(lng=df$lon[kmmarker], lat=df$lat[kmmarker],
                           label=paste(kms,"km"), labelOptions=
                           labelOptions(noHide=T,textsize="14px",textOnly=T))
       # addCircles(~lon, ~lat, stroke=F, color=~col)
#df_sf <- sf::st_as_sf(df, coords=c("lon","lat") )
#mapview::mapview(df_sf, zcol="speed_smooth_kmh", color=NA)

if(!is.null(wp)) # add waypoints
  {
  wp$display <- paste0("waypoint:<br>", wp$link, "<br>",
                       round(wp$lat,6), "\t", round(wp$lon,6),  "<br>",
                       round(as.numeric(wp$ele),1), "m")
  map <- map %>% addCircleMarkers(wp$lon, wp$lat, popup=wp$display, color="black")
  }

print(map)
} else map=NA

# Static plots ----

if(plot_static)
{
# Speed over Distance and Time:
op <- par(mfrow=mfrow, mgp=c(1.9, 0.7, 0), mar=c(3,4,2,0.5), las=1)
plot(df$run_dist_cum, df$speed_kmh, type="l", ylim=lim0(df$speed_smooth_kmh),
     xlab="Distance [km]", ylab="Speed  [km/h]", col="orange",
     main="Speed over distance")
lines(df$run_dist_cum, df$speed_smooth_kmh, lwd=3, col="blue")

plot(df$time, df$speed_kmh, type="l", ylim=lim0(df$speed_smooth_kmh),
     xlab="Time  [UTC (=GMT)]", ylab="Speed  [km/h]", col="blue",
     main="Speed over time")

# Histogram of speed:
breaks <- seq(0, max(df$speed_kmh)+1, 0.5)
hh <- hist(df$speed_smooth_kmh, breaks=breaks, plot=FALSE)
hist(df$speed_kmh, breaks=breaks, col=addAlpha("orange"),
     main=paste("Speed histogram, avg =",avg,"kmh"),
     ylim=lim0(hh$counts), xlab="", ylab="")
hist(df$speed_smooth_kmh, breaks=breaks, col=addAlpha("blue"), add=TRUE)

# Static map:
bgmap <- OSMscale::pointsMap("lat","lon", data=df, pch=NA, map=bgmap, ...)
dfp <- OSMscale::projectPoints("lat","lon", data=df,
                               from=OSMscale::pll(),
                               to=OSMscale::pmap(bgmap)  )
colPoints(dfp$x,dfp$y, df$speed_smooth_kmh, add=TRUE, y1=0.9, y2=1,
          legargs=list(mar=0, labelpos=5), density=FALSE, zlab="")
textField(dfp$x[kmmarker], dfp$y[kmmarker], paste(kms,"km"))

if(!keeppar) par(op)
} else bgmap=NA

# Output ----
return(invisible(list(map=map, df=df, average_speed_kmh=avg, bgmap=bgmap)))
}
