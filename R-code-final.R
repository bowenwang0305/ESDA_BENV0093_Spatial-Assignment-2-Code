#ssrd
library(ncdf4) #library to read and process netcdf data
library(lattice)
library(RColorBrewer)
library(sf)
library(tmap)
era <- nc_open(r"(C:\Users\DELL\Desktop\Spatial Assignment 2\2nd_Assignment-20231228\数据准备\Q2数据整理\Solar surface radiance\Week_6_data\era5.nc)" )
lon <- ncvar_get(era, "longitude")
lat <- ncvar_get(era, "latitude")
time <- ncvar_get(era, "time")
ssrd_array <- ncvar_get(era,"ssrd") 
dim(ssrd_array)
dlname <- ncatt_get(era,"ssrd","long_name")
dunits <- ncatt_get(era,"ssrd","units")
fillvalue <- ncatt_get(era,"ssrd","_FillValue")
ssrd_slice <- ssrd_array[,,2] 
length(na.omit(as.vector(ssrd_slice))) /length(as.vector(ssrd_slice))
image(ssrd_slice, col=rev(brewer.pal(10,"RdBu")) )
max_rad <- max(ssrd_slice, na.rm=TRUE)
max_rad
lonlat <- as.matrix( (expand.grid(lon, lat))) 
ssrd_vec <- as.vector( ssrd_slice) 
ssrd_df <- data.frame( cbind( lonlat,ssrd_vec  ))
colnames(ssrd_df) <- c("lon", "lat", "ssrd")
ssrd_df_value <- na.omit (ssrd_df)
head(ssrd_df_value, 3) 
ssrd_sf<- st_as_sf( ssrd_df_value, coords = c(  "lon", "lat")  )
st_crs(ssrd_sf) <- 4326 
ssrd_sf <- st_transform(ssrd_sf, 4326 )
tmap_mode("view")
tm_shape(ssrd_sf)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "viridis")


#IDW
ssrd_sf = st_transform(ssrd_sf, 4326)
indonesia = st_transform(indonesia, st_crs(ssrd_sf))

coor = as.data.frame(st_coordinates(ssrd_sf))
ssrd_sf$x = coor$X
ssrd_sf$y = coor$Y
ssrd_nogeom = st_drop_geometry(ssrd_sf) 
ssrd_nogeom=na.omit(ssrd_nogeom)
gs <- gstat(formula=ssrd~1, locations=~x+y, data=ssrd_nogeom, nmax=Inf, set=list(idp=2)) 
indonesia = st_read(r"(C:\Users\DELL\Desktop\Spatial Assignment 2\2nd_Assignment-20231228\Week_6_Lecture_Spatial interpolation-20240103\Week_6_Practical_Interpolation wind power data in Indonesia-20240103\Week_6_data\idn_admbnda_adm0_bps_20200401.shp)")
st_bbox(indonesia)
raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)
idw <- interpolate(raster_template, gs, debug.level=0) 
plot(idw$var1.pred)
idw_mask <- mask(idw, indonesia)
plot(idw_mask$var1.pred)
names(idw_mask) = c( "predicted","observed" )
tmap_mode("view")
tm_shape(idw_mask$predicted) + 
  tm_raster(col="predicted", style = "quantile", n = 10, palette= "Blues", legend.show = TRUE)
writeRaster(idw_mask$predicted, filename = "ssrd_predicted.tif")

st_write(ssrd_sf, "ssrd_shapefile.shp")

#To power
radiation_to_power <- function(G, A=1, r=0.175, p=0.6, hours=1){
  kWh <- G * A * r * p * (hours/3600) / 1000
  return(kWh)
}

idw_values <- values(idw_mask$predicted)
idw_kwh_values <- radiation_to_power(idw_values)

values(idw_mask$predicted) <- idw_kwh_values

tmap_mode("view")
tm_shape(idw_mask$predicted) + 
  tm_raster(col="predicted", style = "quantile", n = 10, palette= "YlOrRd", legend.show = TRUE)

writeRaster(idw_mask$predicted, filename = "ssrd_predicted_kWh_final.tif")

## Financial feasibility analysis

# NPV
calc_NPV <- function(annual_revenue, discount_rate, lifetime_yrs, CAPEX, OPEX=0) {
  costs_op <- rep(OPEX, lifetime_yrs)
  revenue <- rep(annual_revenue, lifetime_yrs)
  t <- seq(1, lifetime_yrs, 1)
  NPV <- sum((revenue - costs_op) / (1 + discount_rate)^t) - CAPEX
  return(round(NPV, 2))
}

# LCOE
Life_span_generation_kWH <- function(yearly_generation_kWH, discount=0.08, lifetime_yrs) {
  t <- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH / (1 + discount)^t)
  return(round(L_S_G, 2))
}


calc_LCOE <- function(NPV_cost, Life_span_generation) {
  lcoe <- NPV_cost / Life_span_generation
  return(round(lcoe, 2))
}

# parameter assumptiom
area_km2 = 12
sun_hours_per_day = 8
yield_per_m2 = 0.25
days_per_year = 365
Pr = 0.6
CAPEX_per_MW = 1.16e6
life_span_years = 25
discount_rate_NPV = 0.05
discount_rate_LCOE = 0.08
revenue_per_MWh = 103

peak_generation_MWh = area_km2 * 1e6 * yield_per_m2 / 1000
installed_capacity_MW = peak_generation_MWh

CAPEX = installed_capacity_MW * CAPEX_per_MW

annual_generation_MWh = installed_capacity_MW * sun_hours_per_day * days_per_year

annual_revenue = annual_generation_MWh * revenue_per_MWh

npv = calc_NPV(annual_revenue, discount_rate_NPV, life_span_years, CAPEX)

life_span_generation = Life_span_generation_kWH(annual_generation_MWh, discount_rate_LCOE, life_span_years)

lcoe = calc_LCOE(CAPEX, life_span_generation)

print(paste("Installed capacity per station (MW):", installed_capacity_MW))
print(paste("CAPEX per station (USD):", CAPEX))
print(paste("Annual generation per station (MWh):", annual_generation_MWh))
print(paste("Annual revenue per station (USD):", annual_revenue))
print(paste("NPV per station (USD):", npv))
print(paste("LCOE per station (USD/MWh):", lcoe))
