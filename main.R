
pi = 3.1415926535897932384626 # 圆周率
a = 6378245.0  # 长半轴
ee = 0.00669342162296594323  # 扁率


transformlat<-function(lng,lat){
  ret =(-100.0) + 2.0 * lng + 3.0 * lat + 0.2 * lat * lat + 0.1 * lng * lat + 0.2 * sqrt(abs(lng))
  ret =ret+ (20.0 * sin(6.0 * lng * pi) + 20.0 *sin(2.0 * lng * pi)) * 2.0 / 3.0
  ret =ret+ (20.0 * sin(lat * pi) + 40.0 *sin(lat / 3.0 * pi)) * 2.0 / 3.0
  ret =ret+ (160.0 * sin(lat / 12.0 * pi) + 320 *sin(lat * pi / 30.0)) * 2.0 / 3.0
  return(ret)
}

transformlng<-function(lng,lat){
  ret = 300.0 + lng + 2.0 * lat + 0.1 * lng * lng + 0.1 * lng * lat + 0.1 * sqrt(abs(lng))
  ret =ret+ (20.0 * sin(6.0 * lng * pi) + 20.0 *sin(2.0 * lng * pi)) * 2.0 / 3.0
  ret =ret+ (20.0 * sin(lng * pi) + 40.0 *sin(lng / 3.0 * pi)) * 2.0 / 3.0
  ret =ret+ (150.0 * sin(lng / 12.0 * pi) + 300.0 *sin(lng / 30.0 * pi)) * 2.0 / 3.0
  return(ret)
}






WGS84toGCJ02<-function(lng,lat){
  #WGS84转GCJ02(火星坐标系)
  #:param lng:WGS84坐标系的经度
  #:param lat:WGS84坐标系的纬度
  
  dlat = transformlat(lng - 105.0, lat - 35.0)
  dlng = transformlng(lng - 105.0, lat - 35.0)
  radlat = lat / 180.0 * pi
  magic = sin(radlat)
  magic = 1 - ee * magic * magic
  sqrtmagic = sqrt(magic)
  dlat = (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * pi)
  dlng = (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * pi)
  mglat = lat + dlat
  mglng = lng + dlng
  return([mglng,mglat])
}

GCJ02toWGS84<-function(lng,lat){
  #GCJ02(火星坐标系)转GPS84
  #:param lng:火星坐标系的经度
  #:param lat:火星坐标系纬度
  dlat = transformlat(lng - 105.0, lat - 35.0)
  dlng = transformlng(lng - 105.0, lat - 35.0)
  radlat = lat / 180.0 * pi
  magic = sin(radlat)
  magic = 1 - ee * magic * magic
  sqrtmagic = sqrt(magic)
  dlat = (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * pi)
  dlng = (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * pi)
  mglat = lat + dlat
  mglng = lng + dlng
  return ([(lng * 2 - mglng),(lat * 2 - mglat)]
}


#test:
lng = 128.543
lat = 37.065

print(WGS84toGCJ02(lng,lat))
print(GCJ02toWGS84(lng,lat))