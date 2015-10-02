function toRadians(deg) {
  return deg * Math.PI / 180;
}

module.exports = function(lat1, lat2, long1, long2) {
  lat1 = toRadians(lat1);
  lat2 = toRadians(lat2);
  long1 = toRadians(long1);
  long2 = toRadians(long2);

  return  1.0 - (Math.asin(Math.sqrt(Math.pow(Math.sin((lat2 - lat1) / 2.0), 2.0) + 
              Math.cos(lat1) * 
                Math.cos(lat2) * 
                Math.pow(Math.sin((long2 - long1) / 2.0), 2.0))) / 
          (Math.PI / 2.0));
}