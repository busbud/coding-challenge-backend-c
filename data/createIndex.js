var conn = new Mongo();
var db = conn.getDB("location-db");
db.locations.createIndex({name : "text"});

//merging lat and longitude into 1
db.locations.find().snapshot().forEach(function(myDoc){ 
    myDoc.coords = [ myDoc.longitude, myDoc.lat]; 
    delete myDoc.lat; 
    delete myDoc.longitude; 
    db.locations.save(myDoc)});
