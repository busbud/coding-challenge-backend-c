var conn = new Mongo();
var db = conn.getDB("location-db");

//create index here for full text search on the City names
db.locations.createIndex({name : "text"});

//merging lat and longitude into 1
db.locations.find().snapshot().forEach(function(myDoc){ 
    myDoc.coords = [ type:"Point", "coordinates": [myDoc.longitude, myDoc.lat]; 
    delete myDoc.lat; 
    delete myDoc.longitude; 
    db.locations.save(myDoc)});

db.locations.createIndex( { "loc": "2dsphere" } )
