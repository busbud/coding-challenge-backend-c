var conn = new Mongo();
var db = conn.getDB("location-dbxxx");

//create index here for full text search on the City names
db.locations.createIndex({ascii : "text"});

//merging lat and longitude into 1
db.locations.find().snapshot().forEach(function(myDoc){ 
    myDoc.loc = { type:"Point", "coordinates": [myDoc.longitude, myDoc.lat]}; 
    delete myDoc.lat; 
    delete myDoc.longitude;
    //just update here instead of issuing another $update command to mongo
    if(myDoc.admin1 == 1)
	myDoc.admin1 = "AB";
    if(myDoc.admin1 == 2)
	myDoc.admin1 = "BC";
    if(myDoc.admin1 == 3)
	myDoc.admin1 = "MB";
    if(myDoc.admin1 == 4)
	myDoc.admin1 = "NB";
    if(myDoc.admin1 == 5)
	myDoc.admin1 = "NL";
    if(myDoc.admin1 == 7)
	myDoc.admin1 = "NS";
    if(myDoc.admin1 == 8)
	myDoc.admin1 = "ON";
    if(myDoc.admin1 == 9)
	myDoc.admin1 = "PE";
    if(myDoc.admin1 == 10)
	myDoc.admin1 = "QC";
    if(myDoc.admin1 == 11)
	myDoc.admin1 = "SK";
    if(myDoc.admin1 == 12)
	myDoc.admin1 = "YT";
    if(myDoc.admin1 == 13)
	myDoc.admin1 = "NT";
    db.locations.save(myDoc);
});
//remove the document which has the fields as values
db.locations.remove({name : "name"});
//create the 2dsphere index
db.locations.createIndex( {loc: "2dsphere" });

