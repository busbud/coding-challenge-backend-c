// Node.js Embedded Libraries
const fs = require('fs');
const readline = require('readline');

// NPM Installed Libraries
var Math = require('mathjs');

// Constants
const EARTH_RADIUS = 6371; // KM
	
// Module
module.exports = new (function() {
	//----- Private Section -----//
	// Variables
	var isDataReady = false;
	var allDataArrays = []; // Main array of arrays of all data
	
	// Functions
	async function readAndProcessDataFromFile(){ // Should be called only ONCE!
		try {
			// Reading data from file
			const fileStream = fs.createReadStream('data/cities_canada-usa.tsv');
			const rl = readline.createInterface({
				input: fileStream
			});
			
			let isFirstLinePassed = false; // To skip the first line (Keys/Columns row in file)
			
			for await (const line of rl) {
				let data = line.split('\t');
				
				if(isFirstLinePassed){ // Will be false once, then true all the time till finish reading the whole file, unless somehow error happen.
					if((data[8] != 'US' && data[8] != 'CA') || data[14] <= 5000){ // suggestions are restricted to cities in the USA and Canada with a population above 5000 people.
						continue;
					}
					
					let recordArray = [];
					
					// Using only needed columns for the task
					// Frequently used values are in the beginning of the array
					recordArray[0] = data[1].toLowerCase(); // index [0] // name = toLowered case, used to improve search algorithm (case insensitive search)

					recordArray[1] = data[4]; // index [1] // lat
					recordArray[2] = data[5]; // index [2] // lon
					
					recordArray[3] = data[1]; // index [3] // name // This column is stored for the final result (showing result with normal case)
					
					recordArray[4] = data[10]; // index [4] // state
					
					if(data[8] == 'US')recordArray[5] = 'USA'; // index [5] // country
					else if(data[8] == 'CA')recordArray[5] = 'Canada';
					else recordArray[5] = data[8];
					
					allDataArrays.push(recordArray);
				}
				else { // First loop, for the keys of data
					isFirstLinePassed = true;
				}
				
				//console.log(`This line is: ${line}`); // debug
			}
			
			// Sorting data (Sorting algorithm depends on Node.js Engine and Array.length) // We do this ONCE!, when we run the app, so too much traffic won't affect performance
			await allDataArrays.sort((a, b) => {
				return (a[0] > b[0]) ? 1 : -1;
			});
			
			isDataReady = true;
		}
		catch(e){
			console.log("Error happened: ", e);
			console.log("Data not ready!");
			isDataReady = false;
		}
	}
	
	function toRadius(degree) {
		return degree * (Math.pi / 180);
	}

	function getDistanceBetweenTwoCoordinates(lat1, long1, lat2, long2) {
		let latDiff = toRadius(lat2 - lat1);
		let longDiff = toRadius(long2 - long1); 
		
		let formula = 
			Math.sin(latDiff/2) * Math.sin(latDiff/2) +
			Math.cos(toRadius(lat1)) * Math.cos(toRadius(lat2)) * 
			Math.sin(longDiff/2) * Math.sin(longDiff/2)
		; 
		let calc = 2 * Math.atan2(Math.sqrt(formula), Math.sqrt(1 - formula)); 
		let dist = EARTH_RADIUS * calc;
		return dist; // Distance in km
	}
	
	// Private Function Call (Private Main)
	readAndProcessDataFromFile();
	
	//----- Public Section -----//
	// Functions
	this.searchInArray = function(searchFor, lat, long){ // Modified Binary Search Algorithm // log(n) + c1 + c2
		if(!isDataReady)return;
		
		// Making sure 'q' parameter is given and at least 3 chars provided (for performance reasons)
		if(!searchFor || (searchFor && searchFor.length < 3)){
			//console.log("rejected");
			return [];
		}
		
		searchFor = searchFor.toLowerCase(); // (case insensitive search)
		
		// Binary Search Algorithm // Rules
		var startIndex  = 0,
			stopIndex   = allDataArrays.length - 1,
			middle      = Math.floor((stopIndex + startIndex)/2);
			
		while(allDataArrays[middle][0].substr(0, searchFor.length) != searchFor && startIndex < stopIndex){ // log(n)
			//adjust search area
			if (searchFor < allDataArrays[middle][0]){
				stopIndex = middle - 1;
			}
			else if (searchFor > allDataArrays[middle][0]){
				startIndex = middle + 1;
			}

			//recalculate middle
			middle = Math.floor((stopIndex + startIndex) / 2);
		}
		
		// Just to make sure search value has been found and correct
		if(allDataArrays[middle][0].substr(0, searchFor.length) != searchFor){
			return []; // return empty array if not found
		}
		
		var searchResults = [];
		
		// Checking for multiple same/similar results (before and after middle)
		// Left first - going backwards (before middle) // middle included
		for(let i = middle; i >= 0; i--){ // c1
			if(allDataArrays[i][0].substr(0, searchFor.length) == searchFor){
				if(allDataArrays[i][0] == searchFor){
					searchResults.unshift([allDataArrays[i], 0, 0]); // add to the beginning of result array, same exact value (highest probability)
				}
				else {
					searchResults.push([allDataArrays[i], 0, 0]);
				}
			}
			else {
				break; // no more results
			}
		}
		
		// Then right - going forward (after middle) // middle not included
		for(let i = middle + 1; i < allDataArrays.length; i++){ // c2
			if(allDataArrays[i][0].substr(0, searchFor.length) == searchFor){
				if(allDataArrays[i][0] == searchFor){
					searchResults.unshift([allDataArrays[i], 0, 0]); // add to the beginning of result array, same exact value (highest probability)
				}
				else {
					searchResults.push([allDataArrays[i], 0, 0]);
				}
			}
			else {
				break; // no more results
			}
		}
		
		//											0			1		2
		// Note: Search Results' Structure is: [[RecordArray], Score, Distance]
		// (pass by reference + new data (by value))
		// searchResults[i][0][0] name (tolowered case)
		// searchResults[i][0][1] lat
		// searchResults[i][0][2] long
		// searchResults[i][0][3] name (normal case)
		// searchResults[i][0][4] country
		// searchResults[i][0][4] state
		// searchResults[i][1] score
		// searchResults[i][2] dist
		
		if(isNaN(lat) || isNaN(long)){ // if no latitude and longitude provided in the query 
			// Scoring
			// Giving scores according to number of search results and char length difference
			if(searchResults.length == 1){
				if(searchResults[0][0][0] == searchFor){
					searchResults[0][1] = 1; // score 1 because 99.9% same written input (99.9% because case insensitive)
				}
				else {
					searchResults[0][1] = 0.9; // because it is only one
				}
			}
			else if(searchResults.length == 2){
				if(searchResults[0][0][0] == searchResults[1][0][0] && searchResults[0][0][0] != searchFor){
					searchResults[0][1] = 0.9;
					searchResults[1][1] = 0.9;
				}
				else {
					if(searchResults[0][0][0] == searchFor){
						searchResults[0][1] = 1;
					}
					else {
						searchResults[0][1] = 0.9;
					}
					
					if(searchResults[1][0][0] == searchFor){
						searchResults[0][1] = 1;
					}
					else {
						searchResults[1][1] = 0.7;
					}
				}
			}
			else {
				for(let i = 0; i < searchResults.length; i++){
					searchResults[i][1] = 0.0; // index [1] // scores // init value
					
					if(searchResults[i][0][0] == searchFor){
						searchResults[i][1] = 1; // score 1 because 99.9% same written input (99.9% because case insensitive)
						continue;
					}
					
					// Char length difference
					let lenDif = searchResults[i][0][0].length - searchFor.length;
					
					if(lenDif == 1){
						searchResults[i][1] = 0.9;
					}
					else if(lenDif == 2){
						searchResults[i][1] = 0.7;
					}
					else if(lenDif == 3){
						searchResults[i][1] = 0.5;
					}
					else if(lenDif == 4){
						searchResults[i][1] = 0.4;
					}
					else if(lenDif > 4 && lenDif <= 7){
						searchResults[i][1] = 0.3;
					}
					else if(lenDif > 7){
						searchResults[i][1] = 0.1;
					}
				}
			}
		}
		else { // else if latitude and longitude provided in the query 
			for(let i = 0; i < searchResults.length; i++){ // calculate distance between each found result and the given coordinate
				searchResults[i][1] = 0.0; // index [1] // scores // init value
				searchResults[i][2] = getDistanceBetweenTwoCoordinates(lat, long, searchResults[i][0][1], searchResults[i][0][2]); // index [2] // dist
			}
			
			// Sorting small data, performance not effected
		    // Sorting according to: similar ones with less chars are first with distance ASC
			searchResults.sort((a, b) => {
				if(a[0][0] == b[0][0]) {
					return a[2] - b[2];
				}
				return ((a[0][0].length > b[0][0].length) ? 1 : -1);
			});
			
			// Scoring
			// Giving scores according to number of search results and char length difference and distance difference
			if(searchResults.length == 1){
				if(searchResults[0][0][0] == searchFor){
					searchResults[0][1] = 1; // score 1 because 99.9% same written input (99.9% because case insensitive)
				}
				else {
					searchResults[0][1] = 0.9;
				}
			}
			else if(searchResults.length == 2){
				if(searchResults[0][0][0] == searchResults[1][0][0] && searchResults[0][0][0] != searchFor){
					searchResults[0][1] = 0.9;
					searchResults[1][1] = 0.7;
				}
				else {
					if(searchResults[0][0][0] == searchFor){
						searchResults[0][1] = 1;
					}
					else {
						searchResults[0][1] = 0.9;
					}
					
					if(searchResults[1][0][0] == searchFor){
						searchResults[0][1] = 0.9;
					}
					else {
						searchResults[1][1] = 0.7;
					}
				}
			}
			else {
				for(let i = 0; i < searchResults.length; i++){
					// Giving scores according to char length difference and distance difference
					if(searchResults[i][0][0] == searchFor && i == 0){ // if same input found and nearest distance then this would be a 99.9% == 1 // (99.9% because case insensitive)
						searchResults[i][1] = 1;
						continue;
					}
					else if(i == 0){ // i == 0 means first value which is nearest because data sorted according to nearest distance and name
						searchResults[i][1] = 0.9;
						continue;
					}
					
					// Giving score according to char length difference
					let lenDif = searchResults[i][0][0].length - searchFor.length;
					
					if(lenDif == 0 || lenDif == 1){
						searchResults[i][1] = 0.3;
					}
					else if(lenDif == 2){
						searchResults[i][1] = 0.2;
					}
					else if(lenDif > 2 && lenDif <= 7){
						searchResults[i][1] = 0.1;
					}
					
					// Adding score according to distance difference
					if(searchResults[i][2] <= 200){
						searchResults[i][1] = searchResults[i][1] + 0.5;
					}
					else if(searchResults[i][2] <= 350 && searchResults[i][2] > 200){
						searchResults[i][1] = searchResults[i][1] + 0.4;
					}
					else if(searchResults[i][2] <= 1000 && searchResults[i][2] > 350){
						searchResults[i][1] = searchResults[i][1] + 0.2;
					}
					else {
						searchResults[i][1] = searchResults[i][1] + 0.1;
					}
					
				}
			}
		}
		
		// Sorting small data, performance not effected
		// Sorting according to Scores
		searchResults.sort((a, b) => { // more scored ones are first as well as similar ones with less chars				
			if(a[0][0] == b[0][0]) {
				return b[1] - a[1];
			}
			return ((a[0][0].length > b[0][0].length) ? 1 : -1);
		});
		
		let finalSearchResults = [];
		for(let i = 0; i < searchResults.length; i++){
			finalSearchResults.push({
				name: searchResults[i][0][3] + ", " + searchResults[i][0][4] + ", " + searchResults[i][0][5],
				latitude: searchResults[i][0][1],
				longitude: searchResults[i][0][2],
				score: parseFloat(searchResults[i][1].toFixed(1)), // Had to use (.toFixed) because of javascript's international bug (0.1 + 0.2) = 0.3000000000004 // fixed with using .toFixed(1) and parseFloat
				//dist: searchResults[i][2] // debug
			})
		}
		
		searchResults = [];

		return finalSearchResults;
	}
})();




// Challenge solved by Tarik Seyceri - tarik@seyceri.info - Istanbul, Turkey