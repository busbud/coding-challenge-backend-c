const { PrismaClient } = require('@prisma/client')

const prisma = new PrismaClient()

var fs = require('fs');
var path = require('path');

//get file directory paths
var citiesJson = path.join(__dirname, '..', 'data', 'cities_canada-usa.tsv');
var citiesTSV = fs.readFileSync(citiesJson, 'utf8');

var admin1JSON = path.join(__dirname, '..', 'data', 'canada_usa.tsv')
var admin1TXT = fs.readFileSync(admin1JSON, 'utf8');

//function to convert tsv to json
function tsvJSON(tsv) {
    const lines = tsv.split('\n');
    const headers = lines.shift().split('\t');
    return lines.map(line => {
      const data = line.split('\t');
      return headers.reduce((obj, nextKey, index) => {
        obj[nextKey] = data[index];
        return obj;
      }, {});
    });
  }

  //seed function
  const main = async () => {
    console.log(`Start seeding ...`) 
   const seedData =  await tsvJSON(citiesTSV)
   const cadUsa = await tsvJSON(admin1TXT)
   await seedData.forEach(tsvData => {
     cadUsa.forEach( async adminCode => {
       //set conditionals to store dynamic state and country names
       if(tsvData.country !== undefined && tsvData.admin1 !== undefined){
       const adminCodeId = tsvData.country.concat('.', tsvData.admin1)

       let stateName = ''
       let countryName = ''

       if(adminCodeId === adminCode.id){
         stateName = adminCode.name ? adminCode.name : adminCode.alt_name
       }

       if(tsvData.country === 'CA'){
         countryName = 'Canada'
       }

       if(tsvData.country === 'US'){
         countryName = 'United States of America'
       }
       
       if(stateName !== '' && countryName !== ''){
      //seed cities table 
       await prisma.cities.create({
        data: {
          id: Number(tsvData.id),
          name: tsvData.name,
          ascii_name: tsvData.ascii,
          alternate_name: tsvData.alt_name,
          latitude: parseFloat(tsvData.lat),
          longitude: parseFloat(tsvData.long),
          state: stateName,
          country: countryName,
          population: Number(tsvData.population),
        }
       })
      }
    }
     })
   });
  }

  main() 
  .catch((e) => console.error(e))
  .finally(async () => {
    await prisma.$disconnect()
  })