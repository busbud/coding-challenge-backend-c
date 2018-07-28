// one-time script to initialize mongodb

// first, load the data using these commands :

// sed 's/\"//g' data/cities_canada-usa.tsv > data/cities_canada-usa.clean.tsv
// mongoimport --db busbud --collection cities --type tsv --headerline --file data/cities_canada-usa.clean.tsv
// mongoimport --db busbud --collection provinces --type tsv --fields code,name --file admin1CodesASCII.txt

// then execute this script to create indexes

const MongoClient = require('mongodb').MongoClient;
const mongoURI = process.env.MONGO_URI || 'mongodb://localhost/busbud';

createIndexes = async () => {
  const client = await MongoClient.connect(mongoURI);

  const db = client.db();

  await db
    .collection('cities')
    .aggregate([
      {
        $addFields: {
          geometry: { type: 'Point', coordinates: ['$long', '$lat'] }
        }
      },
      { $out: 'cities' }
    ])
    .toArray();

  await db.collection('cities').createIndex({ geometry: '2dsphere' });
  await db.collection('cities').createIndex({ ascii: 1 });
  await db.collection('provinces').createIndex({ ascii: 1 });

  // we keep the existing lat and long in case something else needs them there

  console.log('done');
};

createIndexes().then(() => {
  process.exit(1);
});
