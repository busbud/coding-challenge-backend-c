import _ from "lodash";
import es6promise from "es6-promise";
import natural from "natural";
es6promise.polyfill();

/**
 * Responsible for interacting with MongoDB: the repository is only responsible for
 * fetching locations from the database.
 */
export default class LocationRepo {

  constructor(db) {
    this.db = db;
  }

  listLocations(startsWith, near) {
    const cities = this.db.collection("cities");

    try {
      if (near == null) {
        return cities.find(
          {"fullName": new RegExp(`^${startsWith}`)}
        ).limit(3000).toArray();
      }
      else {
        return cities.find({
          "fullName": new RegExp(`^${startsWith}`),
          "location": {
            "$near": {
              "$geometry": {
                type: "Point",
                  coordinates: [parseFloat(near.longitude), parseFloat(near.latitude)]
              }
            }
          }
        }).limit(3000).toArray();
      }
    } catch(err) {
      console.log(err);
    }

  }

}
