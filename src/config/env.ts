import "dotenv/config.js";

export default {
  port: Number(process.env.PORT) || 2345,
  googleMap: {
    baseUrl: "https://maps.googleapis.com/maps/api/geocode/json",
    googleApiKey: process.env.API_KEY,
  },
};
