

// simple algorithm to calculate the score based on the Levenshtein distance between the search term and the city name.
// Function to calculate distance between two points given their latitude and longitude
export const getDistanceFromLatLonInKm = (lat1: number, lon1: number, lat2: number, lon2: number): number  => {
    const R = 6371; // Radius of the earth in km
    const dLat = deg2rad(lat2 - lat1);
    const dLon = deg2rad(lon2 - lon1);
    const a =
        Math.sin(dLat / 2) * Math.sin(dLat / 2) +
        Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) *
        Math.sin(dLon / 2) * Math.sin(dLon / 2);
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    const d = R * c; // Distance in km
    return d;
}

// Function to convert degrees to radians
export function deg2rad(deg: number): number {
    return deg * (Math.PI / 180);
}
