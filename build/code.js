"use strict";
//--->>>>>> Using For Loop
// for (let i = 0; i < cities.length; i++) {
//     const city = cities[i];
//     const city_population = city.population as string;
//     if (city_population !== undefined) {
//         const population = parseInt(city_population.replace(/,/g, ''));
//         // Filter cities with a population above 5000 people in the USA and Canada
//         if (population >= 5000 && (city.country === 'US' || city.country === 'CA')) {
//             const cityName = city.name as string;
//             const state = city.admin1;
//             const country = city.country;
//             // Calculate score based on search term and optionally caller's location
//             const score = calculateScore(q, qLatitude, qLongitude, cityName, city);
//             // Create suggestion object
//             const suggestion: Suggestion = {
//                 name: `${cityName}, ${state}, ${country}`,
//                 latitude: city.lat as string,
//                 longitude: city.long as string,
//                 score,
//             };
//             suggestions.push(suggestion);
//         }
//     }
// }
//# sourceMappingURL=code.js.map