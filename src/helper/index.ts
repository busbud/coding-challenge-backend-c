import _ from "lodash";
import { GetSuggestionAccuracyByLatAndLongParam } from "./../services/cities.definition";

export const calculateLocationAccuracyScoreByCoordinates = (
  params: GetSuggestionAccuracyByLatAndLongParam
): number => {
  const { suggestedLocationCoordinates, latitude, longitude } = params;
  const lat = Math.abs(suggestedLocationCoordinates?.latitude - latitude);
  const long = Math.abs(suggestedLocationCoordinates?.longitude - longitude);

  let score = 10 - (lat + long) / 2;
  score = score > 0 ? Math.round(score) / 10 : 0;
  return score;
};

export const findSpecificAddressInformation = (
  addressInformation: { long_name: string; short_name: string; types: string[] }[],
  name: string
) => {
  const address = _.find(addressInformation, (obj) => {
    return obj.types[0] == name && obj.types[1] == "political";
  });
  return address ? address.short_name : null;
};
