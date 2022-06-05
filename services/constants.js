export const FLAGS = {
  CHAR_NOT_FOUND: "CHAR_NOT_FOUND",
  OUTSIDE_LOCATION: "OUTSIDE_LOCATION",
  SIMILAR_WORD: "SIMILAR_WORD",
};

export const FLAG_VALUES = {
  CHAR_NOT_FOUND: () => -0.05,
  OUTSIDE_LOCATION: () => -0.5,
  SIMILAR_WORD: (term, word) => Math.max(0, word.length - term.length) * -0.03,
};

export const distanceRangeKM = 200;

export const SCORE_WEIGHT_PERCENTAGE = {
  ACCURACY: 40,
  LOCATION_PROXIMITY: 50,
  FREQUENTLY_SEARCHED: 10,
};
