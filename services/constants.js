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
