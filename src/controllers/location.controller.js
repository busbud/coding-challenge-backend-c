const { getDistance, convertDistance } = require('geolib');
const url = require('url');
const Location = require('../models/location.model');

exports.suggestions = async (req, res) => {
  const query = url.parse(req.url, true).query;

  const suggestions = await Location.find({
    $or: [
      {
        name: {
          $regex: `^.*${query.q}.*`,
          $options: 'i',
        },
      },
      {
        ascii: {
          $regex: `^.*${query.q}.*`,
          $options: 'i',
        },
      },
      {
        alt_name: {
          $regex: `^.*${query.q}.*`,
          $options: 'i',
        },
      },
      // could add time zones
    ],
  });

  if (suggestions.length > 0) {
    return generate_score(
      {
        name: query.q,
        latitude: query.latitude,
        longitude: query.longitude,
      },
      suggestions
    );
  } else {
    return [];
  }
};

const generate_score = (source, suggestions) => {
  const source_word_score = score_words(source.name.toLowerCase());
  let min_distance;
  let max_distance;
  if (source.latitude && source.longitude) {
    min_distance = convertDistance(
      getDistance(source, {
        latitude: suggestions[0].lat,
        longitude: suggestions[0].long,
      }),
      'mi'
    );
    max_distance = convertDistance(
      getDistance(source, {
        latitude: suggestions[0].lat,
        longitude: suggestions[0].long,
      }),
      'mi'
    );
  }

  let min_pop = suggestions[0].population;
  let max_pop = suggestions[0].population;

  let min_word_score = Math.abs(
    source_word_score - score_words(suggestions[0].name.toLowerCase())
  );
  let max_word_score = Math.abs(
    source_word_score - score_words(suggestions[0].name.toLowerCase())
  );

  const result = [];

  for (const suggestion of suggestions) {
    if (source.latitude && source.longitude) {
      const distance = convertDistance(
        getDistance(source, {
          latitude: suggestion.lat,
          longitude: suggestion.long,
        }),
        'mi'
      );
      if (distance < min_distance) {
        min_distance = distance;
      }
      if (distance > max_distance) {
        max_distance = distance;
      }
      suggestion.distance = distance;
    }

    suggestion.word_score = Math.abs(
      source_word_score - score_words(suggestion.name.toLowerCase())
    );

    if (suggestion.population < min_pop) {
      min_pop = suggestion.population;
    }
    if (suggestion.population > max_pop) {
      max_pop = suggestion.population;
    }
    if (suggestion.word_score < min_word_score) {
      min_word_score = suggestion.word_score;
    }
    if (suggestion.word_score > max_word_score) {
      max_word_score = suggestion.word_score;
    }
  }

  for (const suggestion of suggestions) {
    let dist_prob = -1;
    const pop_prob = (suggestion.population - min_pop) / (max_pop - min_pop);

    const word_score =
      (max_word_score - suggestion.word_score) /
      (max_word_score - min_word_score);
    let score = pop_prob + word_score;

    if (source.latitude && source.longitude) {
      dist_prob =
        (max_distance - suggestion.distance) / (max_distance - min_distance);
      score = (pop_prob + dist_prob + word_score) / 3;
    } else {
      score = score / 2;
    }

    const country = suggestion.country === 'CA' ? 'Canada' : 'USA';
    result.push({
      name: `${suggestion.ascii}, ${suggestion.admin1}, ${country}`,
      latitude: suggestion.lat,
      longitude: suggestion.long,
      score: Math.round((score + Number.EPSILON) * 100) / 100,
    });
  }
  const final_suggestion = result.sort((a, b) => {
    return b.score - a.score;
  });
  return final_suggestion;
};

const score_words = (word) => {
  let score = 0;
  for (let i = 0; i < word.length; i++) {
    score += word.charCodeAt(i);
  }
  return score;
};
