export enum CitiesSeederEvents {
  NEW_CITY = 'cities.seeder.new',
  SEEDING_FINISHED = 'cities.seeder.finished',
}

export enum CitiesRepositoryEvents {
  SEEDING_REQUESTED = 'cities.repository.seeding-requested',
  CITIES_READY = 'cities.repository.ready',
}

export enum SuggestionsEvents {
  SUGGESTION_GENERATED = 'suggestions.generated',
  SUGGESTION_RETURNED = 'suggestions.returned',
}
