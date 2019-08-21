export interface City {
  id: string
  name: string
  ascii: string
  alt_name: string
  lat: string
  long: string
  feat_class: string
  feat_code: string
  country: string
  cc2: string
  admin1: string
  admin2: string
  admin3: string
  admin4: string
  population: string
  elevation: string
  dem: string
  tz: string
  modified_at: string
}

export interface States {
  [code: string]: string
}

export interface Suggestion {
  name: string
  latitude: string
  longitude: string
  score: number
}
