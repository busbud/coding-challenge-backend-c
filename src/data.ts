// NOTE: TSV parsing could also be done with csv-parse lib
import { promises as fs } from 'fs'
import { City, States } from './types'

export const getCities = async (
  filepath = 'data/cities_canada-usa.tsv'
): Promise<City[]> => {
  const tsv = await fs.readFile(filepath, 'utf-8')

  const lines = tsv.trim().split('\n')

  const keys = lines[0].split('\t')

  const cities: City[] = []

  for (let i = 1; i < lines.length; i++) {
    const city: any = {} // eslint-disable-line @typescript-eslint/no-explicit-any

    const values = lines[i].split('\t')

    for (let j = 0; j < keys.length; j++) {
      city[keys[j]] = values[j]
    }

    cities.push(city)
  }

  return cities
}

export const getStates = async (
  filepath = 'data/provinces-states.tsv'
): Promise<States> => {
  const tsv = await fs.readFile(filepath, 'utf-8')

  const lines = tsv.trim().split('\n')

  const states: States = {}

  for (const line of lines) {
    const [code, name] = line.split('\t')

    states[code] = name
  }

  return states
}
