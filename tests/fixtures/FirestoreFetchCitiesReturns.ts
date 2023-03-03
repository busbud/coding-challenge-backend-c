import { City } from '../../app/entities/City'

export const mockCities: City[] = [
    {
        country: 'CA',
        id: '6090408',
        latitude: 43.7251,
        longitude: -80.96723,
        name: 'North Perth',
        population: 12254
    },
    {
        country: 'CA',
        id: '4161150',
        latitude: 28.09196,
        longitude: -81.72341,
        name: 'Lake Alfred',
        population: 5015
    },
    {
        country: 'CA',
        id: '6090405',
        latitude: 50.7251,
        longitude: -10.96723,
        name: 'Northhampton',
        population: 20000
    }
]
export const fireStoreDbQueryResponse: any = {
    citiesResponse1: [
        {
            data: (): City => {
                return mockCities[0]
            }
        },
        {
            data: (): City => {
                return mockCities[1]
            }
        },
        {
            data: (): City => {
                return mockCities[2]
            }
        }
    ]
}
