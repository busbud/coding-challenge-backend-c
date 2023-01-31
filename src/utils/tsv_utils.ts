import {ICityRawData} from "../interfaces/raw_cities";


const tsv_header_arrangement = ["id",
    "name",
    "ascii",
    "alt_name",
    "latitude",
    "longitude",
    "feat_class",
    "feat_code",
    "country",
    "cc2",
    "admin1",
    "admin2",
    "admin3",
    "admin4",
    "population",
    "elevation",
    "dem",
    "timezone",
    "modified_at"]

export function tabbedGeoDataToObject(dataArray: Array<string>): ICityRawData {
    return dataArray.reduce((previousValue, currentValue, currentIndex) => {
        const key = tsv_header_arrangement[currentIndex];
        return {...previousValue, [key]: currentValue.length ? currentValue : undefined}
    }, {}) as ICityRawData
}