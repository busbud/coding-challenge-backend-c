
export class CityData {
    readonly id: string;
    readonly name: string;
    readonly ascii: string;
    readonly alt_name: string[] = [];
    readonly lat: number;
    readonly long: number;
    readonly feat_class: string;
    readonly feat_code: string;
    readonly country: string;
    readonly cc2: string;
    readonly admin1: string;
    readonly admin2: string;
    readonly admin3: string;
    readonly admin4: string;
    readonly population: number;
    readonly elevation: number;
    readonly dem: string;
    readonly tz: string;
    readonly modified_at: number;
    public parseToModel(obj: { [key: string]: string }): CityData {
        Object.keys(obj).forEach((key) => {
            if (key) {
                (this as any)[key] = obj[key];
            }
        });
        return this;
    }
}