import "reflect-metadata";
export declare class cities {
    id: number;
    name: string;
    ascii_name: string;
    alternate_name: string;
    latitude: Float32Array;
    longitude: Float32Array;
    country: string;
    state: string;
    createdAt: Date;
    updatedAt: Date;
}
export declare class citiesRes {
    id: number;
    name: string;
    latitude: Float32Array;
    longitude: Float32Array;
    distance: Float32Array;
    score: Float32Array;
}
