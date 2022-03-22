import { PrismaService } from "./prisma.service";
export declare class CitiesCreateInput {
    search: string;
}
export declare class citiesresolver {
    private prismaService;
    constructor(prismaService: PrismaService);
    searchCities(search: string, lat: number, long: number): Promise<any>;
}
