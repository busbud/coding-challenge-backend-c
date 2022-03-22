"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
var __param = (this && this.__param) || function (paramIndex, decorator) {
    return function (target, key) { decorator(target, key, paramIndex); }
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.citiesresolver = exports.CitiesCreateInput = void 0;
const graphql_1 = require("@nestjs/graphql");
const common_1 = require("@nestjs/common");
const cities_1 = require("./cities");
const prisma_service_1 = require("./prisma.service");
let CitiesCreateInput = class CitiesCreateInput {
};
__decorate([
    (0, graphql_1.Field)(),
    __metadata("design:type", String)
], CitiesCreateInput.prototype, "search", void 0);
CitiesCreateInput = __decorate([
    (0, graphql_1.InputType)()
], CitiesCreateInput);
exports.CitiesCreateInput = CitiesCreateInput;
let citiesresolver = class citiesresolver {
    constructor(prismaService) {
        this.prismaService = prismaService;
    }
    async searchCities(search, lat, long) {
        let res;
        if (!lat && !long) {
            res = await this.prismaService.$queryRaw `
            SELECT 
            cities.name,
            cities.state,
            cities.country,
            cities.population,
            cities.latitude,
            cities.longitude,
            similarity
        FROM 
            cities, 
            to_tsvector(cities.name || cities.alternate_name || cities.ascii_name || cities.state || cities.country) document,
            websearch_to_tsquery(${search}) query,
            SIMILARITY(${search}, cities.name || cities.alternate_name || cities.ascii_name || cities.state) similarity
        WHERE query @@ document OR similarity > 0 AND cities.population > 5000
        ORDER BY similarity DESC NULLS LAST
        LIMIT 8`;
        }
        else {
            res = await this.prismaService.$queryRaw `
                SELECT 
                cities.id,
                cities.name,
                cities.state,
                cities.country,
                cities.population,
                cities.latitude,
                cities.longitude,
                distance,
                similarity
            FROM 
                cities, 
                to_tsvector(cities.name || cities.alternate_name || cities.ascii_name || cities.state || cities.country) document,
                websearch_to_tsquery(${search}) query,
                SQRT(POWER(69.1 * ( cities.latitude - ${lat}),  2) + POWER(69.1 * ( ${long}  - cities.longitude )  * COS(cities.latitude / 57.3), 2)) distance,
                SIMILARITY(${search}, cities.name || cities.alternate_name || cities.ascii_name) similarity
            WHERE query @@ document OR similarity > 0 AND cities.population > 5000 AND distance < 200
            ORDER BY similarity DESC NULLS LAST
            LIMIT 8`;
        }
        const result = res.map((element) => ({
            id: element.id,
            name: element.name,
            latitude: element.latitude,
            longitude: element.longitude,
            distance: element.distance,
            score: element.similarity,
        }));
        return result;
    }
};
__decorate([
    (0, graphql_1.Query)((returns) => [cities_1.citiesRes]),
    __param(0, (0, graphql_1.Args)("search")),
    __param(1, (0, graphql_1.Args)("lat", { nullable: true })),
    __param(2, (0, graphql_1.Args)("long", { nullable: true })),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Number, Number]),
    __metadata("design:returntype", Promise)
], citiesresolver.prototype, "searchCities", null);
citiesresolver = __decorate([
    (0, graphql_1.Resolver)(cities_1.citiesRes),
    __param(0, (0, common_1.Inject)(prisma_service_1.PrismaService)),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService])
], citiesresolver);
exports.citiesresolver = citiesresolver;
//# sourceMappingURL=resolvers.cities.js.map