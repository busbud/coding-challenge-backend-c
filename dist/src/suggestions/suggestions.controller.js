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
exports.SuggestionsController = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma.service");
let SuggestionsController = class SuggestionsController {
    constructor(prismaService) {
        this.prismaService = prismaService;
    }
    async getByName(q) {
        let res;
        if (!q.latitude && !q.longitude) {
            res = await this.prismaService.$queryRaw `   SELECT 
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
        websearch_to_tsquery(${q.q}) query,
        SIMILARITY(${q.q}, cities.name || cities.ascii_name || cities.state) similarity
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
        websearch_to_tsquery(${q.q}) query,
        SQRT(POWER(69.1 * ( cities.latitude - ${parseFloat(q.latitude)}),  2) + POWER(69.1 * ( ${parseFloat(q.longitude)}  - cities.longitude )  * COS(cities.latitude / 57.3), 2)) distance,
        SIMILARITY(${q.q}, cities.name || cities.alternate_name || cities.ascii_name) similarity
    WHERE query @@ document OR similarity > 0 AND cities.population > 5000 AND distance < 200
    ORDER BY similarity DESC NULLS LAST
    LIMIT 8`;
        }
        const result = await res.map((element) => ({
            name: element.name + ", " + element.state + ", " + element.country,
            latitude: element.latitude,
            longitude: element.longitude,
            distance: element.distance,
            score: element.similarity,
        }));
        if (!!result[0].score && result[0].score < 0.19) {
            throw new common_1.NotFoundException({ suggestions: [] }, 'no close suggestions found');
        }
        else
            return { suggestions: result };
    }
};
__decorate([
    (0, common_1.Get)(),
    __param(0, (0, common_1.Query)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", Promise)
], SuggestionsController.prototype, "getByName", null);
SuggestionsController = __decorate([
    (0, common_1.Controller)("suggestions"),
    __param(0, (0, common_1.Inject)(prisma_service_1.PrismaService)),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService])
], SuggestionsController);
exports.SuggestionsController = SuggestionsController;
//# sourceMappingURL=suggestions.controller.js.map