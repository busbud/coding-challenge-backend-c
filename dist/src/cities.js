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
Object.defineProperty(exports, "__esModule", { value: true });
exports.citiesRes = exports.cities = void 0;
require("reflect-metadata");
const graphql_1 = require("@nestjs/graphql");
let cities = class cities {
};
__decorate([
    (0, graphql_1.Field)((type) => graphql_1.Int),
    __metadata("design:type", Number)
], cities.prototype, "id", void 0);
__decorate([
    (0, graphql_1.Field)((type) => String),
    __metadata("design:type", String)
], cities.prototype, "name", void 0);
__decorate([
    (0, graphql_1.Field)((type) => String),
    __metadata("design:type", String)
], cities.prototype, "ascii_name", void 0);
__decorate([
    (0, graphql_1.Field)((type) => String),
    __metadata("design:type", String)
], cities.prototype, "alternate_name", void 0);
__decorate([
    (0, graphql_1.Field)((type) => graphql_1.Float),
    __metadata("design:type", Float32Array)
], cities.prototype, "latitude", void 0);
__decorate([
    (0, graphql_1.Field)((type) => graphql_1.Float),
    __metadata("design:type", Float32Array)
], cities.prototype, "longitude", void 0);
__decorate([
    (0, graphql_1.Field)((type) => String),
    __metadata("design:type", String)
], cities.prototype, "country", void 0);
__decorate([
    (0, graphql_1.Field)((type) => String),
    __metadata("design:type", String)
], cities.prototype, "state", void 0);
__decorate([
    (0, graphql_1.Field)((type) => Date),
    __metadata("design:type", Date)
], cities.prototype, "createdAt", void 0);
__decorate([
    (0, graphql_1.Field)((type) => Date),
    __metadata("design:type", Date)
], cities.prototype, "updatedAt", void 0);
cities = __decorate([
    (0, graphql_1.ObjectType)()
], cities);
exports.cities = cities;
let citiesRes = class citiesRes {
};
__decorate([
    (0, graphql_1.Field)((type) => graphql_1.Int),
    __metadata("design:type", Number)
], citiesRes.prototype, "id", void 0);
__decorate([
    (0, graphql_1.Field)((type) => String),
    __metadata("design:type", String)
], citiesRes.prototype, "name", void 0);
__decorate([
    (0, graphql_1.Field)((type) => graphql_1.Float),
    __metadata("design:type", Float32Array)
], citiesRes.prototype, "latitude", void 0);
__decorate([
    (0, graphql_1.Field)((type) => graphql_1.Float),
    __metadata("design:type", Float32Array)
], citiesRes.prototype, "longitude", void 0);
__decorate([
    (0, graphql_1.Field)((type) => graphql_1.Float),
    __metadata("design:type", Float32Array)
], citiesRes.prototype, "distance", void 0);
__decorate([
    (0, graphql_1.Field)((type) => graphql_1.Float),
    __metadata("design:type", Float32Array)
], citiesRes.prototype, "score", void 0);
citiesRes = __decorate([
    (0, graphql_1.ObjectType)()
], citiesRes);
exports.citiesRes = citiesRes;
//# sourceMappingURL=cities.js.map