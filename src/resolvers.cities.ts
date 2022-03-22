import { Resolver, Query, Args, InputType, Field } from "@nestjs/graphql";

import { Inject } from "@nestjs/common";
import { citiesRes } from "./cities";
import { PrismaService } from "./prisma.service";

@InputType()
export class CitiesCreateInput {
  @Field()
  search: string;
}

//create graphql resolver for cities search queries
@Resolver(citiesRes)
export class citiesresolver {
  constructor(@Inject(PrismaService) private prismaService: PrismaService) {}

  @Query((returns) => [citiesRes])
  async searchCities(
    @Args("search") search: string,
    @Args("lat", { nullable: true }) lat: number,
    @Args("long", { nullable: true }) long: number
  ) {
    let res;
    if (!lat && !long) {
      //custom write pgsql queries using prismaService $queryRaw, safe from sqlinjection exploits
      res = await this.prismaService.$queryRaw`
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
    } else {
      res = await this.prismaService.$queryRaw`
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
}
