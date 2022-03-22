import {
  Controller,
  Get,
  Inject,
  NotFoundException,
  Query,
} from "@nestjs/common";
import { PrismaService } from "../prisma.service";

//create a suggestions controller to open up the /suggestions endpoint
@Controller("suggestions")
export class SuggestionsController {
  constructor(@Inject(PrismaService) private prismaService: PrismaService) {}

  @Get()
  //accept get requests with q parameter
  async getByName(@Query() q) {
    let res;
    //conditional for optional lat and long params
    if (!q.latitude && !q.longitude) {
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
        phraseto_tsquery(${q.q}) query,
        SIMILARITY(${q.q}, cities.name || cities.ascii_name || cities.state) similarity
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
        phraseto_tsquery(${q.q}) query,
        SQRT(POWER(69.1 * ( cities.latitude - ${parseFloat(
          q.latitude
        )}),  2) + POWER(69.1 * ( ${parseFloat(
        q.longitude
      )}  - cities.longitude )  * COS(cities.latitude / 57.3), 2)) distance,
        SIMILARITY(${
          q.q
        }, cities.name || cities.alternate_name || cities.ascii_name) similarity
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
      //throw 404 not found error for search similarity scores under 0.19
      throw new NotFoundException(
        { suggestions: [] },
        "no close suggestions found"
      );
    } else return { suggestions: result };
  }
}
