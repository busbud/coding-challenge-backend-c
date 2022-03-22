import { Module } from "@nestjs/common";
import { GraphQLModule } from "@nestjs/graphql";
import { PrismaService } from "./prisma.service";
import { join } from "path";
import { citiesresolver } from "./resolvers.cities";
import { SuggestionsController } from "./suggestions/suggestions.controller";
import { SuggestionsService } from "./suggestions/suggestions.service";

//set up app module with controller and providers
@Module({
  imports: [
    GraphQLModule.forRoot({
      autoSchemaFile: join(process.cwd(), "src/schema.gql"),
      buildSchemaOptions: { dateScalarMode: "timestamp" },
    }),
  ],
  controllers: [SuggestionsController],
  providers: [PrismaService, citiesresolver, SuggestionsService],
})
export class AppModule {}
