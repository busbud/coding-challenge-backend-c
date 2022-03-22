import { NestFactory } from "@nestjs/core";
import { AppModule } from "./app.module";

//create server using nestjs
async function server() {
  const app = await NestFactory.create(AppModule);
  await app.listen(process.env.PORT, () => {
    console.log(`
🚀 Server ready at: http://localhost:3000/graphql
⭐️ See sample queries: http://pris.ly/e/ts/graphql-nestjs#using-the-graphql-api
`);
  });
}

server();
