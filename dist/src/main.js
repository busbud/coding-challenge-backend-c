"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const core_1 = require("@nestjs/core");
const app_module_1 = require("./app.module");
async function server() {
    const app = await core_1.NestFactory.create(app_module_1.AppModule);
    await app.listen(process.env.PORT, () => {
        console.log(`
🚀 Server ready at: http://localhost:3000/graphql
⭐️ See sample queries: http://pris.ly/e/ts/graphql-nestjs#using-the-graphql-api
`);
    });
}
server();
//# sourceMappingURL=main.js.map