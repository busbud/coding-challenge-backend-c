const http = require("http");
const path = require("path");
const test = require("ava");
const got = require("got");
const dotenv = require("dotenv");
const listen = require("test-listen");
const app = require("../../src/app");
const { getServerPort } = require("../../src/utils");

const result = dotenv.config({ path: path.resolve(process.cwd(), "locals.env") });

if (result.error) {
  throw result.error;
}

test.before(async (t) => {
  t.context.server = http.createServer(app);
  t.context.prefixUrl = "http://localhost:" + getServerPort();
});

test.after.always((t) => {
  t.context.server.close();
});

test.serial("get /suggestions?q=new york", async (t) => {
  const result = await got("suggestions?q=new york", { prefixUrl: t.context.prefixUrl }).json();
  t.is(result.suggestions[0].name, "West New York, NJ, US");
});

test.serial("get /suggestions?q=los&longitude=-122&latidute=37", async (t) => {
  const result = await got("suggestions?q=los&longitude=-122&latidute=37", { prefixUrl: t.context.prefixUrl }).json();
  t.is(result.suggestions[0].name, "Los Serranos, CA, US");
});
