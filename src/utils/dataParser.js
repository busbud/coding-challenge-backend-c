import * as fs from "fs";
import * as csv from "csv";

const provinceCodes = {
  1: "AB",
  2: "BC",
  3: "MB",
  4: "NB",
  13: "NT",
  7: "NS",
  14: "NU,",
  8: "ON",
  9: "PE",
  10: "QC",
  11: "SK",
  12: "YT",
  5: "NL",
};

export const tsvToJson = async (file) => {
  return new Promise((resolve, reject) => {
    let output = [];

    if (!fs.existsSync(file)) {
      console.log("File not found");
      reject();
    }
    const input = fs.createReadStream(file);

    const transformer = csv.transform((data) => {
      if (data.admin1 !== undefined && data.country === "CA") {
        data.admin1 = provinceCodes[parseInt(data.admin1)];
      }

      return {
        id: data.id,
        name: data.ascii,
        state: data.admin1,
        country: data.country,
        population: data.population,
        latitude: data.lat,
        longitude: data.long,
      };
    });

    const parser = csv.parse({
      columns: true,
      delimiter: "\t",
      quote: '""',
      escape: "\\",
    });

    transformer.on("readable", (data) => {
      while ((data = transformer.read())) {
        output.push(data);
      }
    });

    parser.on("readable", (data) => {
      while ((data = parser.read())) {
        transformer.write(data);
      }
    });

    parser.on("error", (err) => {
      console.log(err.message);
    });

    input.pipe(parser);

    parser.on("finish", () => {
      resolve(output);
    });
  });
};
