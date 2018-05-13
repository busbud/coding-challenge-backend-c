import { Client } from "pg";

const conString = process.env.DATABASE_URL || "postgres://benoit:benoit@localhost:5432/busbud";

const asyncQuery = (client, query, params = []) => new Promise((resolve, reject) => client.query(query, params, (err, result) => {
    if (err) return reject(err);
    resolve(result);
}));

// databaseUpdater is used to update the cities stored in the database. When called, it returns two functions:
// -updateLine, which will update a city depending on its "modified" field
// -end, used to close the psql client
export const databaseUpdater = async () => {
	const client = new Client(conString);
	client.connect();
    let linesToAdd = 0;
    let linesAdded = 0;
    let linesEnded = false;
    let lastDate;

    try {
        // The lastDate is used to know which fields need to be updated.
        // Given the current retriever, this is not necessary, but it will improve performance on a real-life data scenario
        const lastDateResult = await asyncQuery(client, "SELECT date FROM last_update;");
        lastDate = new Date(lastDateResult.rows[0].date);
        await asyncQuery(client, "UPDATE last_update SET date=CURRENT_DATE;");
    } catch (err) {
        console;log("Error handling last_update date:", err);
    }

    const closeIfDone = () => {
        if (linesEnded && linesToAdd === linesAdded) {
            client.end();
            console.log("All cities have been updated. Closing psql client");
        }
    }

    return {
        updateLine: async parameters => {
            try {
                if (lastDate <= new Date(parameters[18])) {
                    linesToAdd += 1;
                    await asyncQuery(
                        client,
                        "DELETE FROM cities WHERE id=$1;",
                        [parameters[0]],
                    );
                    await asyncQuery(
                        client,
                        "INSERT INTO cities(id, name, ascii, alt_names, lat, long, feat_class, feat_code, country, cc2, admin1, admin2, admin3, admin4, population, elevation, dem, tz, modified) VALUES($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19);",
                        parameters,
                    );
                    // UPSERT could have merged the two previous requests, but since it is not supported on older versions of PSQL, I decided to not use it for this test
                    linesAdded += 1;
                    console.log("Updated line ", parameters[0]);
                    closeIfDone();
                }
            } catch (err) {
                console.log("Error updating line:", err);
            }
        },
        end: () => {
            linesEnded = true;
            closeIfDone();
        },
    }
};

export const searchDatabase = value => {
    const client = new Client(conString);
    client.connect();
    return new Promise((resolve, reject) => {
        client.query(
            "SELECT ascii AS name, lat AS latitude, long AS longitude, population FROM cities WHERE population>5000 AND ascii LIKE $1;",
            [`%${value}%`],
            (err, result) => {
                client.end();
                if (err) return reject("Error deleting all cities in US and Canada:", err);
                resolve(result.rows)
            }
        );
    });
}
