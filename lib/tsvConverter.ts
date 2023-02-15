import fs from 'fs';

interface TsvRow {
    [key: string]: string;
}

export function tsvToJsonArray(filePath: string): Record<string, unknown>[] {
    const tsvData = fs.readFileSync(filePath, 'utf-8');

    // Split the TSV data into rows and columns
    const rows = tsvData.split('\n');
    const headers = rows[0].split('\t');

    // Convert each row into an object and add it to the array
    const jsonData: TsvRow[] = [];
    for (let i = 1; i < rows.length; i++) {
        const row = rows[i].split('\t');
        const obj: TsvRow = {};
        for (let j = 0; j < headers.length; j++) {
            obj[headers[j]] = row[j];
        }
        jsonData.push(obj);
    }

    return jsonData;
}


