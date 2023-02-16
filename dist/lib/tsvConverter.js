"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.tsvToJsonArray = void 0;
const fs_1 = __importDefault(require("fs"));
function tsvToJsonArray(filePath) {
    const tsvData = fs_1.default.readFileSync(filePath, 'utf-8');
    // Split the TSV data into rows and columns
    const rows = tsvData.split('\n');
    const headers = rows[0].split('\t');
    // Convert each row into an object and add it to the array
    const jsonData = [];
    for (let i = 1; i < rows.length; i++) {
        const row = rows[i].split('\t');
        const obj = {};
        for (let j = 0; j < headers.length; j++) {
            obj[headers[j]] = row[j];
        }
        jsonData.push(obj);
    }
    return jsonData;
}
exports.tsvToJsonArray = tsvToJsonArray;
//# sourceMappingURL=tsvConverter.js.map