import byline from 'byline';
import fs from 'fs';
import path from 'path';
import util from 'util';
import stream from 'stream';

// Promisify stream.finish so we can await on it
const finished = util.promisify(stream.finished);

// If a key is not present, throw
const isRequired = (key: any): void => {
    if (!key) {
        throw new Error(`${key} is required`);
    }
};

interface HeaderOpts {
    accumulator: Record<string, any> | any[];
    file: string;
    filter: (line: Record<string, any>,accumulator: Record<string, any>) => void;
    headers: string[] | 'auto';
}

/**
 * Applies the filter to each line of provided file
 * the file is parsed using stream to not bloat the memory
 *
 * @param {HeaderOpts} opts
 * @param opts.accumulator - The initial value to act on with the filter function
 * @param opts.file {string}
 * @param opts.filter {Function} - Will be called on every line of the stream
 * @returns Promise
 */
// eslint-disable-next-line @typescript-eslint/explicit-function-return-type
const tsvToJson = async (opts: HeaderOpts): Promise<any> => {
    const {accumulator, file, filter, headers} = opts;
    [file, filter, headers].forEach(isRequired);

    const stream = fs.createReadStream(path.join(__dirname,'../../data/',file));
    const bylineStream = byline.createStream(stream);
    let finalHeaders: any = headers === 'auto' ? false : headers; // This bit me in the face: https://twitter.com/Elyx0/status/1123066368428003328

    bylineStream.on('readable', (): any =>{
        let line;
        while ((line = bylineStream.read()) !== null) {
            line = line.toString();
            if (!finalHeaders) {
                // First line is the headers
                const splittedLine = line.split('\t');
                finalHeaders = splittedLine;
            } else {
                const elemJSON: { [key: string]: any} = {};
                line.split('\t').forEach((elem: string,index: number): void => {
                    elemJSON[finalHeaders[index]] = elem;
                });
                // If filter passes, do whatever with the accumulator
                filter(elemJSON, accumulator);
            }
        }
    });
    await finished(bylineStream);
    return accumulator;
};

export default tsvToJson;
