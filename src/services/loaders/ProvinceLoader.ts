import fs from 'fs';

class ProvinceLoader {

    load(path: string): Map<string, string> {
        try {
            return fs.readFileSync(path, 'ascii')
                .split('\n')
                .map((line) => line.split('\t'))
                .reduce((prev: Map<string, string>, curr: string[]) => {
                    prev.set(curr[0], curr[1]);
                    return prev;
                }, new Map<string, string>());
            return null;
        } catch (error) {
            console.error(error);
            throw new Error(`Error to load ${path}`);
        }
    }

}

export default new ProvinceLoader().load('data/admin1CodesASCII.txt')