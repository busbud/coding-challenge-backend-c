export class Score {
    constructor(readonly name: string, readonly latitude: number, readonly longitude: number, readonly score: number) {
        this.score = Math.round(score * 100) / 100;
    }
}