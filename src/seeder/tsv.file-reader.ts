import { createInterface, Interface } from 'readline';
import { ReadStream } from 'fs';
import { Observable } from 'rxjs';

export class TsvFileReader {
  private interface: Interface;

  constructor(stream: ReadStream) {
    this.interface = createInterface({
      input: stream,
      crlfDelay: Infinity,
    });
  }

  readLines(): Observable<string> {
    return new Observable<string>((sub) => {
      this.interface.on('line', (line) => sub.next(line));
      this.interface.on('close', () => sub.complete());
    });
  }
}
