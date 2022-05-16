import path from 'path';
import db from 'service/database';
import { seed as seedConfig } from 'config/database';

const [action, name] = process.argv.slice(2);

(async () => {
  switch (action) {
    case 'run': {
      const [list] = await db.seed.run(seedConfig);
      if (list.length > 0) {
        process.stdout.write(`Run ${list.length} seed${list.length > 1 ? 's' : ''} :\n`);
        list.forEach((file: any) => {
          process.stdout.write(`- ${path.parse(file).name}\n`);
        });
      } else {
        process.stdout.write('Nothing to do\n');
      }
      break;
    }
    case 'make': {
      if (name) {
        const file = await db.seed.make(name, seedConfig);
        process.stdout.write('New seed :\n');
        process.stdout.write(`- ${path.parse(file).name}\n`);
      } else {
        process.stdout.write('Missing seed name\n');
      }
      break;
    }
    default: {
      process.stdout.write('Unrecognized action\n');
      break;
    }
  }
  await db.destroy();
})();
