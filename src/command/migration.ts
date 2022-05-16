import path from 'path';
import db from 'service/database';
import { migration as migrationConfig } from 'config/database';

const [action, name] = process.argv.slice(2);

(async () => {
  if (['run', 'up', 'down', 'reset'].includes(action)) {
    let direction = '';
    let list = [];
    switch (action) {
      case 'run':
        direction = 'up';
        [, list] = await db.migrate.latest(migrationConfig);
        break;
      case 'up':
        direction = 'up';
        [, list] = await db.migrate.up(migrationConfig);
        break;
      case 'down':
        direction = 'down';
        [, list] = await db.migrate.down(migrationConfig);
        break;
      case 'reset':
        direction = 'down';
        [, list] = await db.migrate.rollback(migrationConfig, true);
        break;
      default:
    }
    if (list.length > 0) {
      process.stdout.write(`Run ${list.length} migration${list.length > 1 ? 's' : ''} ${direction} :\n`);
      list.forEach((file: any) => {
        process.stdout.write(`- ${path.parse(file).name}\n`);
      });
    } else {
      process.stdout.write('Nothing to do\n');
    }
  } else {
    switch (action) {
      case 'list': {
        const current = await db.migrate.currentVersion(migrationConfig);
        let list = await db.migrate.list(migrationConfig);
        list = list[0]
          .map((file: any) => path.parse(file.name).name)
          .concat(list[1].map((file: any) => path.parse(file.file).name));
        if (list.length === 0) {
          process.stdout.write('No migration\n');
        } else {
          process.stdout.write('Migrations :\n');
          list.forEach((file: any) => {
            process.stdout.write(`${file.substring(0, 14) === current ? '>' : '-'} ${file}\n`);
          });
        }
        break;
      }
      case 'make': {
        if (name) {
          const file = await db.migrate.make(name, migrationConfig);
          process.stdout.write('New migration :\n');
          process.stdout.write(`- ${path.parse(file).name}\n`);
        } else {
          process.stdout.write('Missing migration name\n');
        }
        break;
      }
      default: {
        process.stdout.write('Unrecognized action\n');
        break;
      }
    }
  }
  await db.destroy();
})();
