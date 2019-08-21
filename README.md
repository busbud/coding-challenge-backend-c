# Busbud Coding Challenge (Back-End)

`/suggestions` endpoint with `q`, `latitude`, and `longitude` query params

## Scripts

```sh
# Start dev server
npm run dev

# Run test suite
npm t

# Lint source files
npm run lint

# Build for prod
npm run build

# Start in prod
npm start
```

## Checklist

- [x] query string validation

- [x] most efficient Levenshtein algorithm

- [x] strip diacritics (e.g. `Montréal` -> `Montreal`)

- [x] case-insensitive match (e.g. `Montreal` same as `montreal`)

- [x] state and country under city `name`

- [x] non-blocking TSV import

- [x] rate limiting

- [x] unit tests

- [ ] `latitude` and `longitude` sorting

## References

- states and provinces used in `provinces-states.tsv` from [`admin1CodesASCII.txt`](http://download.geonames.org/export/dump/admin1CodesASCII.txt)
