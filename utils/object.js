/** deeply get an object by a dotted notation path like a.b.c */
function at(obj, path) {
  if (!obj) throw new Error("object is required");
  if (path === "") throw new Error("path cannot be void");
  return path.split(".").reduce((base, currentKey) => {
    if (base[currentKey]) return base[currentKey];
    return undefined;
  }, obj);
}

function getAt(obj, path) {
  return;
}
