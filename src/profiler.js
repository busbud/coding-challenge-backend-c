const profiler = () => {    
    const used = process.memoryUsage();
        for (let key in used) {
            used[key] =  Math.round(used[key] / 1024 / 1024 * 100) / 100 + 'MB';
        }
    console.table(used);
}

module.exports = profiler;
    