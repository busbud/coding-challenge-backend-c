const response = (res, status, httpStatus) => {
    res.writeHead(httpStatus, {'Content-Type': 'application/json'});
    res.end(JSON.stringify({
        status: status
    }));
}

module.exports.response = response
