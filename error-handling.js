
module.exports.handleError = (error, res) => {
    const code = dict[error.name] || 500;

    if (code === 404) return res.status(code).json({ suggestions: [] });

    return res.status(code).json({ message: error.message });
}

const dict = {};
dict['ValidationError'] = 400;
dict['NotFound'] = 404;
