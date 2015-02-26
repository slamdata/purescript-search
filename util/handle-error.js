module.exports = function(source) {
    source.on("error", function(e) {
        console.error("\u0007");
        console.error(e.message);
        source.end();
    });
};
