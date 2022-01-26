const router = require('express').Router();
const net = require('net');
const dataStore = require('../persistence/DataStore');


router.get("/", (req, res) => {
    res.render("home");
});


router.get("/display/:datasetName", async (req, res) => {
    const datasetName = req.params.datasetName;
    const dataset = await dataStore.getDataSet(datasetName);
    res.render("display", {
        datasetName,
        headers: dataset.headers,
        dataset: dataset.result
    });
});


router.get("/reload/:datasetName", async (req, res) => {
    const datasetName = req.params.datasetName;
    await dataStore.reloadDataset(datasetName);
    res.redirect(`/display/${datasetName}`);
});


router.get("/recommendation/:datasetName/:key", async (req, res) => {
    const key = req.params.key;
    const datasetName = req.params.datasetName;
    const dataset = await dataStore.getDataSet(datasetName);
    const { record } = await dataStore.getRecord(datasetName, key);
    let recommendations = [];

    const client = new net.Socket();
    client.connect(5050, '127.0.0.1', () => {
        client.write(JSON.stringify({
            "algorithm_t": "content",
            "tableName": datasetName,
            "pkey_column_name": "id",
            "pkey_val": key
        }));
    });

    client.on('data', data => {
        recommendations = JSON.parse(data.toString());
        client.destroy();
    });

    client.on('close', () => {
        console.log(record);
        res.render("recommendations", {
            record,
            datasetName,
            headers: dataset.headers,
            dataset: recommendations
        });
    });
});


router.get("/insert/:datasetName", async (req, res) => {
    const datasetName = req.params.datasetName;
    const { headers } = await dataStore.getDataSet(datasetName);
    console.log(headers);
    res.render("insert", { datasetName, headers });
});


router.post("/insert/:datasetName", async (req, res) => {
    const datasetName = req.params.datasetName;
    let record = {};
    
    for (const prop of Object.keys(req.body)) {
        record[prop] = req.body[prop];
    }
    await dataStore.insertRecord(datasetName, record);
    res.redirect(`/display/${datasetName}`);
});


router.get("/edit/:datasetName/:key", async (req, res) => {
    const key = req.params.key;
    const datasetName = req.params.datasetName;
    const { record } = await dataStore.getRecord(datasetName, key);
    res.render("edit", { datasetName, record });
});


router.post("/edit/:datasetName", async (req, res) => {
    const datasetName = req.params.datasetName;
    let record = {};

    for (const prop of Object.keys(req.body)) {
        record[prop] = req.body[prop];
    }

    await dataStore.editRecord(datasetName, record.id, record);
    res.redirect(`/display/${datasetName}`);
});


router.post("/delete/:datasetName/:key", async (req, res) => {
    const key = req.params.key;
    const datasetName = req.params.datasetName;
    await dataStore.deleteRecord(datasetName, key);
    res.redirect(`/display/${datasetName}`);
});


router.get("/test/:datasetName/:key", async (req, res) => {
    const key = req.params.key;
    const datasetName = req.params.datasetName;
    let recommendations = [];

    const client = new net.Socket();
    client.connect(5050, '127.0.0.1', () => {
        client.write(JSON.stringify({
            "algorithm_t": "content",
            "tableName": datasetName,
            "pkey_column_name": "id",
            "pkey_val": key
        }));
    });

    client.on('data', function (data) {
        recommendations = JSON.parse(data.toString());
        client.destroy();
    });

    client.on('close', function () {
        res.send(recommendations);
    });
});


module.exports = router;