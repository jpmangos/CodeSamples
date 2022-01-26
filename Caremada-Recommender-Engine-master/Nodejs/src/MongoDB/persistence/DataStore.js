const mongoose = require("mongoose");
const csvtojson = require("csvtojson");

//const uri = process.env.DB_CONNECTION;
const uri = "mongodb://localhost:27017/caremadaDB";
mongoose.connect(uri, { useNewUrlParser: true, useUnifiedTopology: true }, () => console.log("Connected to MongoDB server."));

const Movie = require('../models/Movie.js')
const Caregiver = require('../models/Caregivers.js')


function getModel(name) {
    switch (name) {
        case 'caregivers':
            return Caregiver.model;
        case 'movies':
            return Movie.model;
    }
}


function getHeaders(name) {
    switch (name) {
        case 'caregivers':
            return Caregiver.headers;
        case 'movies':
            return Movie.headers;
    }
}


function getCsvName(name) {
    switch (name) {
        case 'caregivers':
            return "/Mock_Caremada.csv";
        case 'movies':
            return "/sample_movie_dataset.csv";
    }
}


async function getDataSet(datasetName) {
    return {
        headers: getHeaders(datasetName),
        data: await getModel(datasetName).find().exec()
    }
}


function reloadDataset(datasetName) {
    const model = getModel(datasetName);
    return model.deleteMany().exec()
        .then(csvtojson()
            .fromFile(__dirname + getCsvName(datasetName))
            .then(csvData => model.create(csvData))
        )
}


function getRecord(datasetName, id) {
    return getModel(datasetName).findById(id).exec();
}


function insertRecord(datasetName, record) {
    return getModel(datasetName).create(record);
}


function editRecord(datasetName, id, record) {
    return getModel(datasetName).updateOne({ _id: id }, { $set: record }).exec();
}


function deleteRecord(datasetName, id) {
    return getModel(datasetName).findByIdAndDelete(id).exec();
}


module.exports = {
    getHeaders,
    getDataSet,
    reloadDataset,
    getRecord,
    insertRecord,
    editRecord,
    deleteRecord,
    db_connect: mongoose.connection
}