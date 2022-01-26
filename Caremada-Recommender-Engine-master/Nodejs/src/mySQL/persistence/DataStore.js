const mysql = require("mysql");

const con = mysql.createConnection({
    host: "localhost",
    user: "alex",
    password: process.env.DB_PASSWORD,
    database: "caremada"
});

con.connect(function (err) {
    if (err) {
        console.log('Connection failed!', 'Error: ', err);
    } else {
        console.log('Connected successfully to mySQL');
    }
});


function getDataSet(datasetName) {
    let headers = [];
    return new Promise(resolve => {
            con.query(`SELECT * FROM ${datasetName}`, function (err, result, fields) {
            if (err) throw err;
            fields.forEach(e => headers.push(e.name));
            resolve({ headers, result });
        })
    });
}


function getRecord(datasetName, id) {
    let headers = [];
    return new Promise(resolve => {
        con.query(`SELECT * FROM ${datasetName} WHERE id=${id}`, function (err, record, fields) {
            if (err) throw err;
            fields.forEach(e => headers.push(e.name));
            resolve({ headers, record: record[0] });
        })
    });
}

    
function editRecord(datasetName, id, record)
{
    let setQuery = "SET";
    for (const prop in record) {
        setQuery += ` ${prop} = \'${record[prop]}\',`;
    }
    setQuery = setQuery.substring(0, setQuery.length - 1);

    return new Promise(resolve => {
        con.query(`UPDATE ${datasetName} ${setQuery} WHERE id=${id}`, function (err, record, fields) {
            if (err) throw err;
            resolve();
        })
    });
}


function insertRecord(datasetName, record)
{
    let insertQuery = "(";
    for (const prop in record) {
        insertQuery += ` ${prop},`;
    }
    insertQuery = insertQuery.substring(0, insertQuery.length - 1);
    insertQuery += ") VALUES (";
    for (const prop in record) {
        insertQuery += ` \'${record[prop]}\',`;
    }
    insertQuery = insertQuery.substring(0, insertQuery.length - 1);
    insertQuery += ");";

    return new Promise(resolve => {
        con.query(`INSERT INTO ${datasetName} ${insertQuery}`, function (err, record, fields) {
            if (err) throw err;
            resolve();
        })
    });
}


function deleteRecord(datasetName, id)
{
    return new Promise(resolve => {
        con.query(`DELETE FROM ${datasetName} WHERE id=${id}`, function (err, record, fields) {
            if (err) throw err;
            resolve();
        })
    });
}


module.exports = {
    getDataSet,
    getRecord,
    editRecord,
    insertRecord,
    deleteRecord
}