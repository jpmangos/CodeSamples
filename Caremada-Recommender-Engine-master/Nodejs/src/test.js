require('./app')
const { db_connect, getDataSet } = require('./persistence/DataStore')
const http = require('http');
const { expect } = require('chai');


function runTests() {

    let dataset;
    let recommendations;
    let options = {
        hostname: 'localhost',
        port: 3000,
        method: 'GET',
        headers: {
            'Content-Type': 'application/x-www-form-urlencoded'
        }
    };

    before(done => {
        if (db_connect.readyState !== 1) {
            db_connect.once("open", (err, db) => done());
        } else {
            done();
        }
    });

    describe("Test 1: Retrieve the Caregiver dataset from the database", () => {

        it("checks if we received an array of objects", async () => {
            expect(dataset).to.not.be.an.instanceof(Array);
            dataset = (await getDataSet("caregivers")).data;
            expect(dataset).to.be.an.instanceof(Array);
            expect(dataset[0]).to.be.an.instanceof(Object);
        });
    });

    describe("Test 2: Request recommendations for a caregiver", () => {

        it("checks if the server responds with an array of caregiver objects", done => {
            const id = dataset[0]._id;
            options.path = `/test/caregivers/${id}`

            const req = http.request(options, res => {
                res.on("data", data => {
                    recommendations = JSON.parse(String(data));
                    expect(recommendations).to.be.an.instanceof(Array);
                    expect(recommendations.length).to.equal(10);
                    expect(recommendations[0]).to.be.an.instanceof(Object);
                });
                res.on("end", () => done());
            });
            req.end();
        });
    });

    describe("Test 3: Request recommendations for a non-existent caregiver", () => {

        it("checks if the server responds with no recommendations", done => {
            const id = "abc123";
            options.path = `/test/caregivers/${id}`

            const req = http.request(options, res => {
                res.on("data", data => {
                    recommendations = JSON.parse(String(data));
                    expect(recommendations).to.deep.equal([]);
                });
                res.on("end", () => { done(); process.exit(0); });
            });
            req.end();
        });
    });

}

runTests();
