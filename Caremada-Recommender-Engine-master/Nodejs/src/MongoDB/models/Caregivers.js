const mongoose = require('mongoose');

const caregiverSchema = new mongoose.Schema({
  'Name': String,
  'Occupation': String,
  'Services': String,
  'Age': Number,
  'Availability': String,
  'Location': String,
  'Rating': Number,
});

let headers = Object.keys(caregiverSchema.paths);
let model = mongoose.model('Caregiver', caregiverSchema);

module.exports = {
  headers,
  model
};