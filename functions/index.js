const functions = require('firebase-functions');
const admin = require('firebase-admin'); // The Firebase Admin SDK to access the Firebase Realtime Database.

admin.initializeApp();

// // Create and Deploy Your First Cloud Functions
// // https://firebase.google.com/docs/functions/write-firebase-functions
//
// exports.helloWorld = functions.https.onRequest((request, response) => {
//  response.send("Hello from Firebase!");
// });

exports.createAccountDocument = functions.auth.user().onCreate((user) => {
  // write new doc to collection
  return admin.firestore().collection('users').doc(user.uid).set({
    color: "",
    thoughts: []
  }); 
});
