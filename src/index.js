import firebase from 'firebase/app';
import 'firebase/auth';
import 'firebase/firestore';
import firebaseui from 'firebaseui';
import firebaseConfig from '../.env.json';
import { Elm } from './Main.elm';

firebase.initializeApp(firebaseConfig);

const auth = firebase.auth();
const db = firebase.firestore();
const ui = new firebaseui.auth.AuthUI(firebase.auth());

class FirebaseAuth extends HTMLElement {
  constructor() {
    super();

    setTimeout(() => {
      ui.start(`#${this.id}`, {
        callbacks: {
          signInSuccessWithAuthResult: function() {
            return false;
          },
        },
        signInFlow: 'popup',
        signInOptions: [
          firebase.auth.GoogleAuthProvider.PROVIDER_ID,
          // firebase.auth.FacebookAuthProvider.PROVIDER_ID,
          // firebase.auth.TwitterAuthProvider.PROVIDER_ID,
          // firebase.auth.GithubAuthProvider.PROVIDER_ID,
          // firebase.auth.EmailAuthProvider.PROVIDER_ID,
          // firebase.auth.PhoneAuthProvider.PROVIDER_ID
        ],
      });
    }, 100);
  }
}

window.customElements.define('firebase-auth', FirebaseAuth);

const app = Elm.Main.init({
  flags: {
    now: new Date().getTime(),
    width: window.outerWidth,
  },
});

auth.onAuthStateChanged(function(user) {
  app.ports.userAuthChange.send(user);
});

app.ports.purgeThoughts.subscribe(function(userId) {
  db.collection('users')
    .doc(userId)
    .update({
      color: firebase.firestore.FieldValue.delete(),
      thoughts: firebase.firestore.FieldValue.delete(),
    });
});

app.ports.saveColorChoice.subscribe(function({ userId, color }) {
  db.collection('users')
    .doc(userId)
    .update({ color });
});

app.ports.logout.subscribe(function() {
  firebase.auth().signOut();
});

app.ports.requestThoughts.subscribe(function(userId) {
  db.collection('users')
    .doc(userId)
    .get()
    .then(function(doc) {
      if (doc.exists) {
        const { color, thoughts } = doc.data();

        app.ports.receivedColor.send(color);
        app.ports.receivedThoughts.send(thoughts.reverse());
      }
    });
});

app.ports.writeThought.subscribe(function({ userId, thought }) {
  db.collection('users')
    .doc(userId)
    .update({
      thoughts: firebase.firestore.FieldValue.arrayUnion(thought),
    });
});
