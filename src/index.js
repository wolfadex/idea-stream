import { Elm } from './Main.elm';

const thoughtsStorageKey = 'wolfadex_notes-to-self_thoughts';
const colorStorageKey = 'wolfadex_notes-to-self_color';
let priorThoughts;
let colorChoice;

try {
  priorThoughts = JSON.parse(localStorage.getItem(thoughtsStorageKey));
  colorChoice = localStorage.getItem(colorStorageKey);
} catch (error) {
  // Placeholder
}

const app = Elm.Main.init({
  flags: {
    priorThoughts,
    now: new Date().getTime(),
    width: window.outerWidth,
    height: window.outerHeight,
    colorChoice,
  },
});

app.ports.writeThoughts.subscribe((thoughts) => {
  localStorage.setItem(thoughtsStorageKey, JSON.stringify(thoughts));
});

app.ports.purgeThoughts.subscribe(() => {
  localStorage.removeItem(thoughtsStorageKey);
  localStorage.removeItem(colorStorageKey);
});

app.ports.saveColorChoice.subscribe((color) => {
  localStorage.setItem(colorStorageKey, color);
});
