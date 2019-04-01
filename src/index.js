import { Elm } from './Main.elm';

const localStorageKey = 'wolfadex_idea-stream';
let priorThoughts;

try {
  priorThoughts = JSON.parse(localStorage.getItem(localStorageKey));
} catch (error) {
  // Placeholder
}

const app = Elm.Main.init({
  flags: {
    priorThoughts,
    now: new Date().getTime(),
    width: window.outerWidth,
    height: window.outerHeight,
  },
});

app.ports.writeThoughts.subscribe((thoughts) => {
  localStorage.setItem(localStorageKey, JSON.stringify(thoughts));
});

app.ports.purgeThoughts.subscribe(() => {
  localStorage.removeItem(localStorageKey);
});
