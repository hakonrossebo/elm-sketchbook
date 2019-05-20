import './main.css';
import { Elm } from './src/Main.elm';

var flags = {
  api: "",
}


Elm.Main.init({
  node: document.querySelector('main'),
  flags: flags,
});

