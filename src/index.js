import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var flags = {
  api: "http://localhost:8181",
}


Elm.Main.init({
  node: document.getElementById('root'),
  flags: flags,
});

registerServiceWorker();
