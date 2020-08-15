import '../css/ckalculator.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});


app.ports.sendMessage.subscribe(function(ip) {
// WebSocket
console.log(ip);
let ws = new WebSocket("ws://"+ip+":8081/ckar_consume")
console.log(ws);
ws.onmessage = function(event) {
  scrollDisplays(event.data);
	app.ports.messageReceiver.send(event.data);
};

});


function scrollDisplays(data) {
  let d1 = document.querySelector('#display1');
  let d2 = document.querySelector('#display2');
  let d3 = document.querySelector('#display3');
  let d4 = document.querySelector('#display4');

  let obj = JSON.parse(data);
  let display = obj.display;

  [d1, d2, d3, d4].forEach((d)=>{ d.scrollTop = d.scrollHeight})

}



// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
