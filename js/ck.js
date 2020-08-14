
let endpoint = '192.168.178.108' //localip address 
let ws = new WebSocket("ws://"+endpoint+":8081/ckar_consume")


ws.onmessage = display

function test(e) {
  console.log(e)
}

function display(e) {
  let d1 = document.querySelector('#display1');
  let d2 = document.querySelector('#display2');
  let d3 = document.querySelector('#display3');
  let d4 = document.querySelector('#display4');

  let obj = JSON.parse(e.data);
  console.log(obj);
  let display = obj.display;
  let val = obj.payload;
  let type = obj.key

  if (display == "1") {
    d1.innerText += val;
  }

}
