var Canvas = require('canvas')
  , Image  = Canvas.Image
  , QRCode = require('jsqrcode')(Canvas);

var image = new Image();

image.onload = function() {
  var result;
  try {
    result = QRCode.decode(image);
    console.log(result);
  } catch(e) {
    console.log('unable to read qr code.' + e);
  }
}

var filename = __dirname + '/qr.png';
if (process.argv.length > 2) {
    filename = process.argv[2]
}
image.src = filename;
