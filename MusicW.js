/*
 * @class
 * @property {string | File | Blob} this.source
 * @property {('created' | 'loading' | 'decoding' | 'decoded' | 'error')} this.status
 * @property {?string} this.error
 * @property {?AudioBuffer} this.buffer
 *
 * @param {string | File | Blob} source - the source to load the audio buffer from
 * @param {WebAudioContext} ctx - the web audio context to use for decoding
 */
function Buffer(source, ctx) {
  this.source = source;
  this.ctx = ctx;

  this.status = 'created';
  this.error = null;
  this.buffer = null;
}

Buffer.prototype._changeStatus = function (onStatusChange, status, error, buffer) {
  this.status = status;
  this.error = error != null ? error : null; // If undefined set to null (not undefined)
  this.buffer = buffer != null ? buffer : null;
  onStatusChange(this);
}

/**
 * @param {function(buffer: Buffer): void} onStatusChange
 */
Buffer.prototype.startLoadingAndDecoding = function(onStatusChange) {
  var closure = this;
  var sourceIsUrl = typeof this.source === 'string';

  this._changeStatus(onStatusChange, 'loading');

  var loader = sourceIsUrl ? new XMLHttpRequest() : new FileReader();

  loader.onerror = function () {
    var message = 'Error loading buffer: ' + (sourceIsUrl ? loader.statusText : loader.error.message);
    closure._changeStatus(onStatusChange, 'error', message);
  }
  loader.onabort = this._changeStatus.bind(this, onStatusChange, 'abort', 'Read aborted', null);

  loader.onload = function() {
    closure._changeStatus(onStatusChange, 'decoding');

    var arrayBuffer = sourceIsUrl ? loader.response : loader.result;
    closure.ctx.decodeAudioData(arrayBuffer,
      function success(audioBuffer) {
        closure._changeStatus(onStatusChange, 'decoded', null, audioBuffer);
      },
      function failure(error) {
        closure._changeStatus(onStatusChange, 'error', 'Error decoding buffer: ' + error.message);
      }
    );
  }

  // Start reading.
  if (sourceIsUrl) {
    loader.responseType = 'arraybuffer';
    loader.open('GET', this.source, true);
    loader.send();
  } else {
    loader.readAsArrayBuffer(this.source);
  }
}

function startLoadingAndDecodingMultiple(list, cb){
  var closure = []
  var loaded = []
  for (i in list){

    console.log(i)
    console.log(list[i])
    console.log(loaded)
    list[i].startLoadingAndDecoding((x)=>{
      console.log("x is : " + x)
      console.log("x.status is : " + x.status)

      closure.push(x)
      var haveLoaded = true
      for (j in closure){
        haveLoaded = (closure[j].status == "error" || closure[j].status == "abort" || closure[j].status == "decoded")&&haveLoaded;
        if(haveLoaded){
          loaded.push(closure[j])
        }
      }
      console.log("cb test case:  haveloaded - "+(haveLoaded) +"  loadedlength - "+loaded.length);
      if (haveLoaded && loaded.length >= list.length  ){
        console.log("cb being called...")
        cb(closure)
      }
    });
  }
  console.log("closure:  "+closure)
}
