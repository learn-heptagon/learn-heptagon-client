async function loadAudio(file) {
    let audioCtx = new AudioContext();
    return await audioCtx.decodeAudioData(await file.arrayBuffer());
}

function playBuffer(buffer) {
    let audioCtx = new AudioContext();
    let source = audioCtx.createBufferSource();
    source.buffer = buffer;
    source.connect(audioCtx.destination);
    source.start();
}

var audioContext;
var gainNode;

async function startLiveAudio(lustreStep) {
    audioContext = new AudioContext();
    gainNode = audioContext.createGain();

        <!-- await audioContext.audioWorklet.addModule("./lustre-audio-processor.js"); -->

    if (!navigator.getUserMedia)
        navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia ||
        navigator.mozGetUserMedia || navigator.msGetUserMedia;

    if (navigator.getUserMedia){

        navigator.getUserMedia({audio:true},  start_microphone,
                               function(e) {
                                   alert('Error capturing audio.');
                               });

    } else { alert('getUserMedia not supported in this browser.'); }

    function start_microphone(stream){

        var microphone_stream = audioContext.createMediaStreamSource(stream);
            <!-- var processor = new AudioWorkletNode( -->
                                                       <!--     audioContext, -->
                                                       <!--     "lustre-audio-processor", -->
                                                       <!--     { processorOptions : { lustreCls: lustreCls } }); -->

        var processor = audioContext.createScriptProcessor(256, 1, 1);
        processor.addEventListener(
            "audioprocess",
            (event) => {
                let inputBuffer = event.inputBuffer;
                let outputBuffer = event.outputBuffer;

                for(let channel = 0; channel < outputBuffer.numberOfChannels; channel++) {
                    let inputData = inputBuffer.getChannelData(channel);
                    let outputData = outputBuffer.getChannelData(channel);
                    res = lustreStep(inputData);
                    for(let i = 0; i < res.length; i++) {
                        outputData[i] = res[i];
                    }
                }
            });
        microphone_stream.connect(processor);
        processor.connect(gainNode);
        gainNode.connect(audioContext.destination);
    }

}

function stopLiveAudio() {
    if(audioContext) {
        audioContext.close();
        audioContext = null;
    }
}

function setGain(volume) {
    gainNode.gain.value = volume;
    console.log("volume = ", volume);
}
