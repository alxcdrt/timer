const sounds = {
    end: new Howl({ src: ['/assets/sounds/alarm.mp3']})
}

function ring(key) {
    const sound = sounds[key]
    if (sound != null) {
        sound.play();
    }
}