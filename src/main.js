import { Elm } from './Main.elm'

// Initialize Elm
const app = Elm.Main.init({ node: document.getElementById('root') })

// Initialize WebAudioFont
const audioContext = new AudioContext()
const player = new WebAudioFontPlayer()

/* tones */
player.loader.decodeAfterLoading(audioContext, '_tone_0000_SBLive_sf2') // piano
player.loader.decodeAfterLoading(audioContext, '_tone_0120_SBLive_sf2') // marimba 1
player.loader.decodeAfterLoading(audioContext, '_tone_0400_SBLive_sf2') // strings
player.loader.decodeAfterLoading(audioContext, '_tone_0450_SBLive_sf2') // strings
player.loader.decodeAfterLoading(audioContext, '_tone_0730_SBLive_sf2') // flute
player.loader.decodeAfterLoading(
  audioContext,
  '_tone_0730_Aspirin_sf2_file',
) // flute

/* percussion */
player.loader.decodeAfterLoading(audioContext, '_drum_36_0_SBLive_sf2') // kick
player.loader.decodeAfterLoading(audioContext, '_drum_38_0_SBLive_sf2') // snare

// Handle unified note playback
app.ports.playNote.subscribe((data) => {
  const { webAudioFont, midi, duration, volume } = data
  const when = audioContext.currentTime

  // Look up WebAudioFont instrument by name from window object
  const instrumentSample = window[webAudioFont]

  if (instrumentSample) {
    player.queueWaveTable(
      audioContext,
      audioContext.destination,
      instrumentSample,
      when,
      midi,
      duration,
      volume,
    )
  } else {
    console.warn(`WebAudioFont instrument not found: ${webAudioFont}`)
  }
})

// Send timeSync updates using setInterval (works in background)
setInterval(() => {
  if (app.ports.timeSync) {
    app.ports.timeSync.send(audioContext.currentTime)
  }
}, 16) // ~60fps

// Wake audio context on first click
document.addEventListener(
  'click',
  () => {
    if (audioContext.state === 'suspended') audioContext.resume()
  },
  { once: true },
)