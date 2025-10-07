import { Elm } from './Main.elm'

// Initialize Elm
const app = Elm.Main.init({ node: document.getElementById('root') })

// Initialize WebAudioFont
const audioContext = new AudioContext()
const player = new WebAudioFontPlayer()

// Instrument definitions: explicit mapping of URL to global variable name
const instruments = [
  // Tonal instruments
  {
    url: '0000_SBLive_sf2.js',
    globalVar: '_tone_0000_SBLive_sf2',
    label: 'piano',
  },
  {
    url: '0120_SBLive_sf2.js',
    globalVar: '_tone_0120_SBLive_sf2',
    label: 'marimba',
  },
  {
    url: '0400_SBLive_sf2.js',
    globalVar: '_tone_0400_SBLive_sf2',
    label: 'strings',
  },
  {
    url: '0450_SBLive_sf2.js',
    globalVar: '_tone_0450_SBLive_sf2',
    label: 'strings 2',
  },
  {
    url: '0730_SBLive_sf2.js',
    globalVar: '_tone_0730_SBLive_sf2',
    label: 'flute',
  },
  {
    url: '0730_Aspirin_sf2_file.js',
    globalVar: '_tone_0730_Aspirin_sf2_file',
    label: 'flute 2',
  },
  // Percussion instruments
  {
    url: '12835_0_SBLive_sf2.js',
    globalVar: '_drum_35_0_SBLive_sf2',
    label: 'bass drum 2',
  },
  {
    url: '12836_0_SBLive_sf2.js',
    globalVar: '_drum_36_0_SBLive_sf2',
    label: 'bass drum 1',
  },
  {
    url: '12838_0_SBLive_sf2.js',
    globalVar: '_drum_38_0_SBLive_sf2',
    label: 'snare drum 1',
  },
  {
    url: '12840_0_SBLive_sf2.js',
    globalVar: '_drum_40_0_SBLive_sf2',
    label: 'snare drum 2',
  },
  {
    url: '12876_0_SBLive_sf2.js',
    globalVar: '_drum_76_0_SBLive_sf2',
    label: 'high woodblock',
  },
  {
    url: '12877_0_SBLive_sf2.js',
    globalVar: '_drum_77_0_SBLive_sf2',
    label: 'low woodblock',
  },
]

const urlPrefix = 'https://surikov.github.io/webaudiofontdata/sound/'

// Load all instruments dynamically
instruments.forEach((instrument) => {
  const url = urlPrefix + instrument.url
  // console.log(`Loading instrument: ${instrument.label} from ${url}`)
  player.loader.startLoad(
    audioContext,
    url,
    instrument.globalVar,
  )
})

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
