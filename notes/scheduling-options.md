You're right to question this approach. High-frequency time sync creates performance overhead and sync lag issues. Let me analyze better alternatives:

## Core Problem Analysis
We have **two timing domains** that need coordination:
- **Elm domain**: UI updates, sequence logic, user interactions (~60fps)
- **Audio domain**: Precise microsecond scheduling (~100-200Hz lookahead)

## Alternative Approaches

### 1. **Event-Driven "Pull" Model** (Better)
```javascript
// JS manages timing loop, asks Elm for notes when needed
setInterval(() => {
    const currentTime = audioContext.currentTime;
    const lookaheadTime = currentTime + 0.050; // 50ms lookahead

    // Ask Elm: "what notes should play between now and lookaheadTime?"
    app.ports.requestNotes.send({ from: currentTime, to: lookaheadTime });
}, 10); // 10ms interval

app.ports.responseNotes.subscribe((notes) => {
    notes.forEach(note => scheduleNote(note));
});
```

### 2. **Timeline Ownership Model** (Even Better)
```javascript
// Elm sends complete timeline data when it changes
app.ports.updateTimeline.subscribe((timelineData) => {
    sequenceData = timelineData; // JS owns the timeline
});

// JS does all scheduling from this data
// Elm only sends updates when user edits sequence
```

### 3. **Buffer-Based Hybrid**
```javascript
// Elm maintains future note buffer, JS consumes it
let noteBuffer = [];
app.ports.bufferNotes.subscribe((notes) => {
    noteBuffer.push(...notes);
});

// JS scheduler pulls from buffer
setInterval(() => {
    const notesToPlay = noteBuffer.filter(n => shouldPlayNow(n));
    noteBuffer = noteBuffer.filter(n => !shouldPlayNow(n));
    notesToPlay.forEach(scheduleNote);
}, 10);
```

## Analysis: Best Approach

**Timeline Ownership Model** seems optimal because:
- **Minimal events**: Elm only sends updates on user changes
- **No sync lag**: JS owns timing entirely
- **Elm controls logic**: Sequence structure, editing, transport
- **JS controls precision**: Audio scheduling stays in audio domain

The question becomes: What does Elm need to control in real-time vs. what can be pre-calculated?

For a full sequencer, Elm likely needs real-time control over:
- Tempo changes
- Pattern switching
- Live recording/editing
- Transport (play/stop/loop)

But maybe even these can be expressed as "timeline updates" rather than constant sync?

What's your take on separating the concerns this way?