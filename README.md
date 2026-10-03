```
1. VCD File (text)
   │
   ├─ Parser.hs
   │
   ↓
2. SimulationCommand list
   │  (raw parsed events: SimTime, ScalarChange, VectorChange, etc.)
   │
   ├─ buildWaveValueMap (Waveform.hs)
   │
   ↓
3. WaveValueMap (sparse)
   │  HashMap IdentifierCode [(SimulationTime, WaveValue)]
   │  Example: {"clk" → [(0, V0), (5, V1), (10, V0)]}
   │  ✓ Efficient storage - only stores changes
   │  ✗ Can't directly render - needs interpolation
   │
   ├─ resampleWaveform (Resample.hs)
   │   └─ uses: sample helper (finds value at time t)
   │
   ↓
4. DenseWaveValueMap (dense)
   │  HashMap IdentifierCode [WaveValue]
   │  Example: {"clk" → [V0, V0, V0, V0, V0, V1, V1, V1, V1, V1, V0, ...]}
   │  ✓ Value for every time step [minTime..maxTime]
   │  ✓ Ready for visualization
   │
   ├─ sampleWaveforms → toRenderTicks (Transitions.hs)
   │
   ↓
5. RenderTickMap (renderable)
   │  HashMap IdentifierCode [RenderTick]
   │  Example: {"clk" → [LogicLow, LogicLow, LogicRising, LogicHigh, ...]}
   │  ✓ Detects transitions (rising/falling edges)
   │  ✓ Handles multi-bit vectors (left/right arrows)
   │
   ├─ renderWaveTicks (Render.hs)
   │
   ↓
6. ASCII String with ANSI colors
   clk: ___╱‾‾‾‾‾╲___
```

