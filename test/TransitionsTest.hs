{-# LANGUAGE OverloadedStrings #-}

module TransitionsTest (transitionsTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Transitions
import Types
import Waveform

transitionsTests :: TestTree
transitionsTests =
  testGroup
    "Transition Tests"
    [ logicTransitionTests,
      binaryValueTests
    ]

logicTransitionTests :: TestTree
logicTransitionTests =
  testGroup
    "Logic Value Transitions"
    [ testCase "low to high transition" $
        sampleWaveform [LogicValue V0, LogicValue V1] 0
          @?= [LogicTick LogicRising, LogicTick LogicHigh, LogicTick LogicHigh, LogicTick LogicHigh],
      testCase "high to low transition" $
        sampleWaveform [LogicValue V1, LogicValue V0] 0
          @?= [LogicTick LogicFalling, LogicTick LogicLow, LogicTick LogicLow, LogicTick LogicLow],
      testCase "stable low" $
        sampleWaveform [LogicValue V0, LogicValue V0, LogicValue V0] 0
          @?= [ LogicTick LogicLow,
                LogicTick LogicLow,
                LogicTick LogicLow,
                LogicTick LogicLow,
                LogicTick LogicLow,
                LogicTick LogicLow
              ],
      testCase "stable high" $
        sampleWaveform [LogicValue V1, LogicValue V1, LogicValue V1] 0
          @?= [ LogicTick LogicHigh,
                LogicTick LogicHigh,
                LogicTick LogicHigh,
                LogicTick LogicHigh,
                LogicTick LogicHigh,
                LogicTick LogicHigh
              ],
      testCase "multiple transitions" $
        sampleWaveform [LogicValue V0, LogicValue V1, LogicValue V1, LogicValue V0] 0
          @?= [ LogicTick LogicRising,
                LogicTick LogicHigh,
                LogicTick LogicHigh,
                LogicTick LogicHigh,
                LogicTick LogicFalling,
                LogicTick LogicLow,
                LogicTick LogicLow,
                LogicTick LogicLow
              ]
    ]

binaryValueTests :: TestTree
binaryValueTests =
  testGroup
    "Binary Value Transitions"
    [ testCase "stable binary value ABC" $
        sampleWaveform [BinaryValue "ABC", BinaryValue "ABC", BinaryValue "ABC"] 0
          @?= [ VectorTick (VectorStable (Just 'A')),
                VectorTick (VectorStable (Just 'B')),
                VectorTick (VectorStable (Just 'C')),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing)
              ],
      testCase "stable single character" $
        sampleWaveform [BinaryValue "X", BinaryValue "X"] 0
          @?= [ VectorTick (VectorStable (Just 'X')),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing)
              ],
      testCase "stable short value runs out" $
        sampleWaveform [BinaryValue "AB", BinaryValue "AB", BinaryValue "AB", BinaryValue "AB"] 0
          @?= [ VectorTick (VectorStable (Just 'A')),
                VectorTick (VectorStable (Just 'B')),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing)
              ],
      testCase "empty binary value" $
        sampleWaveform [BinaryValue "", BinaryValue ""] 0
          @?= [ VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing)
              ]
    ]
