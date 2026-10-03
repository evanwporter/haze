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
        toRenderTicks [LogicValue V0, LogicValue V1]
          @?= [LogicTick LogicRising, LogicTick LogicHigh, LogicTick LogicHigh, LogicTick LogicHigh],
      testCase "high to low transition" $
        toRenderTicks [LogicValue V1, LogicValue V0]
          @?= [LogicTick LogicFalling, LogicTick LogicLow, LogicTick LogicLow, LogicTick LogicLow],
      testCase "stable low" $
        toRenderTicks [LogicValue V0, LogicValue V0, LogicValue V0]
          @?= [ LogicTick LogicLow,
                LogicTick LogicLow,
                LogicTick LogicLow,
                LogicTick LogicLow,
                LogicTick LogicLow,
                LogicTick LogicLow
              ],
      testCase "stable high" $
        toRenderTicks [LogicValue V1, LogicValue V1, LogicValue V1]
          @?= [ LogicTick LogicHigh,
                LogicTick LogicHigh,
                LogicTick LogicHigh,
                LogicTick LogicHigh,
                LogicTick LogicHigh,
                LogicTick LogicHigh
              ],
      testCase "multiple transitions" $
        toRenderTicks [LogicValue V0, LogicValue V1, LogicValue V1, LogicValue V0]
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
        toRenderTicks [BinaryValue "ABC", BinaryValue "ABC", BinaryValue "ABC"]
          @?= [ VectorTick (VectorStable (Just 'A')),
                VectorTick (VectorStable (Just 'B')),
                VectorTick (VectorStable (Just 'C')),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing)
              ],
      testCase "stable single character" $
        toRenderTicks [BinaryValue "X", BinaryValue "X"]
          @?= [ VectorTick (VectorStable (Just 'X')),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing)
              ],
      testCase "stable short value runs out" $
        toRenderTicks [BinaryValue "AB", BinaryValue "AB", BinaryValue "AB", BinaryValue "AB"]
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
        toRenderTicks [BinaryValue "", BinaryValue ""]
          @?= [ VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing),
                VectorTick (VectorStable Nothing)
              ]
    ]
