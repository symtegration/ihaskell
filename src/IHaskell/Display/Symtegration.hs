{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: IHaskell.Display.Symtegration
-- Description: IHaskell extension for making the use of Symtegration more seamless.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- This is an [IHaskell](https://github.com/IHaskell/IHaskell) extension to make
-- the use of [Symtegration](https://symtegration.dev/) more seamless.
-- In particular, it will render 'Expression' or 'Maybe' 'Expression' values
-- as mathematical expressions.
--
-- For example, the following integrates to an 'Expression' in a 'Just' value,
-- which is rendered as a mathematical expression.
--
-- ![Example of integral rendered by IHaskell](docs/integration-example.png)
--
-- If an integral cannot be derived, i.e., 'integrate' returns 'Nothing',
-- then the bottom symbol \(\bot\) will be displayed.
module IHaskell.Display.Symtegration () where

import Data.Text (unpack)
import IHaskell.Display
import Symtegration

-- | Typesets 'Expression' values as mathematical expressions.
instance IHaskellDisplay Expression where
  display expr = pure $ Display $ latexDisplayData expr <> haskellDisplayData expr

-- | Typesets an 'Expression' even if it is a 'Maybe' value.
instance IHaskellDisplay (Maybe Expression) where
  display (Just e) = display e
  display Nothing =
    pure $
      Display
        [ latex "\\[\\bot\\]",
          markdown "```haskell\nNothing\n```\n",
          plain "Nothing"
        ]

-- | Returns display data for the expression as LaTeX.
latexDisplayData :: Expression -> [DisplayData]
latexDisplayData expr = [latex $ "\\[" <> unpack (toLaTeX expr) <> "\\]"]

-- | Returns display data for the expression as Haskell code.
haskellDisplayData :: Expression -> [DisplayData]
haskellDisplayData expr =
  [ markdown $ "```haskell\n" <> haskellString <> "\n```\n",
    plain haskellString
  ]
  where
    haskellString = unpack $ toHaskell expr
