{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TypeFamilies       #-}

module Dhallix (
    -- * Dhall to Nix
      dhallToNix
      , context

    -- * Exceptions
    , CompileError(..)
    ) where

import Control.Applicative (empty)
import Control.Exception (Exception)
import Data.Foldable (fold, toList)
import Data.Fix (Fix(..))
import Data.Typeable (Typeable)
import Dhall.Core (Chunks(..), Const(..), Expr(..), Var(..))
import qualified Dhall.Context
import Dhall.TypeCheck (X(..))
import Nix.Atoms (NAtom(..))
import Nix.Expr
    ( Antiquoted(..)
    , Binding(..)
    , NBinaryOp(..)
    , NExprF(..)
    , NKeyName(..)
    , NString(..)
    , Params(..)
    )
import Data.Traversable (for)
import Data.String (fromString)

import qualified Data.HashMap.Strict.InsOrd
import qualified Data.Text
import qualified Dhall.Core
import qualified NeatInterpolation
import qualified Nix
import qualified Data.HashMap.Strict.InsOrd as Map

{-| This is the exception type for all possible errors that might arise when
    translating the Dhall syntax tree to the Nix syntax tree
-}
data CompileError
    = CannotReferenceShadowedVariable Var
    -- ^ Nix does not provide a way to reference a shadowed variable
    | UnexpectedConstructorsKeyword
    -- ^ The @constructors@ keyword is not yet supported
    deriving (Typeable)

instance Show CompileError where
    show (CannotReferenceShadowedVariable v) =
        Data.Text.unpack [NeatInterpolation.text|
$_ERROR: Cannot reference shadowed variable

Explanation: Whenever you introduce two variables of the same name, the latter
variable takes precedence:


                                  This ❰x❱ ...
                                  ⇩
    ┌───────────────────────────────┐
    │ λ(x : Text) → λ(x : Text) → x │
    └───────────────────────────────┘
                      ⇧
                      ... refers to this ❰x❱


The former variable is "shadowed":


    ┌───────────────────────────────┐
    │ λ(x : Text) → λ(x : Text) → x │
    └───────────────────────────────┘
        ⇧
        This ❰x❱ is shadowed


... and Dhall lets you reference shadowed variables using the ❰@❱ notation:


                                  This ❰x❱ ...
                                  ⇩
    ┌─────────────────────────────────┐
    │ λ(x : Text) → λ(x : Text) → x@1 │
    └─────────────────────────────────┘
        ⇧
        ... now refers to this ❰x❱


However, the Nix language does not let you reference shadowed variables and
there is nothing analogous to ❰@❱ in Nix

Your code contains the following expression:

↳ $txt

... which references a shadowed variable and therefore cannot be translated to
Nix
|]
      where
        txt = Dhall.Core.pretty v

    show UnexpectedConstructorsKeyword =
        Data.Text.unpack [NeatInterpolation.text|
$_ERROR: Unexpected ❰constructors❱ keyword

Explanation: The dhallToNix Haskell API function has a precondition that the
Dhall expression to translate to Nix must have already been type-checked.  This
precondition ensures that the normalized expression will have no remaining
❰constructors❱ keywords.

However, the dhallToNix Haskell API function was called with a Dhall expression
that still had a ❰constructors❱ keyword, which indicates that the expression had
not yet been type-checked.
|]

_ERROR :: Data.Text.Text
_ERROR = "\ESC[1;31mError\ESC[0m"

instance Exception CompileError

{-| Convert a Dhall expression to the equivalent Nix expression

>>> :set -XOverloadedStrings
>>> dhallToNix (Lam "x" Natural (Lam "y" Natural (NaturalPlus "x" "y")))
Right (NAbs (Param "x") (NAbs (Param "y") (NBinary NPlus (NSym "x") (NSym "y"))))
>>> fmap Nix.Pretty.prettyNix it
Right x: y: x + y

    Precondition: You must first type-check the Dhall expression before passing
    the expression to `dhallToNix`
-}
dhallToNix :: Expr s X -> Either CompileError (Fix NExprF)
dhallToNix e = loop (Dhall.Core.normalize e)
  where
    outputHashBindings (RecordLit args) = do
        let
          mode =
            case Map.lookup "mode" args of
                Just (UnionLit "Flat" _ _) -> Fix (NStr "flat")
                Just (UnionLit "Recursive" _ _) -> Fix (NStr "recursive")
        hash <- case Map.lookup "hash" args of Just a -> loop a
        let
          algorithm =
            case Map.lookup "algorithm" args of
                Just (UnionLit "SHA256" _ _) -> Fix (NStr "sha256")
        return
          [NamedVar ["outputHashMode"] mode Nix.nullPos
          ,NamedVar ["outputHash"] hash Nix.nullPos
          ,NamedVar ["outputHashAlgo"] algorithm Nix.nullPos
          ]
    loop :: Expr s X -> Either CompileError (Fix NExprF)
    loop (App (Var "derivation") (RecordLit args)) = do
        args <- do
            env <-
              case Map.lookup "environment" args of
                Just (ListLit _ xs) ->
                  for (toList xs) $ \(RecordLit x) -> do
                    name <- case Map.lookup "name" x of Just (TextLit (Chunks [] t)) -> return t
                    value <-
                      case Map.lookup "value" x of
                        Just (UnionLit "Bool" b _) -> loop b
                        Just (UnionLit "Text" t _) -> loop t
                    return (NamedVar [fromString (Data.Text.unpack name)] value Nix.nullPos)
            bargs <-
              case Map.lookup "args" args of
                Just a -> loop a
            name <-
              case Map.lookup "name" args of
                Just a -> loop a
            outputs <-
              case Map.lookup "outputs" args of
                Just a -> loop a
            builder <- do
                case Map.lookup "builder" args of
                    Just (UnionLit "Builtin" (UnionLit "Fetch-Url" _ _) _) ->
                        return (Fix (NStr "builtin:fetchurl"))
                    Just (UnionLit "Exe" str _) -> loop str
            let
              system =
                case Map.lookup "system" args of
                    Just (UnionLit "builtin" _ _) -> NStr "builtin"
                    Just (UnionLit "x86_64-linux" _ _) -> NStr "x86_64-linux"
            moutputHash <-
              case Map.lookup "output-hash" args of
                  Just (OptionalLit _t e) -> traverse outputHashBindings e
            return $ NSet $
                 NamedVar ["args"] bargs Nix.nullPos
               : NamedVar ["name"] name Nix.nullPos
               : NamedVar ["outputs"] outputs Nix.nullPos
               : NamedVar ["builder"] builder Nix.nullPos
               : NamedVar ["system"] (Fix system) Nix.nullPos
               : (fold moutputHash ++ env)
        return (Fix (NBinary NApp (Fix (NSym "derivation")) (Fix args)))
    loop (Const _) = return (Fix (NSet []))
    loop (Var (V a 0)) = return (Fix (NSym a))
    loop (Var  a     ) = Left (CannotReferenceShadowedVariable a)
    loop (Lam a _ c) = do
        c' <- loop c
        return (Fix (NAbs (Param a) c'))
    loop (Pi _ _ _) = return (Fix (NSet []))
    loop (App a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NApp a' b'))
    loop (Let a _ c d) = do
        c' <- loop c
        d' <- loop d
        return (Fix (NLet [NamedVar [StaticKey a] c' Nix.nullPos] d'))
    loop (Annot a _) = loop a
    loop Bool = return (Fix (NSet []))
    loop (BoolLit b) = return (Fix (NConstant (NBool b)))
    loop (BoolAnd a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NAnd a' b'))
    loop (BoolOr a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NOr a' b'))
    loop (BoolEQ a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NEq a' b'))
    loop (BoolNE a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NNEq a' b'))
    loop (BoolIf a b c) = do
        a' <- loop a
        b' <- loop b
        c' <- loop c
        return (Fix (NIf a' b' c'))
    loop Natural = return (Fix (NSet []))
    loop (NaturalLit n) = return (Fix (NConstant (NInt (fromIntegral n))))
    loop NaturalFold = do
        let e0 = Fix (NBinary NMinus "n" (Fix (NConstant (NInt 1))))
        let e1 = Fix (NBinary NApp (Fix (NBinary NApp (Fix (NBinary NApp "naturalFold" e0)) "t")) "succ")
        let e2 = Fix (NBinary NApp "succ" (Fix (NBinary NApp e1 "zero")))
        let e3 = Fix (NBinary NLte "n" (Fix (NConstant (NInt 0))))
        let e4 = Fix (NAbs "succ" (Fix (NAbs "zero" (Fix (NIf e3 "zero" e2)))))
        let e5 = Fix (NAbs "n" (Fix (NAbs "t" e4)))
        return (Fix (NLet [NamedVar ["naturalFold"] e5 Nix.nullPos] "naturalFold"))
    loop NaturalBuild = do
        let e0 = Fix (NBinary NPlus "n" (Fix (NConstant (NInt 1))))
        let e1 = Fix (NBinary NApp (Fix (NBinary NApp "k" (Fix (NSet [])))) (Fix (NAbs "n" e0)))
        return (Fix (NAbs "k" (Fix (NBinary NApp e1 (Fix (NConstant (NInt 0)))))))
    loop NaturalIsZero = do
        let e0 = Fix (NBinary NEq "n" (Fix (NConstant (NInt 0))))
        return (Fix (NAbs "n" e0))
    loop NaturalEven = do
        let e0 = Fix (NBinary NMinus "n" (Fix (NConstant (NInt 2))))
        let e1 = Fix (NBinary NApp "naturalEven" e0)
        let e2 = Fix (NBinary NNEq "n" (Fix (NConstant (NInt 1))))
        let e3 = Fix (NBinary NEq "n" (Fix (NConstant (NInt 0))))
        let e4 = Fix (NBinary NOr e3 (Fix (NBinary NAnd e2 e1)))
        let e5 = NamedVar ["naturalEven"] (Fix (NAbs "n" e4)) Nix.nullPos
        let e6 = Fix (NBinary NMinus (Fix (NConstant (NInt 0))) "n")
        let e7 = Fix (NBinary NLte "n" (Fix (NConstant (NInt 0))))
        let e8 = Fix (NAbs "n" (Fix (NBinary NApp "naturalEven" (Fix (NIf e7 e6 "n")))))
        return (Fix (NLet [e5] e8))
    loop NaturalOdd = do
        let e0 = Fix (NBinary NMinus "n" (Fix (NConstant (NInt 2))))
        let e1 = Fix (NBinary NApp "naturalOdd" e0)
        let e2 = Fix (NBinary NNEq "n" (Fix (NConstant (NInt 0))))
        let e3 = Fix (NBinary NEq "n" (Fix (NConstant (NInt 1))))
        let e4 = Fix (NBinary NOr e3 (Fix (NBinary NAnd e2 e1)))
        let e5 = NamedVar ["naturalOdd"] (Fix (NAbs "n" e4)) Nix.nullPos
        let e6 = Fix (NBinary NMinus (Fix (NConstant (NInt 0))) "n")
        let e7 = Fix (NBinary NLte "n" (Fix (NConstant (NInt 0))))
        let e8 = Fix (NAbs "n" (Fix (NBinary NApp "naturalOdd" (Fix (NIf e7 e6 "n")))))
        return (Fix (NLet [e5] e8))
    loop NaturalShow = do
        return "toString"
    loop NaturalToInteger = do
        return (Fix (NAbs "n" "n"))
    loop (NaturalPlus a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NPlus a' b'))
    loop (NaturalTimes a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NMult a' b'))
    loop Integer = return (Fix (NSet []))
    loop (IntegerLit n) = return (Fix (NConstant (NInt (fromIntegral n))))
    loop IntegerShow = do
        let e0 = Fix (NBinary NApp "toString" "x")
        let e1 = Fix (NBinary NPlus (Fix (NStr "+")) e0)
        let e2 = Fix (NBinary NLte (Fix (NConstant (NInt 0))) "x")
        let e3 = Fix (NAbs "x" (Fix (NIf e2 e1 e0)))
        return e3
    loop IntegerToDouble = do
        return (Fix (NAbs "x" "x"))
    loop Double = return (Fix (NSet []))
    loop (DoubleLit n) = return (Fix (NConstant (NFloat (realToFrac n))))
    loop DoubleShow = do
        return "toString"
    loop Text = return (Fix (NSet []))
    loop (TextLit (Chunks abs_ c)) = do
        let process (a, b) = do
                b' <- loop b
                return [Plain a, Antiquoted b']
        abs' <- mapM process abs_

        let chunks = concat abs' ++ [Plain c]
        return (Fix (NStr (DoubleQuoted chunks)))
    loop (TextAppend a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NPlus a' b'))
    loop List = return (Fix (NAbs "t" (Fix (NSet []))))
    loop (ListAppend a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NConcat a' b'))
    loop (ListLit _ bs) = do
        bs' <- mapM loop (toList bs)
        return (Fix (NList bs'))
    loop ListBuild = do
        let e0 = Fix (NBinary NApp "k" (Fix (NSet [])))
        let e1 = Fix (NBinary NConcat (Fix (NList ["x"])) "xs")
        let e2 = Fix (NBinary NApp e0 (Fix (NAbs "x" (Fix (NAbs "xs" e1)))))
        let e3 = Fix (NAbs "k" (Fix (NBinary NApp e2 (Fix (NList [])))))
        return (Fix (NAbs "t" e3))
    loop ListFold = do
        let e0 = Fix (NBinary NApp "f" (Fix (NBinary NApp (Fix (NBinary NApp "cons" "y")) "ys")))
        let e1 = Fix (NAbs "f" (Fix (NAbs "y" (Fix (NAbs "ys" e0)))))
        let e2 = Fix (NBinary NApp "builtins.foldl'" e1)
        let e3 = Fix (NBinary NApp (Fix (NBinary NApp e2 (Fix (NAbs "ys" "ys")))) "xs")
        let e4 = Fix (NAbs "xs" (Fix (NAbs "t" (Fix (NAbs "cons" e3)))))
        return (Fix (NAbs "t" e4))
    loop ListLength = return (Fix (NAbs "t" "builtins.length"))
    loop ListHead = do
        let e0 = Fix (NBinary NApp "builtins.head" "xs")
        let e1 = Fix (NBinary NEq "xs" (Fix (NList [])))
        let e2 = Fix (NAbs "xs" (Fix (NIf e1 (Fix (NConstant NNull)) e0)))
        return (Fix (NAbs "t" e2))
    loop ListLast = do
        let e0 = Fix (NBinary NApp "builtins.length" "xs")
        let e1 = Fix (NBinary NMinus e0 (Fix (NConstant (NInt 1))))
        let e2 = Fix (NBinary NApp (Fix (NBinary NApp "builtins.elemAt" "xs")) e1)
        let e3 = Fix (NBinary NEq "xs" (Fix (NList [])))
        let e4 = Fix (NAbs "xs" (Fix (NIf e3 (Fix (NConstant NNull)) e2)))
        return (Fix (NAbs "t" e4))
    loop ListIndexed = do
        let e0 = Fix (NBinary NApp "builtins.length" "xs")
        let e1 = Fix (NBinary NApp (Fix (NBinary NApp "builtins.elemAt" "xs")) "i")
        let e2 =
                [ NamedVar ["index"] "i" Nix.nullPos
                , NamedVar ["value"] e1  Nix.nullPos
                ]
        let e3 = Fix (NBinary NApp "builtins.genList" (Fix (NAbs "i" (Fix (NSet e2)))))
        return (Fix (NAbs "t" (Fix (NAbs "xs" (Fix (NBinary NApp e3 e0))))))
    loop ListReverse = do
        let e0 = Fix (NBinary NMinus "n" "i")
        let e1 = Fix (NBinary NMinus e0 (Fix (NConstant (NInt 1))))
        let e2 = Fix (NBinary NApp (Fix (NBinary NApp "builtins.elemAt" "xs")) e1)
        let e3 = Fix (NBinary NApp "builtins.genList" (Fix (NAbs "i" e2)))
        let e4 = Fix (NBinary NApp e3 "n")
        let e5 = Fix (NBinary NApp "builtins.length" "xs")
        let e6 = Fix (NAbs "xs" (Fix (NLet [NamedVar ["n"] e5 Nix.nullPos] e4)))
        return (Fix (NAbs "t" e6))
    loop Optional = return (Fix (NAbs "t" (Fix (NSet []))))
    loop (OptionalLit _ b) =
        case b of
            Nothing -> return (Fix (NConstant NNull))
            Just c  -> loop c
    loop OptionalFold = do
        let e0 = Fix (NBinary NEq "x" (Fix (NConstant NNull)))
        let e1 = Fix (NIf e0 "nothing" (Fix (NBinary NApp "just" "x")))
        let e2 = Fix (NAbs "t" (Fix (NAbs "just" (Fix (NAbs "nothing" e1)))))
        return (Fix (NAbs "t" (Fix (NAbs "x" e2))))
    loop OptionalBuild = do
        let e0 = Pi "nothing" "optional" "optional"
        let e1 = Pi "just" (Pi "_" "a" "optional") e0
        let e2 = Pi "optional" (Const Type) e1
        let e3 = OptionalLit "a" empty
        let e4 = Lam "x" "a" (OptionalLit "a" (pure "x"))
        let e5 = App (App (App "f" (App Optional "a")) e4) e3
        loop (Lam "a" (Const Type) (Lam "f" e2 e5))
    loop (Record _) = return (Fix (NSet []))
    loop (RecordLit a) = do
        a' <- traverse loop a
        let a'' = do
                (k, v) <- Data.HashMap.Strict.InsOrd.toList a'
                return (NamedVar [StaticKey k] v Nix.nullPos)
        return (Fix (NSet a''))
    loop (Union _) = return (Fix (NSet []))
    loop (UnionLit k v kts) = do
        v' <- loop v
        let e0 = do
                k' <- k : Data.HashMap.Strict.InsOrd.keys kts
                return (k', Nothing)
        let e2 = Fix (NBinary NApp (Fix (NSym k)) v')
        return (Fix (NAbs (ParamSet e0 False Nothing) e2))
    loop (Combine a b) = do
        a' <- loop a
        b' <- loop b
        let e0 = Fix (NBinary NApp (Fix (NBinary NApp "map" "toKeyVals")) "ks")
        let e1 = Fix (NBinary NApp "builtins.concatLists" e0)
        let e2 = Fix (NBinary NApp "builtins.listToAttrs" e1)

        let defL = Fix (NBinary NApp (Fix (NBinary NApp "builtins.hasAttr" "k")) "kvsL")
        let defR = Fix (NBinary NApp (Fix (NBinary NApp "builtins.hasAttr" "k")) "kvsR")
        let valL = Fix (NBinary NApp (Fix (NBinary NApp "builtins.getAttr" "k")) "kvsL")
        let valR = Fix (NBinary NApp (Fix (NBinary NApp "builtins.getAttr" "k")) "kvsR")

        let empty_ = Fix (NList [])
        let toNameValue v =
                let bindings =
                        [ NamedVar ["name" ] "k" Nix.nullPos
                        , NamedVar ["value"] v   Nix.nullPos
                        ]
                in  Fix (NList [Fix (NSet bindings)])

        let e3 = Fix (NBinary NApp (Fix (NBinary NApp "combine" valL)) valR)
        let e4 = Fix (NBinary NApp "builtins.isAttrs" valL)
        let e5 = Fix (NBinary NApp "builtins.isAttrs" valR)
        let e6 = Fix (NBinary NAnd e4 e5)
        let e7 = Fix (NIf e6 (toNameValue e3) (toNameValue valR))
        let e8 = Fix (NIf defR e7 (toNameValue valL))
        let e9 = Fix (NIf defR (toNameValue valR) empty_)
        let toKeyVals = Fix (NAbs "k" (Fix (NIf defL e8 e9)))

        let ksL = Fix (NBinary NApp "builtins.attrNames" "kvsL")
        let ksR = Fix (NBinary NApp "builtins.attrNames" "kvsR")
        let ks  = Fix (NBinary NConcat ksL ksR)

        let e10 =
                [ NamedVar ["ks"       ] ks        Nix.nullPos
                , NamedVar ["toKeyVals"] toKeyVals Nix.nullPos
                ]
        let combine = Fix (NAbs "kvsL" (Fix (NAbs "kvsR" (Fix (NLet e10 e2)))))

        let e11 = Fix (NBinary NApp (Fix (NBinary NApp "combine" a')) b')
        return (Fix (NLet [NamedVar ["combine"] combine Nix.nullPos] e11))
    loop (CombineTypes _ _) = return (Fix (NSet []))
    loop (Merge a b _) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NApp b' a'))
    loop (Constructors _) = do
        Left UnexpectedConstructorsKeyword
    loop (Prefer a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NUpdate a' b'))
    loop (Field a b) = do
        a' <- loop a
        return (Fix (NSelect a' [StaticKey b] Nothing))
    loop (Project a b) = do
        a' <- loop a
        let b' = fmap StaticKey (toList b)
        return (Fix (NSet [Inherit (Just a') b' Nix.nullPos]))
    loop (ImportAlt a _) = loop a
    loop (Note _ b) = loop b
    loop (Embed (X x)) = x


context :: Dhall.Context.Context (Expr s X)
context =
  Dhall.Context.insert
    "derivation"
    (Pi "_" (Record (Map.fromList [("args", List `App` Text)
                                  ,("builder", Union (Map.fromList [("Builtin", Union (Map.fromList [("Fetch-Url", Record mempty)]))
                                                                   ,("Exe", Text)]))
                                  ,("environment", List `App` Record (Map.fromList [("name", Text), ("value", Union (Map.fromList [("Bool", Bool), ("Text", Text)]))]))
                                  ,("name", Text)
                                  ,("output-hash", Optional `App` Record (Map.fromList [("algorithm", Union (Map.fromList [("SHA256", Record mempty)]))
                                                                                       ,("hash", Text)
                                                                                       ,("mode", Union (Map.fromList [("Flat", Record mempty)
                                                                                                                     ,("Recursive", Record mempty)
                                                                                                                     ]))
                                                                                       ]))
                                  ,("outputs", List `App` Text)
                                  ,("system", Union (Map.fromList [("builtin", Record mempty), ("x86_64-linux", Record mempty)]))
                                  ])) Text)
    Dhall.Context.empty
  
