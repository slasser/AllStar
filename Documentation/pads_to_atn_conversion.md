### Pads to ATN Conversion

#### I. Overview of evaluation process for Pads descriptions

The following example walks through the sequence of actions performed when this
Pads description is evaluated:

```haskell 
[pads| type MyChar = Char |]
```

The quoted string is parsed by parsePadsDecls (defined in Parser). The
resulting value has type PadsDecl (defined in Syntax).

```haskell 
*Main> let myCharPadsDecl = parsePadsString typeDecl "type MyChar = Char" --utility function for demo purposes
*Main> 
*Main> :t myCharPadsDecl 
myCharPadsDecl :: PadsDecl
```

The value contains an AST representation of the MyChar type:

```haskell 
*Main> myCharPadsDecl 
PadsDeclType "MyChar" [] Nothing (PTycon ["Char"])  -- (PTycon ["Char"]) is the AST
```

The make_pads_declarations' function (defined in CodeGen) converts the Pads
declaration into a list of Haskell declaration ASTs wrapped in the Q monad:

```haskell 
*Main> let myCharDecls = make_pads_declarations' (const $ return []) [myCharPadsDecl]
*Main> 
*Main> :t myCharDecls 
myCharDecls :: Q [Dec]
```

Running the Q monad produces the actual Haskell declaration ASTs:

```haskell 
*Main> runQ myCharDecls 
[TySynD MyChar [] (ConT Char),TySynD MyChar_md [] (ConT Char_md),FunD myChar_parseM [Clause [] (NormalB (VarE char_parseM)) []],FunD myChar_parseS [Clause [] (NormalB (AppE (VarE Language.Pads.PadsParser.parseStringInput) (VarE myChar_parseM))) []],FunD myChar_printFL [Clause [TupP [VarP rep,VarP md]] (NormalB (AppE (VarE char_printFL) (TupE [VarE rep,VarE md]))) []],FunD myChar_def [Clause [] (NormalB (VarE char_def)) []],SigD myChar_printFL (ForallT [] [] (AppT (ConT Language.Pads.PadsPrinter.PadsPrinter) (AppT (AppT (TupleT 2) (ConT MyChar)) (ConT MyChar_md)))),SigD myChar_def (ForallT [] [] (ConT MyChar))]
```

These ASTs are then evaluated to introduce new bindings into the environment, such as the following function:

```haskell
*Main> :t myChar_parseS
myChar_parseS :: String -> ((Char, Base_md), String)
*Main> 
*Main> myChar_parseS "ab"
(('a',Base_md {numErrors = 0, errInfo = Nothing}),"b")
```

Here is the abstract syntax for the myChar_parseM and myChar_parseS functions that were generated:

```haskell
FunD myChar_parseM [Clause [] (NormalB (VarE char_parseM)) []]

FunD myChar_parseS [Clause [] (NormalB (AppE (VarE Language.Pads.PadsParser.parseStringInput) (VarE myChar_parseM))) []]
```

The first definition says that myChar_parseM is a function whose body contains
a single expression: the name of the built-in char_parseM function. The
myChar_parseS function is a partial application of PadsParser.parseStringInput
to myChar_parseM; the resulting function uses myChar_parseM to parse string
input.

#### II. Generating parse functions from Pads descriptions, in detail

This section describes how the Haskell AST for myChar_parseM is created from the MyChar Pads declaration.

make_pads_declarations' calls genPadsDecl on the Pads declaration:

```haskell
make_pads_declarations' :: Derivation -> [PadsDecl] -> Q [Dec]
make_pads_declarations' derivation ds = fmap concat (mapM (genPadsDecl derivation) ds)
```

genPadsDecl calls genPadsParseM, passing the Pads declaration's fields as 
arguments:

```haskell
genPadsDecl derivation (PadsDeclType name args pat padsTy) = do
  let typeDecs = mkTyRepMDDecl name args padsTy
  parseM  <- genPadsParseM name args pat padsTy
  parseS  <- genPadsParseS name args pat
  printFL <- genPadsPrintFL name args pat padsTy
  def <- genPadsDef name args pat padsTy
  let sigs = mkPadsSignature name args (fmap patType pat)
  return $ typeDecs ++ parseM ++ parseS ++ printFL ++ def ++ sigs

*Main> let myCharDecls = genPadsDecl (const $ return []) myCharPadsDecl 
*Main> 
*Main> :t myCharDecls
myCharDecls :: Q [Dec]
```

genPadsParseM uses the PadsDecl's type AST to create the parse function's body.
It uses the PadsDecl's remaining fields to create the rest of the function.

```haskell
genPadsParseM :: UString -> [LString] -> Maybe Pat -> PadsTy -> Q [Dec]
genPadsParseM name args patM padsTy = do 
  body  <- genParseTy padsTy
  return [mkParserFunction name args patM body]

*Main> let myChar_parseM = genPadsParseM "MyChar" [] Nothing (PTycon ["Char"])
*Main> 
*Main> :t myChar_parseM
myChar_parseM :: Q [Dec]
*Main> 
*Main> runQ myChar_parseM
[FunD myChar_parseM [Clause [] (NormalB (VarE char_parseM)) []]]
```

genParseTy does case analysis on the PadsDecl's AST to produce the
parse function's body. Since the AST for the MyChar type is (PTycon ["Char"]), 
genParseTy choose the PTycon branch for this example.

```haskell
genParseTy :: PadsTy -> Q Exp
genParseTy pty = case pty of
    PConstrain pat ty exp   -> genParseConstrain (return pat) ty (return exp)
    PTransform src dest exp -> genParseTyTrans src dest (return exp)
    PList ty sep term       -> genParseList ty sep term
    PPartition ty exp       -> genParsePartition ty exp
    PValue exp ty           -> genParseValue exp
    PApp tys argE           -> genParseTyApp tys argE
    PTuple tys              -> genParseTuple tys
    PExpression exp         -> genParseExp exp
    PTycon c                -> return $ mkParseTycon c
    PTyvar v                -> return $ mkParseTyvar v

*Main> let myChar_parseM_body = genParseTy (PTycon ["Char"])
*Main> 
*Main> :t myChar_parseM_body
myChar_parseM_body :: Q Exp
*Main> 
*Main> runQ myChar_parseM_body 
VarE char_parseM
```

mkParseTycon creates a Template Haskell VarE expression with the result of
calling mkTyParserQName on the type constructor's name.

```haskell
mkParseTycon :: QString -> Exp
mkParseTycon ["EOF"] = VarE 'eof_parseM
mkParseTycon ["EOR"] = VarE 'eor_parseM
mkParseTycon c     = VarE (mkTyParserQName c)

*Main> let myChar_parseM_body' = mkParseTycon ["Char"]
*Main> 
*Main> :t myChar_parseM_body'
myChar_parseM_body' :: Exp
*Main> 
*Main> myChar_parseM_body'
VarE char_parseM
```

mkTyParserQName simply converts the type constructor to a name with "_parseM"
at the end.

```haskell
mkTyParserQName  str = mkName (appendLower str "_parseM")

*Main> let funcName = mkTyParserQName ["Char"]
*Main> 
*Main> :t funcName
funcName :: Name
*Main> 
*Main> funcName
char_parseM
```

The name that mkTyParserQName creates, char_parseM, is also the name of a function
in CoreBaseTypes.hs:

```haskell
char_parseM :: PadsParser (Char, Base_md)
char_parseM  =
  handleEOF def "Char" $
  handleEOR def "Char" $ do
    c <- takeHeadP
    returnClean c
```

This means that when myChar_parseM is applied (within myChar_parseS in the 
example below), it in turn calls char_parseM.

```haskell
*Main> myChar_parseS "ab"
(('a',Base_md {numErrors = 0, errInfo = Nothing}),"b")
```

If the type constructor name that appears in the Pads description does not
correspond to a predefined Pads function name, a compile-time error occurs.

```haskell
[pads| type MyChar = Cha |] -- typo

*Language.Pads.Testing> :l ~/projects/antlr/allstar/PadsToAtn.hs
...
~/projects/antlr/allstar/PadsToAtn.hs:27:7:
    Not in scope: ‘cha_parseM’
    Perhaps you meant one of these:
      ‘char_parseM’ (imported from Language.Pads.Padsc),
      ‘int_parseM’ (imported from Language.Pads.Padsc),
      ‘try_parseM’ (imported from Language.Pads.Padsc)
```

