module Depend (

  ebDepends

) where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import Data.Version
import Data.Maybe

pkgCabEbList :: [ ( String, Maybe String ) ]
pkgCabEbList = [

  ( "ghc"       , Just "dev-lang/ghc"        ) ,
  ( "Cabal"     , Just "dev-haskell/cabal"   ) ,
  ( "X11"       , Just "dev-haskell/x11"     ) ,
  ( "directory" , Nothing                    ) ,
  ( "base"      , Nothing                    ) ,
  ( "bytestring", Nothing                    ) ,
  ( "containers", Nothing                    ) ,
  ( "array"     , Nothing                    ) ,
  ( "unix"      , Nothing                    ) ,
  ( "ghc-prim"  , Nothing                    ) ,
  ( "filepath"  , Nothing                    ) ,
  ( "old-locale", Nothing                    ) ,
  ( "random"    , Nothing                    ) ,
  ( "X11-xft"   , Just "dev-haskell/x11-xft" ) ,
  ( "process"   , Nothing                    ) ,
  ( "HUnit"     , Just "dev-haskell/hunit"   ) ,
  ( "old-time"  , Nothing                    )

 ]

ebDepend :: Dependency -> Maybe String
ebDepend dp
  = let ( b, a ) = ebPkgVersion dp
     in fmap ( \n -> b ++ n ++ a ) $ ebPkgName dp
             

ebPkgName :: Dependency -> Maybe String
ebPkgName = getPkgName . getDepPkgName

getPkgName :: String -> Maybe String
getPkgName cbpkg = case lookup cbpkg pkgCabEbList of
                        Just ebpkg -> ebpkg
                        Nothing    -> Just $ "dev-haskell/" ++ cbpkg

getDepPkgName :: Dependency -> String
getDepPkgName ( Dependency ( PackageName nam ) _ ) = nam

ebPkgVersion :: Dependency -> ( String, String )
ebPkgVersion = getPkgVersion . getDepPkgVersion

getPkgVersion :: VersionRange -> ( String, String )
getPkgVersion AnyVersion = ( "", "" )
getPkgVersion ( UnionVersionRanges vr1 vr2 )
  = let ( b1, a1 ) = getPkgVersion vr1
        ( b2, a2 ) = getPkgVersion vr2
     in ( b2 ++ b1, "-" ++ a1 )
getPkgVersion ( ThisVersion v ) = ( "=", showVersion v )
getPkgVersion ( LaterVersion v ) = ( ">", showVersion v )
getPkgVersion ( EarlierVersion v ) = ( "<", showVersion v )
getPkgVersion ( IntersectVersionRanges v1 v2 ) = getPkgVersion v1

getDepPkgVersion :: Dependency -> VersionRange
getDepPkgVersion ( Dependency _ vr ) = vr


--  print $ catMaybes $ map ebDepend $ concatMap condTreeConstraints $ map snd $ condExecutables gpd
ebDepends :: GenericPackageDescription -> [ String ]
ebDepends gpd =
  let exdep = catMaybes $ map ebDepend $ concatMap condTreeConstraints $ map snd $ condExecutables gpd
      libdep = concat $ maybeToList $ fmap ( catMaybes . map ebDepend . condTreeConstraints ) $ condLibrary gpd
--  print $ map snd $ condExecutables gpd
--  print $ fmap ( map ebDepend . condTreeConstraints ) $ condLibrary gpd
   in exdep ++ libdep
