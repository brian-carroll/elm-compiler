cFile=$1
stack runghc -- DumpAst.hs
./DumpAst $cFile
