Unboxed Ints
------------

This is test I used while developing unboxed ints.

Unboxing means that Int, Char and Bool do not need to be full data structures, they can just be numbers as you'd expect. (It could be extended to Unit and custom types with no parameters.)

The hardest part of the feature is dealing with the SuperTypes `number` and `comparable`, since they contain both boxed and unboxed types.
If we see `doAdd x y = x + y`, do we generate an 'add' instruction for int or float, or branches for both? It matters for non-JS targets.
Detecting types at runtime loses some of the perf advantage of unboxing (and requires shrinking Ints to 31 bits so we can distinguish them from pointers)
To resolve this ambiguity, we need more type info to get through to code gen.

This paper explains the Haskell implementation of type classes
https://static.aminer.org/pdf/PDF/000/542/781/implementing_type_classes.pdf

They insert extra "placeholder" AST nodes so that for each _usage_ of a function can get a different concrete type. Placeholders are inserted before type inference, so they have to be conservative and insert more than they end up using.

It was hard to get the Elm type solver to infer a unique type for each usage of a function. So instead I inserted placeholders at every call site - for every argument, and for the function itself. After type inference, during optimization, I can prune the placeholders that end up containing no useful info.

- Original.elm :             A sample Elm program showing some use cases with the `number` SuperType
- PlaceholdersHaskell.elm :  Placeholders manually inserted as Haskell does it
- PlaceholdersCalls.elm :    Placeholders manually inserted as I decided to do it for Elm

I just wrote the Placeholder*.elm files to help me to get the implementation clear in my mind.
