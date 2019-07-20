# Fields

Approaches

1. Generate the fieldset along with the Record constructor

- Difficulty: knowing we're in the constructor
- Need to maintain an AST stack, which is a pain if done everywhere

2. Maintain a set/map of fieldsets, generate them all together at the top

- All records have to start from a full literal. Elm has no way to add fields that weren't already there.
- Whenever you generate a Record literal
  - Get the List of its fields and wrap it in a `Set.singleton`
  - Take the union of that set with the `Set [Name]` in the generator state
  - `union` is efficient, doesn't do an update if already there

# Constructor tags

Approaches

1. Global enum at the top

- no need for globally unique names, don't care about name clashes between separate types since Elm compiler keeps them apart

2. Constants next to constructor function, set equal to `index` from compiler

- downside: need globally unique names
- definition of ctor ID is next to constructor function. Meh.

3. Just use integers like JS prod mode

- downside: lack of readability with no improvement over C enum
