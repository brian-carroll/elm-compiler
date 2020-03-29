{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Generate.C.JsWrappers
  ( emscriptenModuleRef
  , emscriptenPostRun
  , wrapEmscriptenForElm
  , wrapEmscriptenForElmFnName
  , defineOnReady
  , executeOnReadyCallback
  )
  where


import qualified Data.ByteString.Builder as B
import Text.RawString.QQ (r)


emscriptenModuleRef :: String
emscriptenModuleRef =
  "scope['EmscriptenModule']"


emscriptenPostRun :: B.Builder -> B.Builder
emscriptenPostRun postRunJsCode =
  let
    emscripten = B.stringUtf8 emscriptenModuleRef
  in
  emscripten <> " = " <> emscripten <> " || {};\n"
  <> emscripten <> ".postRun = function postRun() {\n"
  <> postRunJsCode
  <> "}\n"


defineOnReady :: B.Builder
defineOnReady = [r|

var onReadyCallback;
scope['Elm'] = {
  onReady: function(callback) {
    onReadyCallback = callback;
  }
};

|]


executeOnReadyCallback :: B.Builder
executeOnReadyCallback = [r|

if (onReadyCallback) {
  onReadyCallback();
} else {
  throw new Error(`
    Elm.onReady has not been called.
    Elm Wasm apps are initialised differently. You have to initialize your app using a callback function.
    I'll call that function when the WebAssembly |] <> "module" <> [r| is ready.
    It's compiled asynchronously in the browser, so we have to do it this way.
    Your code could look something like this, for example:
       Elm.onReady(() => {
          var app = Elm.Main.init({
             node: document.getElementById('elm'),
             flags: Date.now()
          });
          app.ports.cache.subscribe(function(data) {
            localStorage.setItem('cache', JSON.stringify(data));
          });
       });
  `);
}

|]


wrapEmscriptenForElmFnName :: String
wrapEmscriptenForElmFnName =
  "wrapEmscriptenForElm"


wrapEmscriptenForElm :: B.Builder
wrapEmscriptenForElm = "function " <> (B.stringUtf8 wrapEmscriptenForElmFnName)
  <> [r|(wasmBuffer, wasmExports, generatedAppTypes, kernelFunctions) {
    if (!(wasmBuffer instanceof ArrayBuffer))
        throw new Error('Expected an ArrayBuffer');
    /* --------------------------------------------------
  
                 INITIALISATION & CONSTANTS
  
    -------------------------------------------------- */
    var mem32 = new Uint32Array(wasmBuffer);
    var mem16 = new Uint16Array(wasmBuffer);
    var wasmConstAddrs = (function () {
        var _a;
        var Unit = wasmExports._getUnit();
        var Nil = wasmExports._getNil();
        var True = wasmExports._getTrue();
        var False = wasmExports._getFalse();
        return _a = {
                Unit: Unit,
                Nil: Nil,
                True: True,
                False: False
            },
            _a[Unit] = _Utils_Tuple0,
            _a[Nil] = _List_Nil,
            _a[True] = true,
            _a[False] = false,
            _a;
    })();
    var appTypes = {
        ctors: arrayToEnum(generatedAppTypes.ctors),
        fields: arrayToEnum(generatedAppTypes.fields),
        fieldGroups: generatedAppTypes.fieldGroups.reduce(function (enumObj, name) {
            var addr = wasmExports._getNextFieldGroup();
            enumObj[name] = addr;
            enumObj[addr] = name;
            return enumObj;
        }, {})
    };
    function arrayToEnum(names) {
        return names.reduce(function (enumObj, name, index) {
            enumObj[name] = index;
            enumObj[index] = name;
            return enumObj;
        }, {});
    }
    var WORD = 4;
    var TAG_MASK = 0xf0000000;
    var TAG_SHIFT = 28;
    var SIZE_MASK = 0x0fffffff;
    var SIZE_SHIFT = 0;
    var NEVER_EVALUATE = 0xffff;
    var textDecoder = new TextDecoder('utf-16le');
    var identity = function (f) { return f; };
    var elmFunctionWrappers = [identity, identity, F2, F3, F4];
    var Tag;
    (function (Tag) {
        Tag[Tag["Int"] = 0] = "Int";
        Tag[Tag["Float"] = 1] = "Float";
        Tag[Tag["Char"] = 2] = "Char";
        Tag[Tag["String"] = 3] = "String";
        Tag[Tag["List"] = 4] = "List";
        Tag[Tag["Tuple2"] = 5] = "Tuple2";
        Tag[Tag["Tuple3"] = 6] = "Tuple3";
        Tag[Tag["Custom"] = 7] = "Custom";
        Tag[Tag["Record"] = 8] = "Record";
        Tag[Tag["Closure"] = 9] = "Closure";
        Tag[Tag["GcException"] = 10] = "GcException";
        Tag[Tag["GcStackEmpty"] = 11] = "GcStackEmpty";
        Tag[Tag["GcStackPush"] = 12] = "GcStackPush";
        Tag[Tag["GcStackPop"] = 13] = "GcStackPop";
        Tag[Tag["GcStackTailCall"] = 14] = "GcStackTailCall";
        Tag[Tag["Unused"] = 15] = "Unused";
    })(Tag || (Tag = {}));
    function readWasmValue(addr) {
        var index = addr >> 2;
        var header = mem32[index];
        var tag = (header & TAG_MASK) >>> TAG_SHIFT;
        var size = (header & SIZE_MASK) >>> SIZE_SHIFT;
        switch (tag) {
            case Tag.Int: {
                return mem32[index + 1];
            }
            case Tag.Float: {
                return wasmExports._readF64(addr + 2 * WORD);
            }
            case Tag.Char:
            case Tag.String: {
                var idx16 = (index + 1) << 1;
                var len16 = (size - 1) << 1;
                if (mem16[idx16 + len16 - 1] === 0)
                    len16--;
                var words16 = mem16.slice(idx16, idx16 + len16);
                var jsString = textDecoder.decode(words16);
                return tag === Tag.Char ? _Utils_chr(jsString) : jsString;
            }
            case Tag.List: {
                return addr === wasmConstAddrs.Nil
                    ? _List_Nil
                    : _List_Cons(readWasmValue(mem32[index + 1]), readWasmValue(mem32[index + 2]));
            }
            case Tag.Tuple2: {
                return _Utils_Tuple2(readWasmValue(mem32[index + 1]), readWasmValue(mem32[index + 2]));
            }
            case Tag.Tuple3: {
                return _Utils_Tuple3(readWasmValue(mem32[index + 1]), readWasmValue(mem32[index + 2]), readWasmValue(mem32[index + 3]));
            }
            case Tag.Custom: {
                var elmConst = wasmConstAddrs[addr]; // True/False/Unit
                if (elmConst !== undefined)
                    return elmConst;
                var wasmCtor = mem32[index + 1];
                var jsCtor = appTypes.ctors[wasmCtor];
                var custom = { $: jsCtor };
                var fieldNames = 'abcdefghijklmnopqrstuvwxyz';
                var nFields = size - 2;
                for (var i = 0; i < nFields; i++) {
                    var field = fieldNames[i];
                    var childAddr = mem32[index + 2 + i];
                    custom[field] = readWasmValue(childAddr);
                }
                return custom;
            }
            case Tag.Record: {
                var record = {};
                var fgIndex = mem32[index + 1] >> 2;
                var fgSize = mem32[fgIndex];
                var fields = appTypes.fields;
                for (var i = 1; i <= fgSize; i++) {
                    var fieldId = mem32[fgIndex + i];
                    var valAddr = mem32[index + 1 + i];
                    var fieldName = fields[fieldId];
                    var value = readWasmValue(valAddr);
                    record[fieldName] = value;
                }
                return record;
            }
            case Tag.Closure: {
                var idx16 = index << 1;
                var metadata = {
                    n_values: mem16[idx16 + 2],
                    max_values: mem16[idx16 + 3],
                    evaluator: mem32[index + 2],
                    argsIndex: index + 3
                };
                var isKernelThunk = metadata.max_values === NEVER_EVALUATE;
                if (isKernelThunk) {
                    return evalKernelThunk(metadata);
                }
                else {
                    return createWasmCallback(metadata);
                }
            }
            default:
                throw new Error('Tried to decode value with unsupported tag ' +
                    (Tag[tag] || '0x' + tag.toString(16)));
        }
    }
    function evalKernelThunk(metadata) {
        var n_values = metadata.n_values, evaluator = metadata.evaluator, argsIndex = metadata.argsIndex;
        var kernelValue = kernelFunctions[evaluator];
        for (var i = argsIndex; i < argsIndex + n_values; i++) {
            var arg = readWasmValue(mem32[i]);
            kernelValue = kernelValue(arg);
        }
        return kernelValue;
    }
    function createWasmCallback(metadata) {
        var n_values = metadata.n_values, max_values = metadata.max_values, evaluator = metadata.evaluator, argsIndex = metadata.argsIndex;
        var freeVars = [];
        for (var i = argsIndex; i < argsIndex + n_values; i++) {
            freeVars.push(readWasmValue(mem32[i]));
        }
        function wasmCallback() {
            var args = freeVars.slice();
            for (var i = 0; i < arguments.length; i++) {
                args.push(arguments[i]);
            }
            var n_values = args.length;
            if (n_values !== max_values) {
                console.error({ wasmCallback: wasmCallback, args: args });
                throw new Error("Trying to call a Wasm Closure with " + n_values + " args instead of " + max_values + "!");
            }
            var builder = {
                body: [(max_values << 16) | n_values, evaluator],
                jsChildren: args,
                bodyWriter: null
            };
            var addr = handleWasmWrite(function (startIndex) {
                return writeFromBuilder(startIndex, builder, Tag.Closure);
            });
            var resultAddr = wasmExports._evalClosure(addr);
            var resultValue = readWasmValue(resultAddr);
            return resultValue;
        }
        // Attach info in case we have to write this Closure back to Wasm
        wasmCallback.freeVars = freeVars;
        wasmCallback.evaluator = evaluator;
        wasmCallback.max_values = max_values;
        var arity = max_values - n_values;
        var FN = elmFunctionWrappers[arity];
        return FN(wasmCallback);
    }
    /* --------------------------------------------------
  
                  WRITE ELM VALUES TO WASM
  
    -------------------------------------------------- */
    var maxWriteIndex32;
    var maxWriteIndex16;
    var heapOverflowError = new Error('Wasm heap overflow');
    function write32(index, value) {
        if (index > maxWriteIndex32) {
            throw heapOverflowError;
        }
        mem32[index] = value;
    }
    function write16(index, value) {
        if (index > maxWriteIndex16) {
            throw heapOverflowError;
        }
        mem16[index] = value;
    }
    function handleWasmWrite(writer) {
        for (var attempts = 0; attempts < 2; attempts++) {
            try {
                var maxAddr = wasmExports._getMaxWriteAddr();
                maxWriteIndex16 = maxAddr >> 1;
                maxWriteIndex32 = maxAddr >> 2;
                var startAddr = wasmExports._getWriteAddr();
                var startIndex = startAddr >> 2;
                var result = writer(startIndex);
                wasmExports._finishWritingAt(result.nextIndex << 2);
                return result.addr;
            }
            catch (e) {
                if (e === heapOverflowError) {
                    wasmExports._collectGarbage();
                }
                else {
                    console.error(e);
                    throw e;
                }
            }
        }
        throw new Error('Failed to write to Wasm');
    }
    /**
     * Write an Elm value to the Wasm memory
     * Serialises to bytes before writing
     * May throw an error
     */
    function writeWasmValue(nextIndex, value) {
        var typeInfo = detectElmType(value);
        if (typeInfo.kind === 'constAddr') {
            return { addr: typeInfo.value, nextIndex: nextIndex };
        }
        var tag = typeInfo.value;
        var builder = wasmBuilder(tag, value);
        return writeFromBuilder(nextIndex, builder, tag);
    }
    function detectElmType(elmValue) {
        switch (typeof elmValue) {
            case 'number': {
                // There's no way to tell `1 : Int` from `1.0 : Float` at this low level. But `1.2` is definitely a Float.
                // So _for now_ take a _horribly unsafe_ guess, by checking if it's a round number or not.
                // Not cool. This is Elm! Long term, the ambiguity needs to be solved at some higher level.
                // Maybe some lib like `JSON.Encode` so the app dev decides? Pity for it not to be automatic though!
                var isRoundNumberSoGuessInt = elmValue === Math.round(elmValue);
                return {
                    kind: 'tag',
                    value: isRoundNumberSoGuessInt ? Tag.Int : Tag.Float
                };
            }
            case 'string':
                return { kind: 'tag', value: Tag.String };
            case 'boolean':
                return {
                    kind: 'constAddr',
                    value: elmValue ? wasmConstAddrs.True : wasmConstAddrs.False
                };
            case 'function':
                return { kind: 'tag', value: Tag.Closure };
            case 'object': {
                if (elmValue instanceof String) {
                    return { kind: 'tag', value: Tag.Char };
                }
                switch (elmValue.$) {
                    case undefined:
                        return { kind: 'tag', value: Tag.Record };
                    case '[]':
                        return { kind: 'constAddr', value: wasmConstAddrs.Nil };
                    case '::':
                        return { kind: 'tag', value: Tag.List };
                    case '#0':
                        return { kind: 'constAddr', value: wasmConstAddrs.Unit };
                    case '#2':
                        return { kind: 'tag', value: Tag.Tuple2 };
                    case '#3':
                        return { kind: 'tag', value: Tag.Tuple3 };
                    default:
                        return { kind: 'tag', value: Tag.Custom };
                }
            }
        }
        console.error(elmValue);
        throw new Error('Cannot determine type of Elm value');
    }
    function wasmBuilder(tag, value) {
        switch (tag) {
            case Tag.Int:
                return {
                    body: [value],
                    jsChildren: [],
                    bodyWriter: null
                };
            case Tag.Float:
                return {
                    body: [],
                    jsChildren: [],
                    bodyWriter: function (bodyAddr) {
                        write32(bodyAddr >> 2, 0);
                        var afterPadding = bodyAddr + WORD;
                        wasmExports._writeF64(afterPadding, value);
                        return 3; // words written
                    }
                };
            case Tag.Char:
            case Tag.String:
                return {
                    body: [],
                    jsChildren: [],
                    bodyWriter: function (bodyAddr) {
                        var s = value;
                        var offset16 = bodyAddr >> 1;
                        var lenAligned = s.length + (s.length % 2); // for odd length, write an extra word (gets coerced to 0)
                        for (var i = 0; i < lenAligned; i++) {
                            write16(offset16 + i, s.charCodeAt(i));
                        }
                        var wordsWritten = lenAligned >> 1;
                        return wordsWritten;
                    }
                };
            case Tag.Tuple2:
            case Tag.List:
                return {
                    body: [],
                    jsChildren: [value.a, value.b],
                    bodyWriter: null
                };
            case Tag.Tuple3:
                return {
                    body: [],
                    jsChildren: [value.a, value.b, value.c],
                    bodyWriter: null
                };
            case Tag.Custom: {
                var jsCtor = value.$;
                var jsChildren_1 = [];
                Object.keys(value).forEach(function (k) {
                    if (k !== '$')
                        jsChildren_1.push(value[k]);
                });
                return {
                    body: [appTypes.ctors[jsCtor]],
                    jsChildren: jsChildren_1,
                    bodyWriter: null
                };
            }
            case Tag.Record: {
                // JS doesn't have the concept of fieldgroups but Wasm does.
                // It's a structure containing info about a specific Record type
                // Need to look it up in the appTypes.
                var keys = Object.keys(value);
                keys.sort();
                var fgName = keys.join('$');
                var fgAddr = appTypes.fieldGroups[fgName];
                return {
                    body: [fgAddr],
                    jsChildren: keys.map(function (k) { return value[k]; }),
                    bodyWriter: null
                };
            }
            case Tag.Closure: {
                // The only Closures that get written back (so far) are Wasm constructor functions.
                // They get passed to Task.map (Wasm) to wrap a value from JS runtime in a Msg constructor,
                // in preparation for a call to `update`
                // If attaching properties to JS functions causes de-optimisation in JS engines,
                // it might actually be worth implementing Task.map and Cmd.map in JS rather than Wasm.
                // (This code will break if JS tries to partially apply an arg before writing to Wasm!
                // That would require custom F2, F3... But I don't think it happens. Wait for a use case.)
                var freeVars = value.freeVars;
                var max_values = value.max_values;
                var evaluator = value.evaluator;
                if (!evaluator) {
                    console.error(value);
                    throw new Error("Can't write a Closure without a reference to a Wasm evaluator function!" +
                        ' Writing arbitrary JS functions is not supported');
                }
                var n_values = freeVars.length;
                return {
                    body: [(max_values << 16) | n_values, evaluator],
                    jsChildren: freeVars,
                    bodyWriter: null
                };
            }
        }
        console.error(value);
        throw new Error("Can't write to WebAssembly for tag \"" + Tag[tag] + "\"");
    }
    function writeFromBuilder(nextIndex, builder, tag) {
        if (builder.bodyWriter) {
            /**
             * Special cases: Float (64-bit data) or String/Char (16-bit data)
             */
            var headerIndex = nextIndex;
            var addr = headerIndex << 2;
            var wordsWritten = builder.bodyWriter(addr + WORD);
            var size = 1 + wordsWritten;
            write32(headerIndex, encodeHeader(tag, size));
            return {
                addr: addr,
                nextIndex: headerIndex + size
            };
        }
        else {
            /**
             * Normal cases (32-bit data)
             */
            var body = builder.body, jsChildren = builder.jsChildren;
            var childAddrs_1 = [];
            jsChildren.forEach(function (child) {
                var update = writeWasmValue(nextIndex, child); // recurse
                childAddrs_1.push(update.addr);
                nextIndex = update.nextIndex;
            });
            var addr = nextIndex << 2;
            var size = 1 + body.length + childAddrs_1.length;
            write32(nextIndex++, encodeHeader(tag, size));
            body.forEach(function (word) {
                write32(nextIndex++, word);
            });
            childAddrs_1.forEach(function (pointer) {
                write32(nextIndex++, pointer);
            });
            return { addr: addr, nextIndex: nextIndex };
        }
    }
    function encodeHeader(tag, size) {
        return (tag << TAG_SHIFT) | (size << SIZE_SHIFT);
    }
    /* --------------------------------------------------
  
                      EXPORTS
  
    -------------------------------------------------- */
    var mains = [];
    while (true) {
        var mainAddr = wasmExports._getNextMain();
        if (!mainAddr)
            break;
        mains.push(readWasmValue(mainAddr));
    }
    return {
        mains: mains,
        // functions for testing
        readWasmValue: readWasmValue,
        writeWasmValue: function (value) {
            return handleWasmWrite(function (nextIndex) { return writeWasmValue(nextIndex, value); });
        }
    };
}
|]
