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
  <> [r|(wasmBuffer, wasmExports, generatedAppTypes, kernelFuncRecord) {
    if (!(wasmBuffer instanceof ArrayBuffer))
        throw new Error('Expected wasmMemory to be an ArrayBuffer');
    /* --------------------------------------------------
  
                 INITIALISATION & CONSTANTS
  
    -------------------------------------------------- */
    const mem32 = new Uint32Array(wasmBuffer);
    const mem16 = new Uint16Array(wasmBuffer);
    const wasmConstAddrs = (function () {
        const Unit = wasmExports._getUnit();
        const Nil = wasmExports._getNil();
        const True = wasmExports._getTrue();
        const False = wasmExports._getFalse();
        const JsNull = wasmExports._getJsNull();
        return {
            Unit,
            Nil,
            True,
            False,
            JsNull,
            [Unit]: _Utils_Tuple0,
            [Nil]: _List_Nil,
            [True]: true,
            [False]: false,
            [JsNull]: null
        };
    })();
    const CTOR_KERNEL_ARRAY = 'CTOR_KERNEL_ARRAY';
    generatedAppTypes.ctors.push(CTOR_KERNEL_ARRAY);
    const appTypes = {
        ctors: arrayToEnum(generatedAppTypes.ctors),
        fields: arrayToEnum(generatedAppTypes.fields),
        fieldGroups: generatedAppTypes.fieldGroups.reduce((enumObj, name) => {
            const addr = wasmExports._getNextFieldGroup();
            enumObj[name] = addr;
            enumObj[addr] = name;
            return enumObj;
        }, {})
    };
    function arrayToEnum(names) {
        return names.reduce((enumObj, name, index) => {
            enumObj[name] = index;
            enumObj[index] = name;
            return enumObj;
        }, {});
    }
    const kernelFunctions = Object.values(kernelFuncRecord);
    const kernelFunctionNames = Object.keys(kernelFuncRecord); // for debug
    const WORD = 4;
    const TAG_MASK = 0xf0000000;
    const TAG_SHIFT = 28;
    const SIZE_MASK = 0x0fffffff;
    const SIZE_SHIFT = 0;
    const NEVER_EVALUATE = 0xffff;
    const KERNEL_CTOR_OFFSET = 1024 * 1000;
    const textDecoder = new TextDecoder('utf-16le');
    const identity = (f) => f;
    const elmFunctionWrappers = [
        identity,
        identity,
        F2,
        F3,
        F4,
        F5,
        F6,
        F7,
        F8,
        F9
    ];
    let Tag;
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
        if (!addr)
            return null;
        const index = addr >> 2;
        const header = mem32[index];
        const tag = (header & TAG_MASK) >>> TAG_SHIFT;
        const size = (header & SIZE_MASK) >>> SIZE_SHIFT;
        switch (tag) {
            case Tag.Int: {
                return mem32[index + 1];
            }
            case Tag.Float: {
                return wasmExports._readF64(addr + 2 * WORD);
            }
            case Tag.Char:
            case Tag.String: {
                const idx16 = (index + 1) << 1;
                let len16 = (size - 1) << 1;
                if (mem16[idx16 + len16 - 1] === 0)
                    len16--;
                const words16 = mem16.slice(idx16, idx16 + len16);
                const jsString = textDecoder.decode(words16);
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
                const elmConst = wasmConstAddrs[addr]; // True/False/Unit/JsNull
                if (elmConst !== undefined)
                    return elmConst;
                const nFields = size - 2;
                const wasmCtor = mem32[index + 1];
                if (wasmCtor >= KERNEL_CTOR_OFFSET) {
                    const custom = {
                        $: wasmCtor - KERNEL_CTOR_OFFSET
                    };
                    const fieldNames = readWasmValue(mem32[index + 2]).split(' ');
                    for (let i = 1; i < nFields; i++) {
                        const field = fieldNames[i];
                        const childAddr = mem32[index + 2 + i];
                        custom[field] = readWasmValue(childAddr);
                    }
                    return custom;
                }
                const jsCtor = appTypes.ctors[wasmCtor];
                if (jsCtor === CTOR_KERNEL_ARRAY) {
                    const kernelArray = [];
                    mem32.slice(index + 2, index + nFields).forEach(childAddr => {
                        kernelArray.push(readWasmValue(childAddr));
                    });
                    return kernelArray;
                }
                const custom = { $: jsCtor };
                const fieldNames = 'abcdefghijklmnopqrstuvwxyz';
                for (let i = 0; i < nFields; i++) {
                    const field = fieldNames[i];
                    const childAddr = mem32[index + 2 + i];
                    custom[field] = readWasmValue(childAddr);
                }
                return custom;
            }
            case Tag.Record: {
                const record = {};
                const fgIndex = mem32[index + 1] >> 2;
                const fgSize = mem32[fgIndex];
                const fields = appTypes.fields;
                for (let i = 1; i <= fgSize; i++) {
                    const fieldId = mem32[fgIndex + i];
                    const valAddr = mem32[index + 1 + i];
                    const fieldName = fields[fieldId];
                    const value = readWasmValue(valAddr);
                    record[fieldName] = value;
                }
                return record;
            }
            case Tag.Closure: {
                const idx16 = index << 1;
                const metadata = {
                    n_values: mem16[idx16 + 2],
                    max_values: mem16[idx16 + 3],
                    evaluator: mem32[index + 2],
                    argsIndex: index + 3
                };
                return metadata.max_values === NEVER_EVALUATE
                    ? evalKernelThunk(metadata)
                    : createWasmCallback(metadata);
            }
            default:
                throw new Error('Tried to decode value with unsupported tag ' +
                    (Tag[tag] || '0x' + tag.toString(16)));
        }
    }
    function evalKernelThunk(metadata) {
        const { n_values, evaluator, argsIndex } = metadata;
        let kernelValue = kernelFunctions[evaluator];
        for (let i = argsIndex; i < argsIndex + n_values; i++) {
            const arg = readWasmValue(mem32[i]);
            kernelValue = kernelValue(arg);
        }
        return kernelValue;
    }
    function createWasmCallback(metadata) {
        const { n_values, max_values, evaluator, argsIndex } = metadata;
        const freeVars = [];
        for (let i = argsIndex; i < argsIndex + n_values; i++) {
            freeVars.push(readWasmValue(mem32[i]));
        }
        function wasmCallback() {
            const args = freeVars.slice();
            for (let i = 0; i < arguments.length; i++) {
                args.push(arguments[i]);
            }
            const n_values = args.length;
            if (n_values !== max_values) {
                console.error({ wasmCallback, args });
                throw new Error(`Trying to call a Wasm Closure with ${n_values} args instead of ${max_values}!`);
            }
            const builder = {
                body: [(max_values << 16) | n_values, evaluator],
                jsChildren: args,
                bodyWriter: null
            };
            const addr = handleWasmWrite((startIndex) => {
                return writeFromBuilder(startIndex, builder, Tag.Closure);
            });
            const resultAddr = wasmExports._evalClosure(addr);
            const resultValue = readWasmValue(resultAddr);
            return resultValue;
        }
        // Attach info in case we have to write this Closure back to Wasm
        wasmCallback.freeVars = freeVars;
        wasmCallback.evaluator = evaluator;
        wasmCallback.max_values = max_values;
        const arity = max_values - n_values;
        const FN = elmFunctionWrappers[arity];
        return FN(wasmCallback);
    }
    /* --------------------------------------------------
  
                  WRITE ELM VALUES TO WASM
  
    -------------------------------------------------- */
    let maxWriteIndex32;
    let maxWriteIndex16;
    const heapOverflowError = new Error('Wasm heap overflow');
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
        for (let attempts = 0; attempts < 2; attempts++) {
            try {
                const maxAddr = wasmExports._getMaxWriteAddr();
                maxWriteIndex16 = maxAddr >> 1;
                maxWriteIndex32 = maxAddr >> 2;
                const startAddr = wasmExports._getWriteAddr();
                const startIndex = startAddr >> 2;
                const result = writer(startIndex);
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
        const typeInfo = detectElmType(value);
        switch (typeInfo.kind) {
            case 'constAddr':
                return { addr: typeInfo.value, nextIndex };
            case 'tag': {
                const tag = typeInfo.value;
                const builder = wasmBuilder(tag, value);
                return writeFromBuilder(nextIndex, builder, tag);
            }
            case 'kernelArray': {
                const customObj = value.slice();
                customObj.$ = CTOR_KERNEL_ARRAY;
                const builder = wasmBuilder(Tag.Custom, customObj);
                return writeFromBuilder(nextIndex, builder, Tag.Closure);
            }
        }
    }
    function detectElmType(elmValue) {
        if (elmValue === null || elmValue === undefined) {
            return { kind: 'constAddr', value: wasmConstAddrs.JsNull };
        }
        switch (typeof elmValue) {
            case 'number': {
                // There's no way to tell `1 : Int` from `1.0 : Float` at this low level. But `1.2` is definitely a Float.
                // So _for now_ take a _horribly unsafe_ guess, by checking if it's a round number or not.
                // Not cool. This is Elm! Long term, the ambiguity needs to be solved at some higher level.
                // Maybe some lib like `JSON.Encode` so the app dev decides? Pity for it not to be automatic though!
                const isRoundNumberSoGuessInt = elmValue === Math.round(elmValue);
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
                if (Array.isArray(elmValue)) {
                    return { kind: 'kernelArray' };
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
                    bodyWriter: (bodyAddr) => {
                        write32(bodyAddr >> 2, 0);
                        const afterPadding = bodyAddr + WORD;
                        wasmExports._writeF64(afterPadding, value);
                        return 3; // words written
                    }
                };
            case Tag.Char:
            case Tag.String:
                return {
                    body: [],
                    jsChildren: [],
                    bodyWriter: (bodyAddr) => {
                        const s = value;
                        const offset16 = bodyAddr >> 1;
                        const lenAligned = s.length + (s.length % 2); // for odd length, write an extra word (gets coerced to 0)
                        for (let i = 0; i < lenAligned; i++) {
                            write16(offset16 + i, s.charCodeAt(i));
                        }
                        const wordsWritten = lenAligned >> 1;
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
                const jsCtor = value.$;
                let body;
                const jsChildren = [];
                const keys = Object.keys(value).filter(k => k !== '$');
                if (typeof jsCtor === 'number') {
                    body = [KERNEL_CTOR_OFFSET + jsCtor];
                    const keyString = keys.join(' ');
                    jsChildren.push(keyString);
                }
                else {
                    body = [appTypes.ctors[jsCtor]];
                }
                keys.forEach(k => jsChildren.push(value[k]));
                return {
                    body,
                    jsChildren,
                    bodyWriter: null
                };
            }
            case Tag.Record: {
                // JS doesn't have the concept of fieldgroups but Wasm does.
                // It's a structure containing info about a specific Record type
                // Need to look it up in the appTypes.
                const keys = Object.keys(value);
                keys.sort();
                const fgName = keys.join(' ');
                const fgAddr = appTypes.fieldGroups[fgName];
                return {
                    body: [fgAddr],
                    jsChildren: keys.map(k => value[k]),
                    bodyWriter: null
                };
            }
            case Tag.Closure: {
                const fun = value.f || value;
                if (fun.evaluator) {
                    const { freeVars, max_values, evaluator } = fun;
                    const n_values = freeVars.length;
                    return {
                        body: [(max_values << 16) | n_values, evaluator],
                        jsChildren: freeVars,
                        bodyWriter: null
                    };
                }
                else {
                    let evaluator = kernelFunctions.findIndex(f => f === value);
                    if (evaluator === -1) {
                        kernelFunctions.push(value);
                        evaluator = kernelFunctions.length - 1;
                    }
                    return {
                        body: [NEVER_EVALUATE << 16, evaluator],
                        jsChildren: [],
                        bodyWriter: null
                    };
                }
            }
        }
        console.error(value);
        throw new Error(`Can't write to WebAssembly for tag "${Tag[tag]}"`);
    }
    function writeFromBuilder(nextIndex, builder, tag) {
        if (builder.bodyWriter) {
            /**
             * Special cases: Float (64-bit data) or String/Char (16-bit data)
             */
            const headerIndex = nextIndex;
            const addr = headerIndex << 2;
            const wordsWritten = builder.bodyWriter(addr + WORD);
            const size = 1 + wordsWritten;
            write32(headerIndex, encodeHeader(tag, size));
            return {
                addr,
                nextIndex: headerIndex + size
            };
        }
        else {
            /**
             * Normal cases (32-bit data)
             */
            const { body, jsChildren } = builder;
            const childAddrs = [];
            jsChildren.forEach(child => {
                const update = writeWasmValue(nextIndex, child); // recurse
                childAddrs.push(update.addr);
                nextIndex = update.nextIndex;
            });
            const addr = nextIndex << 2;
            const size = 1 + body.length + childAddrs.length;
            write32(nextIndex++, encodeHeader(tag, size));
            body.forEach(word => {
                write32(nextIndex++, word);
            });
            childAddrs.forEach(pointer => {
                write32(nextIndex++, pointer);
            });
            return { addr, nextIndex };
        }
    }
    function encodeHeader(tag, size) {
        return (tag << TAG_SHIFT) | (size << SIZE_SHIFT);
    }
    /* --------------------------------------------------
  
                      EXPORTS
  
    -------------------------------------------------- */
    const mains = [];
    while (true) {
        const mainAddr = wasmExports._getNextMain();
        if (!mainAddr)
            break;
        mains.push(readWasmValue(mainAddr));
    }
    return {
        mains,
        // functions for testing
        readWasmValue,
        writeWasmValue: (value) => handleWasmWrite(nextIndex => writeWasmValue(nextIndex, value))
    };
}
|]
