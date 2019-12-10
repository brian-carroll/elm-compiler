{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Generate.C.JsWrappers
  ( emscripten
  , emscriptenModuleName
  , wrapper
  , wrapperFnName
  )
  where


import qualified Data.ByteString.Builder as B
import Text.RawString.QQ (r)


emscriptenModuleName :: String
emscriptenModuleName =
  "EmscriptenModule"

emscripten :: B.Builder
emscripten = [r|

var EmscriptenModule = (
function(EmscriptenModule) {
  EmscriptenModule = EmscriptenModule || {};

var Module=typeof EmscriptenModule!=="undefined"?EmscriptenModule:{};var moduleOverrides={};var key;for(key in Module){if(Module.hasOwnProperty(key)){moduleOverrides[key]=Module[key]}}Module["arguments"]=[];Module["thisProgram"]="./this.program";Module["quit"]=(function(status,toThrow){throw toThrow});Module["preRun"]=[];Module["postRun"]=[];var ENVIRONMENT_IS_WEB=false;var ENVIRONMENT_IS_WORKER=false;var ENVIRONMENT_IS_NODE=false;var ENVIRONMENT_IS_SHELL=false;ENVIRONMENT_IS_WEB=typeof window==="object";ENVIRONMENT_IS_WORKER=typeof importScripts==="function";ENVIRONMENT_IS_NODE=typeof process==="object"&&typeof require==="function"&&!ENVIRONMENT_IS_WEB&&!ENVIRONMENT_IS_WORKER;ENVIRONMENT_IS_SHELL=!ENVIRONMENT_IS_WEB&&!ENVIRONMENT_IS_NODE&&!ENVIRONMENT_IS_WORKER;var scriptDirectory="";function locateFile(path){if(Module["locateFile"]){return Module["locateFile"](path,scriptDirectory)}else{return scriptDirectory+path}}if(ENVIRONMENT_IS_NODE){scriptDirectory=__dirname+"/";var nodeFS;var nodePath;Module["read"]=function shell_read(filename,binary){var ret;if(!nodeFS)nodeFS=require("fs");if(!nodePath)nodePath=require("path");filename=nodePath["normalize"](filename);ret=nodeFS["readFileSync"](filename);return binary?ret:ret.toString()};Module["readBinary"]=function readBinary(filename){var ret=Module["read"](filename,true);if(!ret.buffer){ret=new Uint8Array(ret)}assert(ret.buffer);return ret};if(process["argv"].length>1){Module["thisProgram"]=process["argv"][1].replace(/\\/g,"/")}Module["arguments"]=process["argv"].slice(2);process["on"]("uncaughtException",(function(ex){if(!(ex instanceof ExitStatus)){throw ex}}));process["on"]("unhandledRejection",(function(reason,p){process["exit"](1)}));Module["quit"]=(function(status){process["exit"](status)});Module["inspect"]=(function(){return"[Emscripten Module object]"})}else if(ENVIRONMENT_IS_SHELL){if(typeof read!="undefined"){Module["read"]=function shell_read(f){return read(f)}}Module["readBinary"]=function readBinary(f){var data;if(typeof readbuffer==="function"){return new Uint8Array(readbuffer(f))}data=read(f,"binary");assert(typeof data==="object");return data};if(typeof scriptArgs!="undefined"){Module["arguments"]=scriptArgs}else if(typeof arguments!="undefined"){Module["arguments"]=arguments}if(typeof quit==="function"){Module["quit"]=(function(status){quit(status)})}}else if(ENVIRONMENT_IS_WEB||ENVIRONMENT_IS_WORKER){if(ENVIRONMENT_IS_WEB){if(document.currentScript){scriptDirectory=document.currentScript.src}}else{scriptDirectory=self.location.href}if(scriptDirectory.indexOf("blob:")!==0){scriptDirectory=scriptDirectory.substr(0,scriptDirectory.lastIndexOf("/")+1)}else{scriptDirectory=""}Module["read"]=function shell_read(url){var xhr=new XMLHttpRequest;xhr.open("GET",url,false);xhr.send(null);return xhr.responseText};if(ENVIRONMENT_IS_WORKER){Module["readBinary"]=function readBinary(url){var xhr=new XMLHttpRequest;xhr.open("GET",url,false);xhr.responseType="arraybuffer";xhr.send(null);return new Uint8Array(xhr.response)}}Module["readAsync"]=function readAsync(url,onload,onerror){var xhr=new XMLHttpRequest;xhr.open("GET",url,true);xhr.responseType="arraybuffer";xhr.onload=function xhr_onload(){if(xhr.status==200||xhr.status==0&&xhr.response){onload(xhr.response);return}onerror()};xhr.onerror=onerror;xhr.send(null)};Module["setWindowTitle"]=(function(title){document.title=title})}else{}var out=Module["print"]||(typeof console!=="undefined"?console.log.bind(console):typeof print!=="undefined"?print:null);var err=Module["printErr"]||(typeof printErr!=="undefined"?printErr:typeof console!=="undefined"&&console.warn.bind(console)||out);for(key in moduleOverrides){if(moduleOverrides.hasOwnProperty(key)){Module[key]=moduleOverrides[key]}}moduleOverrides=undefined;var STACK_ALIGN=16;function staticAlloc(size){var ret=STATICTOP;STATICTOP=STATICTOP+size+15&-16;return ret}function alignMemory(size,factor){if(!factor)factor=STACK_ALIGN;var ret=size=Math.ceil(size/factor)*factor;return ret}var asm2wasmImports={"f64-rem":(function(x,y){return x%y}),"debugger":(function(){debugger})};var functionPointers=new Array(0);var GLOBAL_BASE=1024;var ABORT=false;var EXITSTATUS=0;function assert(condition,text){if(!condition){abort("Assertion failed: "+text)}}function getCFunc(ident){var func=Module["_"+ident];assert(func,"Cannot call unknown function "+ident+", make sure it is exported");return func}var JSfuncs={"stackSave":(function(){stackSave()}),"stackRestore":(function(){stackRestore()}),"arrayToC":(function(arr){var ret=stackAlloc(arr.length);writeArrayToMemory(arr,ret);return ret}),"stringToC":(function(str){var ret=0;if(str!==null&&str!==undefined&&str!==0){var len=(str.length<<2)+1;ret=stackAlloc(len);stringToUTF8(str,ret,len)}return ret})};var toC={"string":JSfuncs["stringToC"],"array":JSfuncs["arrayToC"]};function ccall(ident,returnType,argTypes,args,opts){function convertReturnValue(ret){if(returnType==="string")return Pointer_stringify(ret);if(returnType==="boolean")return Boolean(ret);return ret}var func=getCFunc(ident);var cArgs=[];var stack=0;if(args){for(var i=0;i<args.length;i++){var converter=toC[argTypes[i]];if(converter){if(stack===0)stack=stackSave();cArgs[i]=converter(args[i])}else{cArgs[i]=args[i]}}}var ret=func.apply(null,cArgs);ret=convertReturnValue(ret);if(stack!==0)stackRestore(stack);return ret}function cwrap(ident,returnType,argTypes,opts){argTypes=argTypes||[];var numericArgs=argTypes.every((function(type){return type==="number"}));var numericRet=returnType!=="string";if(numericRet&&numericArgs&&!opts){return getCFunc(ident)}return(function(){return ccall(ident,returnType,argTypes,arguments,opts)})}function Pointer_stringify(ptr,length){if(length===0||!ptr)return"";var hasUtf=0;var t;var i=0;while(1){t=HEAPU8[ptr+i>>0];hasUtf|=t;if(t==0&&!length)break;i++;if(length&&i==length)break}if(!length)length=i;var ret="";if(hasUtf<128){var MAX_CHUNK=1024;var curr;while(length>0){curr=String.fromCharCode.apply(String,HEAPU8.subarray(ptr,ptr+Math.min(length,MAX_CHUNK)));ret=ret?ret+curr:curr;ptr+=MAX_CHUNK;length-=MAX_CHUNK}return ret}return UTF8ToString(ptr)}var UTF8Decoder=typeof TextDecoder!=="undefined"?new TextDecoder("utf8"):undefined;function UTF8ArrayToString(u8Array,idx){var endPtr=idx;while(u8Array[endPtr])++endPtr;if(endPtr-idx>16&&u8Array.subarray&&UTF8Decoder){return UTF8Decoder.decode(u8Array.subarray(idx,endPtr))}else{var u0,u1,u2,u3,u4,u5;var str="";while(1){u0=u8Array[idx++];if(!u0)return str;if(!(u0&128)){str+=String.fromCharCode(u0);continue}u1=u8Array[idx++]&63;if((u0&224)==192){str+=String.fromCharCode((u0&31)<<6|u1);continue}u2=u8Array[idx++]&63;if((u0&240)==224){u0=(u0&15)<<12|u1<<6|u2}else{u3=u8Array[idx++]&63;if((u0&248)==240){u0=(u0&7)<<18|u1<<12|u2<<6|u3}else{u4=u8Array[idx++]&63;if((u0&252)==248){u0=(u0&3)<<24|u1<<18|u2<<12|u3<<6|u4}else{u5=u8Array[idx++]&63;u0=(u0&1)<<30|u1<<24|u2<<18|u3<<12|u4<<6|u5}}}if(u0<65536){str+=String.fromCharCode(u0)}else{var ch=u0-65536;str+=String.fromCharCode(55296|ch>>10,56320|ch&1023)}}}}function UTF8ToString(ptr){return UTF8ArrayToString(HEAPU8,ptr)}function stringToUTF8Array(str,outU8Array,outIdx,maxBytesToWrite){if(!(maxBytesToWrite>0))return 0;var startIdx=outIdx;var endIdx=outIdx+maxBytesToWrite-1;for(var i=0;i<str.length;++i){var u=str.charCodeAt(i);if(u>=55296&&u<=57343){var u1=str.charCodeAt(++i);u=65536+((u&1023)<<10)|u1&1023}if(u<=127){if(outIdx>=endIdx)break;outU8Array[outIdx++]=u}else if(u<=2047){if(outIdx+1>=endIdx)break;outU8Array[outIdx++]=192|u>>6;outU8Array[outIdx++]=128|u&63}else if(u<=65535){if(outIdx+2>=endIdx)break;outU8Array[outIdx++]=224|u>>12;outU8Array[outIdx++]=128|u>>6&63;outU8Array[outIdx++]=128|u&63}else if(u<=2097151){if(outIdx+3>=endIdx)break;outU8Array[outIdx++]=240|u>>18;outU8Array[outIdx++]=128|u>>12&63;outU8Array[outIdx++]=128|u>>6&63;outU8Array[outIdx++]=128|u&63}else if(u<=67108863){if(outIdx+4>=endIdx)break;outU8Array[outIdx++]=248|u>>24;outU8Array[outIdx++]=128|u>>18&63;outU8Array[outIdx++]=128|u>>12&63;outU8Array[outIdx++]=128|u>>6&63;outU8Array[outIdx++]=128|u&63}else{if(outIdx+5>=endIdx)break;outU8Array[outIdx++]=252|u>>30;outU8Array[outIdx++]=128|u>>24&63;outU8Array[outIdx++]=128|u>>18&63;outU8Array[outIdx++]=128|u>>12&63;outU8Array[outIdx++]=128|u>>6&63;outU8Array[outIdx++]=128|u&63}}outU8Array[outIdx]=0;return outIdx-startIdx}function stringToUTF8(str,outPtr,maxBytesToWrite){return stringToUTF8Array(str,HEAPU8,outPtr,maxBytesToWrite)}function lengthBytesUTF8(str){var len=0;for(var i=0;i<str.length;++i){var u=str.charCodeAt(i);if(u>=55296&&u<=57343)u=65536+((u&1023)<<10)|str.charCodeAt(++i)&1023;if(u<=127){++len}else if(u<=2047){len+=2}else if(u<=65535){len+=3}else if(u<=2097151){len+=4}else if(u<=67108863){len+=5}else{len+=6}}return len}var UTF16Decoder=typeof TextDecoder!=="undefined"?new TextDecoder("utf-16le"):undefined;function allocateUTF8OnStack(str){var size=lengthBytesUTF8(str)+1;var ret=stackAlloc(size);stringToUTF8Array(str,HEAP8,ret,size);return ret}var WASM_PAGE_SIZE=65536;var ASMJS_PAGE_SIZE=16777216;function alignUp(x,multiple){if(x%multiple>0){x+=multiple-x%multiple}return x}var buffer,HEAP8,HEAPU8,HEAP16,HEAPU16,HEAP32,HEAPU32,HEAPF32,HEAPF64;function updateGlobalBuffer(buf){Module["buffer"]=buffer=buf}function updateGlobalBufferViews(){Module["HEAP8"]=HEAP8=new Int8Array(buffer);Module["HEAP16"]=HEAP16=new Int16Array(buffer);Module["HEAP32"]=HEAP32=new Int32Array(buffer);Module["HEAPU8"]=HEAPU8=new Uint8Array(buffer);Module["HEAPU16"]=HEAPU16=new Uint16Array(buffer);Module["HEAPU32"]=HEAPU32=new Uint32Array(buffer);Module["HEAPF32"]=HEAPF32=new Float32Array(buffer);Module["HEAPF64"]=HEAPF64=new Float64Array(buffer)}var STATIC_BASE,STATICTOP,staticSealed;var STACK_BASE,STACKTOP,STACK_MAX;var DYNAMIC_BASE,DYNAMICTOP_PTR;STATIC_BASE=STATICTOP=STACK_BASE=STACKTOP=STACK_MAX=DYNAMIC_BASE=DYNAMICTOP_PTR=0;staticSealed=false;function abortOnCannotGrowMemory(){abort("Cannot enlarge memory arrays. Either (1) compile with  -s TOTAL_MEMORY=X  with X higher than the current value "+TOTAL_MEMORY+", (2) compile with  -s ALLOW_MEMORY_GROWTH=1  which allows increasing the size at runtime, or (3) if you want malloc to return NULL (0) instead of this abort, compile with  -s ABORTING_MALLOC=0 ")}function enlargeMemory(){abortOnCannotGrowMemory()}var TOTAL_STACK=Module["TOTAL_STACK"]||5242880;var TOTAL_MEMORY=Module["TOTAL_MEMORY"]||16777216;if(TOTAL_MEMORY<TOTAL_STACK)err("TOTAL_MEMORY should be larger than TOTAL_STACK, was "+TOTAL_MEMORY+"! (TOTAL_STACK="+TOTAL_STACK+")");if(Module["buffer"]){buffer=Module["buffer"]}else{if(typeof WebAssembly==="object"&&typeof WebAssembly.Memory==="function"){Module["wasmMemory"]=new WebAssembly.Memory({"initial":TOTAL_MEMORY/WASM_PAGE_SIZE,"maximum":TOTAL_MEMORY/WASM_PAGE_SIZE});buffer=Module["wasmMemory"].buffer}else{buffer=new ArrayBuffer(TOTAL_MEMORY)}Module["buffer"]=buffer}updateGlobalBufferViews();function getTotalMemory(){return TOTAL_MEMORY}function callRuntimeCallbacks(callbacks){while(callbacks.length>0){var callback=callbacks.shift();if(typeof callback=="function"){callback();continue}var func=callback.func;if(typeof func==="number"){if(callback.arg===undefined){Module["dynCall_v"](func)}else{Module["dynCall_vi"](func,callback.arg)}}else{func(callback.arg===undefined?null:callback.arg)}}}var __ATPRERUN__=[];var __ATINIT__=[];var __ATMAIN__=[];var __ATEXIT__=[];var __ATPOSTRUN__=[];var runtimeInitialized=false;var runtimeExited=false;function preRun(){if(Module["preRun"]){if(typeof Module["preRun"]=="function")Module["preRun"]=[Module["preRun"]];while(Module["preRun"].length){addOnPreRun(Module["preRun"].shift())}}callRuntimeCallbacks(__ATPRERUN__)}function ensureInitRuntime(){if(runtimeInitialized)return;runtimeInitialized=true;callRuntimeCallbacks(__ATINIT__)}function preMain(){callRuntimeCallbacks(__ATMAIN__)}function exitRuntime(){callRuntimeCallbacks(__ATEXIT__);runtimeExited=true}function postRun(){if(Module["postRun"]){if(typeof Module["postRun"]=="function")Module["postRun"]=[Module["postRun"]];while(Module["postRun"].length){addOnPostRun(Module["postRun"].shift())}}callRuntimeCallbacks(__ATPOSTRUN__)}function addOnPreRun(cb){__ATPRERUN__.unshift(cb)}function addOnPostRun(cb){__ATPOSTRUN__.unshift(cb)}function writeArrayToMemory(array,buffer){HEAP8.set(array,buffer)}var runDependencies=0;var runDependencyWatcher=null;var dependenciesFulfilled=null;function addRunDependency(id){runDependencies++;if(Module["monitorRunDependencies"]){Module["monitorRunDependencies"](runDependencies)}}function removeRunDependency(id){runDependencies--;if(Module["monitorRunDependencies"]){Module["monitorRunDependencies"](runDependencies)}if(runDependencies==0){if(runDependencyWatcher!==null){clearInterval(runDependencyWatcher);runDependencyWatcher=null}if(dependenciesFulfilled){var callback=dependenciesFulfilled;dependenciesFulfilled=null;callback()}}}Module["preloadedImages"]={};Module["preloadedAudios"]={};var dataURIPrefix="data:application/octet-stream;base64,";function isDataURI(filename){return String.prototype.startsWith?filename.startsWith(dataURIPrefix):filename.indexOf(dataURIPrefix)===0}function integrateWasmJS(){var wasmTextFile="main.wast";var wasmBinaryFile="main.wasm";var asmjsCodeFile="main.temp.asm.js";if(!isDataURI(wasmTextFile)){wasmTextFile=locateFile(wasmTextFile)}if(!isDataURI(wasmBinaryFile)){wasmBinaryFile=locateFile(wasmBinaryFile)}if(!isDataURI(asmjsCodeFile)){asmjsCodeFile=locateFile(asmjsCodeFile)}var wasmPageSize=64*1024;var info={"global":null,"env":null,"asm2wasm":asm2wasmImports,"parent":Module};var exports=null;function mergeMemory(newBuffer){var oldBuffer=Module["buffer"];if(newBuffer.byteLength<oldBuffer.byteLength){err("the new buffer in mergeMemory is smaller than the previous one. in native wasm, we should grow memory here")}var oldView=new Int8Array(oldBuffer);var newView=new Int8Array(newBuffer);newView.set(oldView);updateGlobalBuffer(newBuffer);updateGlobalBufferViews()}function fixImports(imports){return imports}function getBinary(){try{if(Module["wasmBinary"]){return new Uint8Array(Module["wasmBinary"])}if(Module["readBinary"]){return Module["readBinary"](wasmBinaryFile)}else{throw"both async and sync fetching of the wasm failed"}}catch(err){abort(err)}}function getBinaryPromise(){if(!Module["wasmBinary"]&&(ENVIRONMENT_IS_WEB||ENVIRONMENT_IS_WORKER)&&typeof fetch==="function"){return fetch(wasmBinaryFile,{credentials:"same-origin"}).then((function(response){if(!response["ok"]){throw"failed to load wasm binary file at '"+wasmBinaryFile+"'"}return response["arrayBuffer"]()})).catch((function(){return getBinary()}))}return new Promise((function(resolve,reject){resolve(getBinary())}))}function doNativeWasm(global,env,providedBuffer){if(typeof WebAssembly!=="object"){err("no native wasm support detected");return false}if(!(Module["wasmMemory"]instanceof WebAssembly.Memory)){err("no native wasm Memory in use");return false}env["memory"]=Module["wasmMemory"];info["global"]={"NaN":NaN,"Infinity":Infinity};info["global.Math"]=Math;info["env"]=env;function receiveInstance(instance,module){exports=instance.exports;if(exports.memory)mergeMemory(exports.memory);Module["asm"]=exports;Module["usingWasm"]=true;removeRunDependency("wasm-instantiate")}addRunDependency("wasm-instantiate");if(Module["instantiateWasm"]){try{return Module["instantiateWasm"](info,receiveInstance)}catch(e){err("Module.instantiateWasm callback failed with error: "+e);return false}}function receiveInstantiatedSource(output){receiveInstance(output["instance"],output["module"])}function instantiateArrayBuffer(receiver){getBinaryPromise().then((function(binary){return WebAssembly.instantiate(binary,info)})).then(receiver).catch((function(reason){err("failed to asynchronously prepare wasm: "+reason);abort(reason)}))}if(!Module["wasmBinary"]&&typeof WebAssembly.instantiateStreaming==="function"&&!isDataURI(wasmBinaryFile)&&typeof fetch==="function"){WebAssembly.instantiateStreaming(fetch(wasmBinaryFile,{credentials:"same-origin"}),info).then(receiveInstantiatedSource).catch((function(reason){err("wasm streaming compile failed: "+reason);err("falling back to ArrayBuffer instantiation");instantiateArrayBuffer(receiveInstantiatedSource)}))}else{instantiateArrayBuffer(receiveInstantiatedSource)}return{}}Module["asmPreload"]=Module["asm"];var asmjsReallocBuffer=Module["reallocBuffer"];var wasmReallocBuffer=(function(size){var PAGE_MULTIPLE=Module["usingWasm"]?WASM_PAGE_SIZE:ASMJS_PAGE_SIZE;size=alignUp(size,PAGE_MULTIPLE);var old=Module["buffer"];var oldSize=old.byteLength;if(Module["usingWasm"]){try{var result=Module["wasmMemory"].grow((size-oldSize)/wasmPageSize);if(result!==(-1|0)){return Module["buffer"]=Module["wasmMemory"].buffer}else{return null}}catch(e){return null}}});Module["reallocBuffer"]=(function(size){if(finalMethod==="asmjs"){return asmjsReallocBuffer(size)}else{return wasmReallocBuffer(size)}});var finalMethod="";Module["asm"]=(function(global,env,providedBuffer){env=fixImports(env);if(!env["table"]){var TABLE_SIZE=Module["wasmTableSize"];if(TABLE_SIZE===undefined)TABLE_SIZE=1024;var MAX_TABLE_SIZE=Module["wasmMaxTableSize"];if(typeof WebAssembly==="object"&&typeof WebAssembly.Table==="function"){if(MAX_TABLE_SIZE!==undefined){env["table"]=new WebAssembly.Table({"initial":TABLE_SIZE,"maximum":MAX_TABLE_SIZE,"element":"anyfunc"})}else{env["table"]=new WebAssembly.Table({"initial":TABLE_SIZE,element:"anyfunc"})}}else{env["table"]=new Array(TABLE_SIZE)}Module["wasmTable"]=env["table"]}if(!env["memoryBase"]){env["memoryBase"]=Module["STATIC_BASE"]}if(!env["tableBase"]){env["tableBase"]=0}var exports;exports=doNativeWasm(global,env,providedBuffer);assert(exports,"no binaryen method succeeded.");return exports})}integrateWasmJS();STATIC_BASE=GLOBAL_BASE;STATICTOP=STATIC_BASE+7216;__ATINIT__.push();var STATIC_BUMP=7216;Module["STATIC_BASE"]=STATIC_BASE;Module["STATIC_BUMP"]=STATIC_BUMP;STATICTOP+=16;function ___assert_fail(condition,filename,line,func){abort("Assertion failed: "+Pointer_stringify(condition)+", at: "+[filename?Pointer_stringify(filename):"unknown filename",line,func?Pointer_stringify(func):"unknown function"])}var SYSCALLS={varargs:0,get:(function(varargs){SYSCALLS.varargs+=4;var ret=HEAP32[SYSCALLS.varargs-4>>2];return ret}),getStr:(function(){var ret=Pointer_stringify(SYSCALLS.get());return ret}),get64:(function(){var low=SYSCALLS.get(),high=SYSCALLS.get();if(low>=0)assert(high===0);else assert(high===-1);return low}),getZero:(function(){assert(SYSCALLS.get()===0)})};function ___syscall140(which,varargs){SYSCALLS.varargs=varargs;try{var stream=SYSCALLS.getStreamFromFD(),offset_high=SYSCALLS.get(),offset_low=SYSCALLS.get(),result=SYSCALLS.get(),whence=SYSCALLS.get();var offset=offset_low;FS.llseek(stream,offset,whence);HEAP32[result>>2]=stream.position;if(stream.getdents&&offset===0&&whence===0)stream.getdents=null;return 0}catch(e){if(typeof FS==="undefined"||!(e instanceof FS.ErrnoError))abort(e);return-e.errno}}function ___syscall146(which,varargs){SYSCALLS.varargs=varargs;try{var stream=SYSCALLS.get(),iov=SYSCALLS.get(),iovcnt=SYSCALLS.get();var ret=0;if(!___syscall146.buffers){___syscall146.buffers=[null,[],[]];___syscall146.printChar=(function(stream,curr){var buffer=___syscall146.buffers[stream];assert(buffer);if(curr===0||curr===10){(stream===1?out:err)(UTF8ArrayToString(buffer,0));buffer.length=0}else{buffer.push(curr)}})}for(var i=0;i<iovcnt;i++){var ptr=HEAP32[iov+i*8>>2];var len=HEAP32[iov+(i*8+4)>>2];for(var j=0;j<len;j++){___syscall146.printChar(stream,HEAPU8[ptr+j])}ret+=len}return ret}catch(e){if(typeof FS==="undefined"||!(e instanceof FS.ErrnoError))abort(e);return-e.errno}}function ___syscall54(which,varargs){SYSCALLS.varargs=varargs;try{return 0}catch(e){if(typeof FS==="undefined"||!(e instanceof FS.ErrnoError))abort(e);return-e.errno}}function ___syscall6(which,varargs){SYSCALLS.varargs=varargs;try{var stream=SYSCALLS.getStreamFromFD();FS.close(stream);return 0}catch(e){if(typeof FS==="undefined"||!(e instanceof FS.ErrnoError))abort(e);return-e.errno}}function _emscripten_memcpy_big(dest,src,num){HEAPU8.set(HEAPU8.subarray(src,src+num),dest);return dest}function ___setErrNo(value){if(Module["___errno_location"])HEAP32[Module["___errno_location"]()>>2]=value;return value}DYNAMICTOP_PTR=staticAlloc(4);STACK_BASE=STACKTOP=alignMemory(STATICTOP);STACK_MAX=STACK_BASE+TOTAL_STACK;DYNAMIC_BASE=alignMemory(STACK_MAX);HEAP32[DYNAMICTOP_PTR>>2]=DYNAMIC_BASE;staticSealed=true;Module["wasmTableSize"]=48;Module["wasmMaxTableSize"]=48;Module.asmGlobalArg={};Module.asmLibraryArg={"abort":abort,"enlargeMemory":enlargeMemory,"getTotalMemory":getTotalMemory,"abortOnCannotGrowMemory":abortOnCannotGrowMemory,"___assert_fail":___assert_fail,"___setErrNo":___setErrNo,"___syscall140":___syscall140,"___syscall146":___syscall146,"___syscall54":___syscall54,"___syscall6":___syscall6,"_emscripten_memcpy_big":_emscripten_memcpy_big,"DYNAMICTOP_PTR":DYNAMICTOP_PTR,"STACKTOP":STACKTOP};var asm=Module["asm"](Module.asmGlobalArg,Module.asmLibraryArg,buffer);Module["asm"]=asm;var ___errno_location=Module["___errno_location"]=(function(){return Module["asm"]["___errno_location"].apply(null,arguments)});var _collectGarbage=Module["_collectGarbage"]=(function(){return Module["asm"]["_collectGarbage"].apply(null,arguments)});var _debugHeapState=Module["_debugHeapState"]=(function(){return Module["asm"]["_debugHeapState"].apply(null,arguments)});var _evalClosure=Module["_evalClosure"]=(function(){return Module["asm"]["_evalClosure"].apply(null,arguments)});var _finishWritingAt=Module["_finishWritingAt"]=(function(){return Module["asm"]["_finishWritingAt"].apply(null,arguments)});var _getFalse=Module["_getFalse"]=(function(){return Module["asm"]["_getFalse"].apply(null,arguments)});var _getMaxWriteAddr=Module["_getMaxWriteAddr"]=(function(){return Module["asm"]["_getMaxWriteAddr"].apply(null,arguments)});var _getNextFieldGroup=Module["_getNextFieldGroup"]=(function(){return Module["asm"]["_getNextFieldGroup"].apply(null,arguments)});var _getNextMain=Module["_getNextMain"]=(function(){return Module["asm"]["_getNextMain"].apply(null,arguments)});var _getNil=Module["_getNil"]=(function(){return Module["asm"]["_getNil"].apply(null,arguments)});var _getTrue=Module["_getTrue"]=(function(){return Module["asm"]["_getTrue"].apply(null,arguments)});var _getUnit=Module["_getUnit"]=(function(){return Module["asm"]["_getUnit"].apply(null,arguments)});var _getWriteAddr=Module["_getWriteAddr"]=(function(){return Module["asm"]["_getWriteAddr"].apply(null,arguments)});var _main=Module["_main"]=(function(){return Module["asm"]["_main"].apply(null,arguments)});var _readF64=Module["_readF64"]=(function(){return Module["asm"]["_readF64"].apply(null,arguments)});var _writeF64=Module["_writeF64"]=(function(){return Module["asm"]["_writeF64"].apply(null,arguments)});var stackAlloc=Module["stackAlloc"]=(function(){return Module["asm"]["stackAlloc"].apply(null,arguments)});var stackRestore=Module["stackRestore"]=(function(){return Module["asm"]["stackRestore"].apply(null,arguments)});var stackSave=Module["stackSave"]=(function(){return Module["asm"]["stackSave"].apply(null,arguments)});Module["asm"]=asm;Module["ccall"]=ccall;Module["cwrap"]=cwrap;function ExitStatus(status){this.name="ExitStatus";this.message="Program terminated with exit("+status+")";this.status=status}ExitStatus.prototype=new Error;ExitStatus.prototype.constructor=ExitStatus;var initialStackTop;var calledMain=false;dependenciesFulfilled=function runCaller(){if(!Module["calledRun"])run();if(!Module["calledRun"])dependenciesFulfilled=runCaller};Module["callMain"]=function callMain(args){args=args||[];ensureInitRuntime();var argc=args.length+1;var argv=stackAlloc((argc+1)*4);HEAP32[argv>>2]=allocateUTF8OnStack(Module["thisProgram"]);for(var i=1;i<argc;i++){HEAP32[(argv>>2)+i]=allocateUTF8OnStack(args[i-1])}HEAP32[(argv>>2)+argc]=0;try{var ret=Module["_main"](argc,argv,0);exit(ret,true)}catch(e){if(e instanceof ExitStatus){return}else if(e=="SimulateInfiniteLoop"){Module["noExitRuntime"]=true;return}else{var toLog=e;if(e&&typeof e==="object"&&e.stack){toLog=[e,e.stack]}err("exception thrown: "+toLog);Module["quit"](1,e)}}finally{calledMain=true}};function run(args){args=args||Module["arguments"];if(runDependencies>0){return}preRun();if(runDependencies>0)return;if(Module["calledRun"])return;function doRun(){if(Module["calledRun"])return;Module["calledRun"]=true;if(ABORT)return;ensureInitRuntime();preMain();if(Module["onRuntimeInitialized"])Module["onRuntimeInitialized"]();if(Module["_main"]&&shouldRunNow)Module["callMain"](args);postRun()}if(Module["setStatus"]){Module["setStatus"]("Running...");setTimeout((function(){setTimeout((function(){Module["setStatus"]("")}),1);doRun()}),1)}else{doRun()}}Module["run"]=run;function exit(status,implicit){if(implicit&&Module["noExitRuntime"]&&status===0){return}if(Module["noExitRuntime"]){}else{ABORT=true;EXITSTATUS=status;STACKTOP=initialStackTop;exitRuntime();if(Module["onExit"])Module["onExit"](status)}Module["quit"](status,new ExitStatus(status))}function abort(what){if(Module["onAbort"]){Module["onAbort"](what)}if(what!==undefined){out(what);err(what);what=JSON.stringify(what)}else{what=""}ABORT=true;EXITSTATUS=1;throw"abort("+what+"). Build with -s ASSERTIONS=1 for more info."}Module["abort"]=abort;if(Module["preInit"]){if(typeof Module["preInit"]=="function")Module["preInit"]=[Module["preInit"]];while(Module["preInit"].length>0){Module["preInit"].pop()()}}var shouldRunNow=true;if(Module["noInitialRun"]){shouldRunNow=false}Module["noExitRuntime"]=true;run()

  return EmscriptenModule;
}
)();

|]


wrapperFnName :: String
wrapperFnName =
  "wrapWasmElmApp"


wrapper :: B.Builder
wrapper = [r|

function wrapWasmElmApp(wasmBuffer, wasmExports, generatedAppTypes, kernelFunctions) {
    if (!(wasmBuffer instanceof ArrayBuffer))
        throw new Error('Expected wasmMemory to be an ArrayBuffer');
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
        fieldGroups: generatedAppTypes.fieldGroupNames.reduce(function (enumObj, name) {
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
                for (var i = index + 2; i < index + size; i++) {
                    var field = fieldNames[i];
                    var childAddr = mem32[i];
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
    function HeapOverflowError(message) {
        this.name = HeapOverflowError.name;
        this.message = message || '';
    }
    HeapOverflowError.prototype = Error.prototype;
    function write32(index, value) {
        if (index > maxWriteIndex32)
            throw new HeapOverflowError('Wasm heap overflow');
        mem32[index] = value;
    }
    function write16(index, value) {
        if (index > maxWriteIndex16)
            throw new HeapOverflowError('Wasm heap overflow');
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
                if (e.name === HeapOverflowError.name) {
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
