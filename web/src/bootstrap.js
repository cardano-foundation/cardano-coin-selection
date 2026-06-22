import {
  WASI,
  File,
  OpenFile,
  ConsoleStdout,
} from "@bjorn3/browser_wasi_shim";
import wasmBytes from "./assets/coin-select.wasm";

const encoder = new TextEncoder();

globalThis.runCoinSelect = async (stdinText) => {
  const stdin = new OpenFile(new File(encoder.encode(stdinText)));
  const stdoutLines = [];
  const stderrLines = [];
  const stdout = ConsoleStdout.lineBuffered((line) => {
    stdoutLines.push(line);
  });
  const stderr = ConsoleStdout.lineBuffered((line) => {
    stderrLines.push(line);
  });
  const wasi = new WASI([], [], [stdin, stdout, stderr]);
  const wasm = await WebAssembly.compile(wasmBytes);
  const instance = await WebAssembly.instantiate(wasm, {
    wasi_snapshot_preview1: wasi.wasiImport,
  });

  wasi.start(instance);

  if (stderrLines.length > 0) {
    console.error(stderrLines.join("\n"));
  }

  return stdoutLines.join("\n");
};
