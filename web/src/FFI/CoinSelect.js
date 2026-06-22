export const runCoinSelectImpl = (stdinText) => () => {
  const runner = globalThis.runCoinSelect;
  if (typeof runner !== "function") {
    return Promise.reject(
      new Error("globalThis.runCoinSelect is not initialized")
    );
  }
  return runner(stdinText);
};
