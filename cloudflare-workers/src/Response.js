export function mkResponseImpl(body) {
  return function(init) {
    if (typeof process !== "undefined") {
      // Node.js (test environment)
      return init;
    }
    return new Response(body, init);
  }
}

export function status(res) {
  return res.status;
}

export function statusText(res) {
  return res.statusText;
}

export function headers(res) {
  return res.headers;
}
