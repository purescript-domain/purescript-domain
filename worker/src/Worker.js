export function fetchUpstreamImpl(url) {
  return function() {
    return fetch(url, {
      cf: {
        cacheEverything: true,
      },
    });
  };
}