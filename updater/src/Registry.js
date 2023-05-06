import gunzip from "gunzip-maybe";
import https from "https";
import tar from "tar-stream";

var metadataPath = /\/metadata\/([^\.\/]+)\.json$/;

export function fetchPackageEntries(onError, onSuccess) {
  var canceled = false;

  var extract = tar.extract();

  var packages = {};

  extract.on("error", function(error) {
    extract.destroy();
    onError(error);
  });

  extract.on("entry", function(header, stream, next) {
    if (canceled || !header || header.type !== "file" || !header.name || !metadataPath.test(header.name)) {
      stream.on("end", next);
      stream.resume();
      return;
    }

    var packageName = header.name.match(metadataPath)[1];
    var metadata = "";

    stream.on("data", function(chunk) {
      metadata += chunk;
    });

    stream.on("end", () => {
      packages[packageName] = metadata;
      next();
    });

    return stream.resume();
  });

  extract.on("finish", function() {
    if (!canceled) {
      onSuccess(packages);
    }
  });

  let req;

  function updateReq(_req) {
    if (req) {
      req.destroy();
    }
    req = _req;
    req.on("error", function(error) {
      onError(error);
    });
  }

  updateReq(https.get("https://github.com/purescript/registry/tarball/main", function handleResponse(response) {
    if (canceled) {
      return;
    }
    switch (response.statusCode) {
      case 200:
        try {
          return response.pipe(gunzip()).pipe(extract);
        }
        catch (error) {
          return onError(error);
        }
      case 302:
        return updateReq(https.get(response.headers.location, handleResponse));
      default:
        return onError(new Error(`Unexpected status code while fetching registry tarball: ${response.statusCode} ${response.statusMessage}`));
    }
  }));

  return function(cancelError, onCancelerError, onCancelerSuccess) {
    try {
      req && req.destroy();
      extract.destroy();
    }
    catch (error) {
      onCancelerError(error);
    }
    canceled = true;
    onCancelerSuccess();
  };
}