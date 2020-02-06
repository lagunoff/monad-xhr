// @flow

/*::

declare function h$wrapBuffer(buf: ArrayBuffer): any;

type Method =
  | { GET: true } | { POST: true } | { PUT: true } | { DELETE: true } | { PATCH: true }

type ByteString = any;

type Request = {
   xrqUrl: string;
   xrqMethod: Method;
   xrqBody: RequestBody;
   xrqHeaders: Array<[string, string]>;
   xrqWithCredentials: boolean;
   xrqTimeout: number;
   xrqResponseType: ResponseType;
}

type RequestBody =
  | { XRQText: string }
  | { XRQByteString: any }
  | { XRQFile: any }

type Response = {
   xrsUrl: string;
   xrsStatus: number;
   xrsStatusText: string;
   xrsHeaders: Array<[string, string]>;
   xrsBody: ResponseBody;
};

type ResponseType =
  | { XRTText: true } | { XRTArrayBuffer: true }

type ResponseBody =
  | { XRSEmpty: true }
  | { XRSText: string }
  | { XRSByteString: any }

type HttpError =
   | { BadUrl: string }
   | { BadPayload: string }
   | { BadStatus: [number, string] }
   | { Timeout: true }
   | { NetworkError: string }
*/

var Control_Monad_Xhr_Internal = function() {
  function sendXhr(
    req       /*: Request */,
    onSuccess /*: (resp: Response) => void */,
    onFailure /*: (err: HttpError) => void */,
  ) {
    var xhr = new XMLHttpRequest();
    var method;
    if ('GET' in req.xrqMethod) { method = 'GET'; }
    else if ('POST' in req.xrqMethod) { method = 'POST'; }
    else if ('PATCH' in req.xrqMethod) { method = 'PATCH'; }
    else if ('PUT' in req.xrqMethod) { method = 'PUT'; }
    else if ('DELETE' in req.xrqMethod) { method = 'DELETE'; }
    xhr.addEventListener('error', handleError);
    xhr.addEventListener('timeout', onFailure.bind(void 0, { Timeout: true }));
    xhr.addEventListener('load', handleLoad);
    try {
      xhr.open(method, req.xrqUrl, true);
    } catch (e) {
      onFailure({ BadUrl: req.xrqUrl });
    }
    if (req.xrqTimeout) xhr.timeout = req.xrqTimeout;
    if (typeof (req.xrqWithCredentials) !== 'undefined') xhr.withCredentials = req.xrqWithCredentials;
    for (var i = 0; i < req.xrqHeaders.length; i++) {
      var name = req.xrqHeaders[i][0], value = req.xrqHeaders[i][1];
      xhr.setRequestHeader(name, value);
    }

    var body = undefined;
    if ('XRQText' in req.xrqBody) {
      // $FlowFixMe
      body = req.xrqBody.XRQText;
    } else if ('XRQByteString' in req.xrqBody) {
      // $FlowFixMe
      body = req.xrqBody.XRQByteString;
    } else if ('XRQFile' in req.xrqBody) {
      // $FlowFixMe
      body = req.xrqBody.XRQFile;
    }
    xhr.send(body);
    if ('XRTArrayBuffer' in req.xrqResponseType) {
      xhr.responseType = 'arraybuffer';
    }
    return xhr.abort.bind(xhr);

    function handleLoad() {
      var xrsBody /*: ?ResponseBody */ = undefined;
      var responseBody = xhr.response || xhr.responseText;
      if (responseBody instanceof ArrayBuffer) {
        xrsBody = { XRSByteString: responseBody };
      } else if (typeof responseBody === 'string') {
        xrsBody = { XRSText: responseBody };
      } else {
        onFailure({ BadPayload: 'Unsupported XHR response body' });
        return;
      }
      var resp /*: Response*/ = {
        xrsUrl: xhr.responseURL,
        xrsStatus: xhr.status,
        xrsStatusText: xhr.statusText,
        xrsHeaders: [], // parseHeaders(xhr.getAllResponseHeaders()),
        xrsBody: xrsBody || { XRSEmpty: true }
      };
      onSuccess(resp);
    }

    function handleError(e /*: any */) {
      onFailure({ NetworkError: e.message || e.constructor.name });
    }
  }
  return { sendXhr: sendXhr };
} ();
