var My = My || {};

My.Native = Elm.Native || {};
My.Native.Arrays = {};
My.Native.Arrays.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Arrays = localRuntime.Native.Arrays || {};
    if (localRuntime.Native.Arrays.values) {
        return localRuntime.Native.Arrays.values;
    }

    var Arr = Elm.Native.Array.make(localRuntime);
    
    var cartesian = function(f, xs, ys) {
        var yLen = Arr.length(ys);
        var xLen = Arr.length(xs);
        var result = new Array(yLen);
        for (var i = 0; i < yLen; ++i) {
            current = new Array(xLen);
            for (var j = 0; j < xLen; ++j) {
                current[j] = f(Arr.get(j)(xs))(Arr.get(i)(ys));
            }
            result[i] = Arr.fromJSArray(current);
        }
        return Arr.fromJSArray(result);
    }
    
    return localRuntime.Native.Arrays.values = {
        cartesian: F3(cartesian)
    };
}
