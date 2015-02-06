var My = My || {};

My.Native = Elm.Native || {};
My.Native.Wheel = {};
My.Native.Wheel.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Wheel = localRuntime.Native.Wheel || {};
    if (localRuntime.Native.Wheel.values) {
        return localRuntime.Native.Wheel.values;
    }

    var Signal =  Elm.Signal.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);

    function pixelComp(size) {
        return {ctor: 'Component', _0: size, _1: {ctor: 'Pixel'}};
    }

    var zeroC = pixelComp(0.0);
    var wheel = Signal.constant({ ctor:'Wheel', _0: zeroC, _1: zeroC, _2: zeroC});
    var node = localRuntime.isFullscreen() ? document : localRuntime.node;

    localRuntime.addListener([wheel.id], node, 'wheel', function (wheelEvent) {
        localRuntime.notify(wheel.id, { ctor:'Wheel', _0: pixelComp(3.0), _1: zeroC, _2: zeroC});
    });

    return localRuntime.Native.Wheel.values = {
        wheel: wheel
    };
}
