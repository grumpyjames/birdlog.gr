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

    var unitConstructors = ['Pixel', 'Line', 'Page']
    var wheel = Signal.constant({ ctor:'Wheel', _0: 0.0, _1: 0.0, _2: 0.0, _3: unitConstructors[0]});
    var node = localRuntime.isFullscreen() ? document : localRuntime.node;

    localRuntime.addListener([wheel.id], node, 'wheel', function (event) {
        var unit = unitConstructors[event.deltaMode];
        if (unit) {
            localRuntime.notify(wheel.id, { ctor:'Wheel', _0: event.deltaX, _1: event.deltaY, _2: event.deltaZ, _3: unitConstructors[0]});
        }
    });

    return localRuntime.Native.Wheel.values = {
        wheel: wheel
    };
}
