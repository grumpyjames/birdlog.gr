var My = My || {};

My.Native = Elm.Native || {};
My.Native.Location = {};
My.Native.Location.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Location = localRuntime.Native.Location || {};
    if (localRuntime.Native.Location.values) {
        return localRuntime.Native.Location.values;
    }

    var Signal =  Elm.Signal.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);

    var location = Signal.constant({ ctor:'NoneYet' });
    var fakeNode = {
        addEventListener: function() {},
        removeEventListener: function() {}
    }

    localRuntime.addListener([location.id], fakeNode, 'moo', function () {});
    navigator.geolocation.getCurrentPosition(function(position) {
        localRuntime.notify(location.id, { ctor: "LatLn", _0: position.coords.latitude, _1: position.coords.longitude });
    }, function(err) {
        localRuntime.notify(location.id, { ctor: "Error", _0: err.code });
    });

    return localRuntime.Native.Location.values = {
        location: location
    };
}
