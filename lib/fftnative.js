const FFT_SIZE = 1024;

class Fftnative { }

Fftnative.twiddles = null

Fftnative.Twiddle = class {
    constructor() {
        if(!Fftnative.twiddles) {
            Fftnative.twiddles = new Array(FFT_SIZE);
            for(let span = 1; span <= FFT_SIZE/2; span<<=1) {
                let primitive_root = -Math.PI/span;
                for(let i = 0; i < span; i++) {
                    Fftnative.twiddles[span+i] = {
                        re : Math.cos(primitive_root*i),
                        im : Math.sin(primitive_root*i)
                    };
                }
            }
        }
    }
    step() { return Fftnative.twiddles; }
}

Fftnative.hanns = null

Fftnative.Hann = class {
    constructor() {
        if(!Fftnative.hanns) {
            Fftnative.hanns = new Array(FFT_SIZE);
            for(let i = 0; i < FFT_SIZE; i++) {
                Fftnative.hanns[i] = (1 - Math.cos(Math.PI*2*i/FFT_SIZE))/2;
            }
        }
    }
    step() { return Fftnative.hanns; }
}

function swap(forward, rev, data) {
    let tmp = data[forward];
    data[forward] = data[rev];
    data[rev] = tmp;
}

function bitrev(data, logN) {
    const N = 1<<logN;
    const halfn = N>>1;
    const quartn = N>>2;
    const nmin1 = N-1;

    var forward = halfn;
    var rev = 1;

    for(let i = quartn; i; i--) {
        // Gray code generator for even values:
        let nodd = ~i;                              // counting ones is easier
        for(zeros = 0; nodd&1; zeros++) nodd >>= 1; // find trailing zeros in i
        forward ^= 2 << zeros;                      // toggle one bit of forward
        rev ^= quartn >> zeros;                     // toggle one bit of rev

        // swap even and ~even conditionally
        if(forward<rev) {
            swap(forward, rev, data);
            nodd = nmin1 ^ forward;                   // compute the bitwise negations
            noddrev = nmin1 ^ rev;
            swap(nodd, noddrev, data);                // swap bitwise-negated pairs
        }

        nodd = forward ^ 1;                         // compute the odd values from the even
        noddrev = rev ^ halfn;
        swap(nodd, noddrev, data);                  // swap odd unconditionally
    }
}

Fftnative.Bitrev1024 = class {
    step(bits) {
        let o = Array.from(bits);
        bitrev(o, 10);
        return o;
    }
}

Fftnative.Float_print = class {
    step(msg,i) {
        console.log(msg, i);
    }
}

Fftnative.Float512_print = class {
    step(msg,i) {
        console.log(msg, i);
    }
}

Fftnative.Float1024_print = class {
    step(msg,i) {
        console.log(msg, i);
    }
}

Fftnative.Complex8_print = class {
    step(msg,i) {
        console.log(msg, i);
    }
}

Fftnative.Complex1024_print = class {
    step(msg,i) {
        console.log(msg, i);
    }
}

Fftnative.Int_is_odd = class {
    step(i) {
        return i&1;
    }
}
