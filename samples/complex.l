let token = 1;

let test: (int float unit int -> [char; 4]);

let list = array : [char; 8] {
	1, 2, 3, 4
};

let a = 3;

let b: int = 4;

let A: (int int -> int);

let C = lambda a b : int int -> int {
	return 0;
};

let foo: unit;

foo;
foo::foo;
for name in (0 to 5) {
	1 + 2;
};

let what: (unit -> int);
let how:  (unit -> int);

for azazel in (what! to how!) {
	1;
} yielding (1-1);

//let cond: bool = true;
let cond = 1;

while (cond) {};
while (cond) { yield 3; };
while (cond) { 1 + 2; } yielding (3);

let res = 1 + if (cond) {
	yield 1;
} else if (cond) {
	yield 2;
} else if (cond) {
	yield 3;
} else {
	yield 4;
}, ros = 2 + if (cond) {
	yield 3;
} else if (cond) {
	yield 8;
};

let funcall:  (int int (int int int -> int) int -> int);
let morecall: (int int int -> int);

1 + 2, 3 + 4;
let variable = 1;
let othervar = funcall 1 2 (morecall 3 4 5) 6;

7 : float;
! : (int unit -> [char; 4]);

let lastone = funcall 1 2 3;

const saab = 1, suub = 2;
let tust = 2, tast = tust = 3;

lambda _ _ v : unit unit int -> unit {
	return!;
};

//namespace u {
	let assert: (int -> unit);
//};

//namespace io {
	let get_int: (unit -> int);
	let put_int: (int -> unit);
//};

//namespace space {

	let mult = lambda x y : int int -> int {
		
		let r = 0;

		let negative = y < 0;

		let handleSign = lambda n v : int int -> int { 
			return if (n) { yield -n; }
			       else   { yield +n; };
		};

		let c = handleSign negative y;

		return handleSign negative while (c != 0) { r += x, c--; } yielding (r);
	};
	
//};

let main = lambda _ : unit -> unit {

	let repeat2 = lambda act times valA valB : (int int -> int) int int int -> int {

		u::assert (times >= 0);

		while (times != 0) {
			valA  = act valA valB;
			times = times - 1;
		};

		return valA;

	};

	let pow = lambda pw base : int int -> int {
		return repeat2 mult pw 1 base;
	};

	let read_int: (unit -> int);
	let out: (int -> unit);

	let res = if (io::put_int! == 0) {
		yield (pow 2 6);
	} else {
		yield pow 2 10;
	};

	let A = 2, X = -74, B: int;

	X = B = A = res;

	io::get_int X;
	io::get_int B;
	io::get_int A;
	io::get_int res;

	let numer = 1 + (2 * 3);
};
