const factorialR = n => (n < 3 ? n : n * factorial(n - 1));

const fac = f => n => (n < 3 ? n : n * f(n - 1));

// const rec = f => a => (x => f(y => x(x)(y)))(x => f(y => x(x)(y)))(a);

const rec = f => a => f(rec(f))(a);

const factorial = n => rec(fac)(n);

console.log(factorial(10));
