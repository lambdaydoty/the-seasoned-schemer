const fib = [1, 1, 2, 3, 5, 8, 13]

;(() => {
  function map (f, arr) {
    const [x, ..._arr] = arr
    return (x === undefined)
      ? []
      : [f(x), ...map(f, _arr)]
  }

  console.log(
    map(x => -x, fib)
  )
})()

;(() => {
  function map (f, arr, k) {
    const [x, ..._arr] = arr
    return (x === undefined)
      ? k([])
      : f(x, r1 => map(f, _arr, r2 => k([r1, ...r2])))
  }

  map(
    (x, f) => f(-x),
    fib,
    console.log
  )
})()

;(() => {
  const arr = fib

  function foreach (f, i, k) {
    if (i === arr.length) {
      k()
    } else {
      f(arr[i], () => foreach(f, i + 1, k))
    }
  }

  foreach(
    (x, g) => { console.log(-x); g() },
    0,
    () => console.log('### Done')
  )
})()

console.log('***')

;(() => {
  function fibonacci (n, k) {
    return (n === 0) ? k(1)
      : (n === 1) ? k(1)
        : fibonacci(n - 1, a =>
          fibonacci(n - 2, b =>
            k(a + b)
          )
        )
  }

  // console.log(fibo(5))
  fibonacci(0, console.log)
  fibonacci(1, console.log)
  fibonacci(2, console.log)
  fibonacci(3, console.log)
  fibonacci(4, console.log)
  fibonacci(5, console.log)
  console.log('')
})()

;(() => {
  function fibonacci (n, k) {
    return (n === 0) ? k(1)
      : (n === 1) ? k(1)
        : fibonacci(n - 1, a =>
          fibonacci(n - 2, b =>
            k(a + b)
          )
        )
  }

  // console.log(fibo(5))
  fibonacci(0, console.log)
  fibonacci(1, console.log)
  fibonacci(2, console.log)
  fibonacci(3, console.log)
  fibonacci(4, console.log)
  fibonacci(5, console.log)
  console.log('')
})()

// function logIter (arr) {
//   iter(console.log, () => console.log('### Done'))
//   function iter (fn, done) {
//     for (let i = 0; i < arr.length; ++i) {
//       fn(arr[i])
//     }
//     done()
//   }
// }

// function logRecur (arr) {
//   recur(0, console.log, () => console.log('### Done'))
//   function recur (ix, fn, done) {
//     if (ix < arr.length) {
//       fn(arr[ix])
//       recur(ix + 1, fn, done)
//     }
//     // done()
//   }
// }

// logIter(fib)
// logRecur(fib)

// function logCps (arr) {

//   function recur (ix, element, next) {
//     if (ix < arr.length)
//   }
// }
