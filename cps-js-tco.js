const assert = require('assert')

let log = () => {}

/*
 * Ref: https://www.eriwen.com/javascript/cps-tail-call-elimination/
 */

function inspect (fn, tag, args) {
  const toString = x => typeof x === 'function'
    ? 'func'
    : x.toString()
  const output = `${fn.name || 'unkno'}`.padEnd(5, ' ')
    + `.${tag}`.padEnd(5, ' ')
    + `: ${args.map(toString).join(', ')}`
  return output
}

Function.prototype.tail = function (...args) { // eslint-disable-line no-extend-native
  log(this, 'tail', args)
  return { fn: this, args }
}

Function.prototype.tco = function (...args) { // eslint-disable-line no-extend-native
  let conti = { fn: this, args }
  const escapeFn = args[args.length - 1]
  while (conti.fn !== escapeFn) {
    log(this, 'tco', conti.args)
    conti = conti.fn.apply(this, conti.args) // we will obtain a proper { fn, agrs } if fn has been properly transformed into tail form.
  }
  log(this, 'esc', args)
  return escapeFn.apply(this, conti.args)
}

const id = function (x) { return x }

const fact1 = function (n) {
  return n === 0
    ? '.'
    : 'x' + fact1(n - 1)
}

const fact2 = function (n, k) {
  return n === 0
    ? k('.')
    : fact2(n - 1, v => k('x' + v))
}

const fact3 = function (n, k) {
  return n === 0
    ? k.tail('.')
    : fact3.tail(n - 1, function (v) {
      return k.tail('x' + v)
    })
}

const overflow = /Maximum call stack size exceeded/
const N = 100000
assert.throws(() => fact1(N), overflow)
assert.throws(() => fact2(N, id), overflow)
assert.strictEqual(fact3.tco(N, id), '.'.padStart(100001, 'x'))

log = (...args) => console.log(inspect(...args))
fact3.tco(3, console.log)

const fibK = function (n, k) {
  return n === 1
    ? k.tail(0, 1)
    : fibK.tail(n - 1, function (a, b) {
        return k.tail(b, a + b)
      })
}

fibK.tco(30, console.log)

console.log('...')

// Generator

function* G (n) {
  for (let i = 1; i < n; ++i) {
    let y = null
    fibK.tco(i, x => y = x)
    yield y
  }
}
const fibonacci = [...G(10)]

console.log(fibonacci)

//

const map = function (f, arr) {
  return cps(arr)
  function cps ([x, ..._arr]) {
    return (x === undefined)
      ? []
      : [f(x), ...cps(_arr)]
  }
}

console.log(map(x => -x, fibonacci))
