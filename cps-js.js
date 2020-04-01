const p = new Promise((k, reject) => {
  setTimeout(
    result => k(result),
    1000,
    '*secret*',
  )
})

p.then(console.log)
