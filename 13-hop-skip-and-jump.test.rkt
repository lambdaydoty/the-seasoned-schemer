#lang racket
(require "utils.rkt")
(require rackunit "13-hop-skip-and-jump.rkt")

(check-equal? (intersectall '((3 mangos and)
                              (3 kiwis and)
                              (3 hamburgers)))
              '(3))
(check-equal? (intersectall '((3 steaks and)
                              (no food and)
                              (three baked potatoes)
                              (3 diet hamburgers)))
              "empty!!")
(check-equal? (intersectall '((3 mangos and)
                              ()
                              (3 diet hamburgers)))
              "empty!")
(check-equal? (intersectall '((1 2 3)
                              (2 3 4)
                              (3 4 5)
                              (4 5 6)))
              '())

(let ([lat '(noodles spaghetti spatzle bean-thread roots potatoes yam others rice)]
      [lat2 '(cookies chocolate mints caramel delight ginger snaps
                      desserts chocolate mousse vanilla ice cream
                      German chocolate cake more desserts gingerbreadman
                      chocolate chip brownies)]
      [lat3 '(cookies chocolate mints caramel delight ginger snaps
                      desserts chocolate mousse vanilla ice cream
                      German chocolate cake more cookies gingerbreadman
                      chocolate chip brownies)])
  (check-equal? (rember-beyond-first 'roots lat) '(noodles spaghetti spatzle bean-thread))
  (check-equal? (rember-beyond-first 'others lat) '(noodles spaghetti spatzle bean-thread roots potatoes yam))
  (check-equal? (rember-beyond-first 'sweetthing lat) lat)
  (check-equal? (rember-beyond-first 'desserts lat2) '(cookies chocolate mints caramel delight ginger snaps))
  (check-equal? (rember-upto-last 'roots lat) '(potatoes yam others rice))
  (check-equal? (rember-upto-last 'sweetthing lat) lat)
  (check-equal? (rember-upto-last 'cookies lat3) '(gingerbreadman chocolate chip brownies))
  )


