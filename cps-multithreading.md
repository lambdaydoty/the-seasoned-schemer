# https://mermaidjs.github.io/mermaid-live-editor/

``` mermaid

sequenceDiagram
    main ->> thread1: (thread-new thread1)
    Note left of thread1: "Start from 1"
    thread1 -->> main: (thread-yield)
    main ->> thread2: (thread-new thread2)
    Note left of thread2: "Start from 2"
    thread2 -->> thread1: (thread-yield)
    Note left of thread1: "End of 1"
    thread1 -->> main: (thread-end)
    main ->> thread3: (thread-new thread3)
    Note left of thread3: "Start from 3"
    thread3 -->> thread2: (thread-yield)
    Note left of thread2: "In the midway of 2"
    thread2 -->> main: (thread-end)
    Note right of main: "End of main"
    main ->> thread3: (thread-end)
    Note left of thread3: "End of 3"
    thread3 -->> thread2: (thread-end)
    Note left of thread2: "End of 2"
    thread2 ->> thread2: (exit)
    
 ```
