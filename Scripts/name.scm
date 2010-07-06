(define (getname) (read))

(begin
  (write "Whats your name?")
  (define name getname)
  (write "Hello")
  (write (name)))
